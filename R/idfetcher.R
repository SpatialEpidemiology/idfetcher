#' Fetch and write PMID and PMCID identifiers to Zotero
#'
#' Retrieves PMID and PMCID identifiers for Zotero journal articles using:
#'   1. NCBI PMC ID Converter for DOI-based lookups
#'   2. PubMed DOI lookup
#'   3. PubMed title + first-author lookup
#'   4. PubMed title-only lookup
#'
#' Identifiers are written only to Zotero's dedicated PMID and PMCID fields.
#'
#' @param dry_run Logical. If TRUE, lookups are performed but Zotero is not
#'   modified. Default is TRUE.
#' @param batch_size Number of DOIs sent to PMC in each batch. Default is 50.
#' @param pubmed_delay Seconds to wait between PubMed requests. Default is 1.
#' @param pmc_delay Seconds to wait between PMC batches. Default is 1.5.
#' @param max_retries Maximum number of retries after an API error or HTTP 429.
#'   Default is 5.
#' @param ncbi_api_key Optional NCBI API key. Default is an empty string.
#'
#' @return Invisibly returns a list containing the run summary.
#'
#' @export
idfetcher <- function(
    dry_run = TRUE,
    batch_size = 50,
    pubmed_delay = 1,
    pmc_delay = 1.5,
    max_retries = 5,
    ncbi_api_key = ""
) {

  # --------------------------------------------------------------------------
  # Package checks
  # --------------------------------------------------------------------------

  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      "Package 'httr2' is required. ",
      "Run install.packages('httr2').",
      call. = FALSE
    )
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Package 'jsonlite' is required. ",
      "Run install.packages('jsonlite').",
      call. = FALSE
    )
  }

  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop(
      "Package 'xml2' is required. ",
      "Run install.packages('xml2').",
      call. = FALSE
    )
  }

  # --------------------------------------------------------------------------
  # Credentials
  # --------------------------------------------------------------------------

  credentials <- idfetcher_get_credentials()

  user_id <- credentials$user_id
  api_key <- credentials$api_key

  # --------------------------------------------------------------------------
  # URLs
  # --------------------------------------------------------------------------

  zotero_base <- "https://api.zotero.org"

  pmc_url <- paste0(
    "https://pmc.ncbi.nlm.nih.gov/",
    "tools/idconv/api/v1/articles/"
  )

  pubmed_esearch_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/",
    "entrez/eutils/esearch.fcgi"
  )

  pubmed_efetch_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/",
    "entrez/eutils/efetch.fcgi"
  )

  user_agent <- "IDFetcher/1.0"

  # --------------------------------------------------------------------------
  # Helpers
  # --------------------------------------------------------------------------

  `%||%` <- function(x, y) {
    if (is.null(x)) {
      return(y)
    }

    x
  }

  get_field <- function(item, field) {

    value <- item$data[[field]]

    if (
      is.null(value) ||
      length(value) == 0 ||
      is.na(value[1])
    ) {
      return("")
    }

    value <- as.character(value)[1]

    if (is.na(value)) {
      return("")
    }

    trimws(value)
  }

  normalize_doi <- function(doi) {

    if (is.null(doi) || length(doi) == 0) {
      return("")
    }

    doi <- as.character(doi)[1]

    if (is.na(doi)) {
      return("")
    }

    doi <- trimws(tolower(doi))

    prefixes <- c(
      "https://doi.org/",
      "http://doi.org/",
      "https://dx.doi.org/",
      "http://dx.doi.org/",
      "doi:"
    )

    for (prefix in prefixes) {

      if (startsWith(doi, prefix)) {

        doi <- substring(
          doi,
          nchar(prefix) + 1
        )

        break
      }
    }

    trimws(doi)
  }

  normalize_title <- function(title) {

    if (
      is.null(title) ||
      length(title) == 0 ||
      is.na(title[1])
    ) {
      return("")
    }

    title <- as.character(title)[1]

    # Normalize common Unicode punctuation before comparison.
    title <- gsub(
      "\u2018|\u2019|\u201A|\u201B",
      "'",
      title
    )

    title <- gsub(
      "\u201C|\u201D|\u201E",
      "\"",
      title
    )

    title <- gsub(
      "\u2013|\u2014",
      "-",
      title
    )

    title <- tolower(title)

    title <- gsub(
      "&",
      " and ",
      title,
      fixed = TRUE
    )

    title <- gsub(
      "[^a-z0-9]+",
      " ",
      title
    )

    title <- gsub(
      "\\s+",
      " ",
      title
    )

    trimws(title)
  }

  title_similarity <- function(x, y) {

    x <- normalize_title(x)
    y <- normalize_title(y)

    if (!nzchar(x) || !nzchar(y)) {
      return(0)
    }

    if (identical(x, y)) {
      return(1)
    }

    distance <- adist(x, y)[1, 1]

    max_length <- max(
      nchar(x),
      nchar(y)
    )

    if (max_length == 0) {
      return(0)
    }

    max(
      0,
      1 - (distance / max_length)
    )
  }

  normalize_author <- function(author) {

    if (
      is.null(author) ||
      length(author) == 0 ||
      is.na(author[1])
    ) {
      return("")
    }

    author <- as.character(author)[1]

    author <- tolower(author)

    author <- gsub(
      "[^a-z0-9]+",
      "",
      author
    )

    trimws(author)
  }

  # --------------------------------------------------------------------------
  # Extract first author from Zotero
  # --------------------------------------------------------------------------

  get_first_author <- function(item) {

    creators <- item$data$creators

    if (
      is.null(creators) ||
      length(creators) == 0
    ) {
      return("")
    }

    first_creator <- creators[[1]]

    if (
      !is.null(first_creator$lastName) &&
      nzchar(trimws(as.character(first_creator$lastName)))
    ) {
      return(
        trimws(
          as.character(
            first_creator$lastName
          )
        )
      )
    }

    # Some Zotero creator records use a single "name"
    # for collective/corporate authors.
    if (
      !is.null(first_creator$name) &&
      nzchar(trimws(as.character(first_creator$name)))
    ) {
      return(
        trimws(
          as.character(
            first_creator$name
          )
        )
      )
    }

    ""
  }

  # --------------------------------------------------------------------------
  # Generic NCBI request with retry / 429 handling
  #
  # IMPORTANT:
  # Do not use !!!params here. That was producing:
  #
  # All elements of `...` must be either an atomic vector or NULL.
  #
  # do.call() safely passes the named query parameters to httr2.
  # --------------------------------------------------------------------------

  ncbi_request <- function(
    url,
    params,
    parse = c("json", "text"),
    request_name = "NCBI request"
  ) {

    parse <- match.arg(parse)

    # Make sure query values are simple atomic values.
    params <- lapply(
      params,
      function(x) {
        if (is.null(x)) {
          return(NULL)
        }

        if (length(x) == 0) {
          return(NULL)
        }

        if (length(x) > 1) {
          paste(
            as.character(x),
            collapse = ","
          )
        } else {
          as.character(x)
        }
      }
    )

    params <- params[
      !vapply(
        params,
        is.null,
        logical(1)
      )
    ]

    for (attempt in seq_len(max_retries)) {

      result <- tryCatch({

        req <- httr2::request(url)

        req <- httr2::req_headers(
          req,
          `User-Agent` = user_agent
        )

        req <- do.call(
          httr2::req_url_query,
          c(
            list(req),
            params
          )
        )

        response <- httr2::req_perform(req)

        status <- httr2::resp_status(response)

        if (status == 429) {

          wait_time <- max(
            2^(attempt - 1),
            1
          )

          message(
            request_name,
            " error: HTTP 429 Too Many Requests."
          )

          message(
            "Retrying in ",
            wait_time,
            " seconds..."
          )

          Sys.sleep(wait_time)

          return(NULL)
        }

        if (status >= 400) {

          stop(
            "HTTP ",
            status,
            " ",
            httr2::resp_status_desc(response)
          )
        }

        if (parse == "json") {

          return(
            jsonlite::fromJSON(
              httr2::resp_body_string(response),
              simplifyVector = FALSE
            )
          )
        }

        httr2::resp_body_string(response)

      }, error = function(e) {

        if (attempt < max_retries) {

          wait_time <- max(
            2^(attempt - 1),
            1
          )

          message(
            request_name,
            " error: ",
            conditionMessage(e)
          )

          message(
            "Retrying in ",
            wait_time,
            " seconds..."
          )

          Sys.sleep(wait_time)

          return(NULL)
        }

        message(
          request_name,
          " failed: ",
          conditionMessage(e)
        )

        NULL
      })

      if (!is.null(result)) {
        return(result)
      }
    }

    NULL
  }

  # --------------------------------------------------------------------------
  # PubMed PMID lookup by DOI
  # --------------------------------------------------------------------------

  get_pubmed_id_by_doi <- function(doi) {

    if (!nzchar(doi)) {
      return(NA_character_)
    }

    params <- list(
      db = "pubmed",
      term = paste0(
        "\"",
        doi,
        "\"[DOI]"
      ),
      retmode = "json",
      retmax = "10"
    )

    if (
      !is.null(ncbi_api_key) &&
      nzchar(ncbi_api_key)
    ) {
      params$api_key <- ncbi_api_key
    }

    json <- ncbi_request(
      pubmed_esearch_url,
      params,
      parse = "json",
      request_name = "PubMed DOI lookup"
    )

    if (is.null(json)) {
      return(NA_character_)
    }

    ids <- json$esearchresult$idlist

    if (
      is.null(ids) ||
      length(ids) == 0
    ) {
      return(NA_character_)
    }

    ids <- as.character(
      unlist(ids)
    )

    if (length(ids) == 0) {
      return(NA_character_)
    }

    ids[1]
  }

  # --------------------------------------------------------------------------
  # PMC DOI batch lookup
  # --------------------------------------------------------------------------

  get_pmc_records <- function(dois) {

    if (length(dois) == 0) {
      return(list())
    }

    dois <- unique(
      vapply(
        dois,
        normalize_doi,
        character(1)
      )
    )

    dois <- dois[
      nzchar(dois)
    ]

    if (length(dois) == 0) {
      return(list())
    }

    params <- list(
      ids = paste(dois, collapse = ","),
      idtype = "doi",
      format = "json",
      tool = "idfetcher"
    )

    if (
      !is.null(ncbi_api_key) &&
      nzchar(ncbi_api_key)
    ) {
      params$api_key <- ncbi_api_key
    }

    json <- ncbi_request(
      pmc_url,
      params,
      parse = "json",
      request_name = "PMC lookup"
    )

    if (is.null(json)) {
      return(list())
    }

    records <- json$records

    if (is.null(records)) {
      return(list())
    }

    # Normalize a single record into a list.
    if (
      is.list(records) &&
      !is.null(records$doi)
    ) {
      records <- list(records)
    }

    result <- list()

    for (record in records) {

      if (!is.list(record)) {
        next
      }

      record_doi <- normalize_doi(
        record$doi %||%
          record$requested_id %||%
          record[["requested-id"]] %||%
          ""
      )

      if (!nzchar(record_doi)) {
        next
      }

      result[[record_doi]] <- record
    }

    result
  }

  # --------------------------------------------------------------------------
  # Fetch PubMed records as XML
  # --------------------------------------------------------------------------

  fetch_pubmed_records <- function(pmids) {

    if (length(pmids) == 0) {
      return(list())
    }

    pmids <- unique(
      as.character(pmids)
    )

    pmids <- pmids[
      nzchar(pmids)
    ]

    if (length(pmids) == 0) {
      return(list())
    }

    params <- list(
      db = "pubmed",
      id = paste(pmids, collapse = ","),
      rettype = "xml",
      retmode = "xml"
    )

    if (
      !is.null(ncbi_api_key) &&
      nzchar(ncbi_api_key)
    ) {
      params$api_key <- ncbi_api_key
    }

    xml_text <- ncbi_request(
      pubmed_efetch_url,
      params,
      parse = "text",
      request_name = "PubMed record fetch"
    )

    if (
      is.null(xml_text) ||
      !nzchar(xml_text)
    ) {
      return(list())
    }

    doc <- tryCatch(
      xml2::read_xml(xml_text),
      error = function(e) {

        message(
          "Could not parse PubMed XML: ",
          conditionMessage(e)
        )

        NULL
      }
    )

    if (is.null(doc)) {
      return(list())
    }

    articles <- xml2::xml_find_all(
      doc,
      ".//PubmedArticle"
    )

    if (length(articles) == 0) {
      return(list())
    }

    results <- vector(
      "list",
      length(articles)
    )

    counter <- 0

    for (article in articles) {

      counter <- counter + 1

      # --------------------------------------------------------------
      # PMID
      # --------------------------------------------------------------

      pmid_node <- xml2::xml_find_first(
        article,
        ".//PMID"
      )

      pmid <- ""

      if (
        !inherits(
          pmid_node,
          "xml_missing"
        )
      ) {
        pmid <- trimws(
          xml2::xml_text(pmid_node)
        )
      }

      # --------------------------------------------------------------
      # Title
      # --------------------------------------------------------------

      title_node <- xml2::xml_find_first(
        article,
        ".//ArticleTitle"
      )

      title <- ""

      if (
        !inherits(
          title_node,
          "xml_missing"
        )
      ) {
        title <- trimws(
          xml2::xml_text(
            title_node
          )
        )
      }

      # --------------------------------------------------------------
      # Publication date
      # --------------------------------------------------------------

      date_node <- xml2::xml_find_first(
        article,
        ".//PubDate"
      )

      pub_date <- ""

      if (
        !inherits(
          date_node,
          "xml_missing"
        )
      ) {

        year_node <- xml2::xml_find_first(
          date_node,
          "./Year"
        )

        month_node <- xml2::xml_find_first(
          date_node,
          "./Month"
        )

        day_node <- xml2::xml_find_first(
          date_node,
          "./Day"
        )

        year <- ""
        month <- ""
        day <- ""

        if (
          !inherits(
            year_node,
            "xml_missing"
          )
        ) {
          year <- xml2::xml_text(year_node)
        }

        if (
          !inherits(
            month_node,
            "xml_missing"
          )
        ) {
          month <- xml2::xml_text(month_node)
        }

        if (
          !inherits(
            day_node,
            "xml_missing"
          )
        ) {
          day <- xml2::xml_text(day_node)
        }

        date_parts <- c(
          year,
          month,
          day
        )

        date_parts <- date_parts[
          nzchar(date_parts)
        ]

        pub_date <- paste(
          date_parts,
          collapse = "-"
        )
      }

      # --------------------------------------------------------------
      # Article IDs
      # --------------------------------------------------------------

      article_ids <- list()

      id_nodes <- xml2::xml_find_all(
        article,
        ".//ArticleId"
      )

      if (length(id_nodes) > 0) {

        for (node in id_nodes) {

          id_type <- xml2::xml_attr(
            node,
            "IdType"
          )

          id_value <- trimws(
            xml2::xml_text(node)
          )

          if (
            !is.na(id_type) &&
            nzchar(id_value)
          ) {

            article_ids[
              [tolower(id_type)]
            ] <- id_value
          }
        }
      }

      # --------------------------------------------------------------
      # First author
      # --------------------------------------------------------------

      first_author_node <- xml2::xml_find_first(
        article,
        ".//AuthorList/Author[1]"
      )

      first_author <- ""

      if (
        !inherits(
          first_author_node,
          "xml_missing"
        )
      ) {

        last_name_node <- xml2::xml_find_first(
          first_author_node,
          "./LastName"
        )

        collective_name_node <- xml2::xml_find_first(
          first_author_node,
          "./CollectiveName"
        )

        if (
          !inherits(
            last_name_node,
            "xml_missing"
          )
        ) {

          first_author <- trimws(
            xml2::xml_text(
              last_name_node
            )
          )

        } else if (
          !inherits(
            collective_name_node,
            "xml_missing"
          )
        ) {

          first_author <- trimws(
            xml2::xml_text(
              collective_name_node
            )
          )
        }
      }

      results[[counter]] <- list(
        pmid = pmid,
        title = title,
        pub_date = pub_date,
        first_author = first_author,
        doi = article_ids$doi %||% "",
        pmcid = article_ids$pmc %||% "",
        article_ids = article_ids
      )
    }

    results[
      vapply(
        results,
        function(x) {
          !is.null(x$pmid) &&
            nzchar(x$pmid)
        },
        logical(1)
      )
    ]
  }

  # --------------------------------------------------------------------------
  # PubMed query helper
  # --------------------------------------------------------------------------

  run_pubmed_search <- function(
    term,
    request_name,
    retmax = 50
  ) {

    params <- list(
      db = "pubmed",
      term = term,
      retmode = "json",
      retmax = as.character(retmax)
    )

    if (
      !is.null(ncbi_api_key) &&
      nzchar(ncbi_api_key)
    ) {
      params$api_key <- ncbi_api_key
    }

    json <- ncbi_request(
      pubmed_esearch_url,
      params,
      parse = "json",
      request_name = request_name
    )

    if (is.null(json)) {
      return(character(0))
    }

    ids <- json$esearchresult$idlist

    if (
      is.null(ids) ||
      length(ids) == 0
    ) {
      return(character(0))
    }

    as.character(
      unlist(ids)
    )
  }

  # --------------------------------------------------------------------------
  # Choose best PubMed match
  # --------------------------------------------------------------------------

  choose_best_pubmed_match <- function(
    zotero_title,
    zotero_author,
    records
  ) {

    if (length(records) == 0) {
      return(NULL)
    }

    title_scores <- vapply(
      records,
      function(record) {

        title_similarity(
          zotero_title,
          record$title
        )
      },
      numeric(1)
    )

    author_target <- normalize_author(
      zotero_author
    )

    author_matches <- vapply(
      records,
      function(record) {

        if (!nzchar(author_target)) {
          return(FALSE)
        }

        normalize_author(
          record$first_author
        ) == author_target
      },
      logical(1)
    )

    # Prefer an exact/very strong title match with the correct first author.
    combined_score <- title_scores

    if (nzchar(author_target)) {

      combined_score <- title_scores

      combined_score[
        author_matches
      ] <- combined_score[
        author_matches
      ] + 0.05
    }

    best_index <- which.max(
      combined_score
    )

    best_record <- records[
      [best_index]
    ]

    best_title_score <- title_scores[
      best_index
    ]

    best_author_match <- author_matches[
      best_index
    ]

    # Exact normalized title.
    if (best_title_score >= 0.999) {

      best_record$match_type <- if (
        best_author_match
      ) {
        "exact_title_author"
      } else {
        "exact_title"
      }

      best_record$match_score <- best_title_score
      best_record$author_match <- best_author_match

      return(best_record)
    }

    # Very strong title match.
    if (best_title_score >= 0.94) {

      best_record$match_type <- if (
        best_author_match
      ) {
        "strong_title_author"
      } else {
        "strong_title"
      }

      best_record$match_score <- best_title_score
      best_record$author_match <- best_author_match

      return(best_record)
    }

    # If the title is slightly less similar but the author is correct,
    # accept only a reasonably strong title match.
    if (
      best_author_match &&
      best_title_score >= 0.90
    ) {

      best_record$match_type <- "author_supported_title"
      best_record$match_score <- best_title_score
      best_record$author_match <- TRUE

      return(best_record)
    }

    NULL
  }

  # --------------------------------------------------------------------------
  # PubMed title + first-author search
  # --------------------------------------------------------------------------

  search_pubmed_by_title_author <- function(
    title,
    first_author
  ) {

    clean_title <- normalize_title(
      title
    )

    if (!nzchar(clean_title)) {
      return(NULL)
    }

    author <- trimws(
      as.character(first_author %||% "")
    )

    # --------------------------------------------------------------
    # Search 1:
    #
    # Exact title phrase + first-author surname.
    # --------------------------------------------------------------

    if (nzchar(author)) {

      exact_term <- paste0(
        "\"",
        title,
        "\"[Title] AND ",
        author,
        "[Author]"
      )

      message(
        "  PubMed search: title + first author (exact)"
      )

      ids <- run_pubmed_search(
        exact_term,
        "PubMed title + author lookup",
        retmax = 20
      )

      Sys.sleep(
        pubmed_delay
      )

      if (length(ids) > 0) {

        records <- fetch_pubmed_records(
          ids
        )

        Sys.sleep(
          pubmed_delay
        )

        match <- choose_best_pubmed_match(
          title,
          author,
          records
        )

        if (!is.null(match)) {
          return(match)
        }
      }
    }

    # --------------------------------------------------------------
    # Search 2:
    #
    # Normalized title words + author.
    #
    # This is important for punctuation, hyphens, apostrophes,
    # Unicode punctuation, subtitles, and PubMed title formatting.
    # --------------------------------------------------------------

    words <- strsplit(
      clean_title,
      "\\s+"
    )[[1]]

    words <- words[
      nchar(words) >= 3
    ]

    if (length(words) > 0) {

      # Use the most informative beginning of the title.
      words <- words[
        seq_len(
          min(
            length(words),
            15
          )
        )
      ]

      if (nzchar(author)) {

        broad_term <- paste0(
          "(",
          paste0(
            words,
            "[Title]",
            collapse = " AND "
          ),
          ") AND ",
          author,
          "[Author]"
        )

      } else {

        broad_term <- paste0(
          words,
          "[Title]",
          collapse = " AND "
        )
      }

      message(
        "  PubMed search: normalized title + first author"
      )

      ids <- run_pubmed_search(
        broad_term,
        "PubMed broad title + author lookup",
        retmax = 50
      )

      Sys.sleep(
        pubmed_delay
      )

      if (length(ids) > 0) {

        records <- fetch_pubmed_records(
          ids
        )

        Sys.sleep(
          pubmed_delay
        )

        match <- choose_best_pubmed_match(
          title,
          author,
          records
        )

        if (!is.null(match)) {
          return(match)
        }
      }
    }

    NULL
  }

  # --------------------------------------------------------------------------
  # PubMed title-only fallback
  # --------------------------------------------------------------------------

  search_pubmed_by_title <- function(
    title,
    first_author = ""
  ) {

    # First use title + author.
    match <- search_pubmed_by_title_author(
      title,
      first_author
    )

    if (!is.null(match)) {
      return(match)
    }

    # --------------------------------------------------------------
    # Final title-only search.
    # --------------------------------------------------------------

    clean_title <- normalize_title(
      title
    )

    if (!nzchar(clean_title)) {
      return(NULL)
    }

    words <- strsplit(
      clean_title,
      "\\s+"
    )[[1]]

    words <- words[
      nchar(words) >= 3
    ]

    if (length(words) == 0) {
      return(NULL)
    }

    words <- words[
      seq_len(
        min(
          length(words),
          15
        )
      )
    ]

    broad_term <- paste0(
      words,
      "[Title]",
      collapse = " AND "
    )

    message(
      "  PubMed search: normalized title only"
    )

    ids <- run_pubmed_search(
      broad_term,
      "PubMed broad title lookup",
      retmax = 50
    )

    Sys.sleep(
      pubmed_delay
    )

    if (length(ids) == 0) {
      return(NULL)
    }

    records <- fetch_pubmed_records(
      ids
    )

    Sys.sleep(
      pubmed_delay
    )

    choose_best_pubmed_match(
      title,
      first_author,
      records
    )
  }

  # --------------------------------------------------------------------------
  # Get Zotero journal articles
  # --------------------------------------------------------------------------

  get_zotero_articles <- function() {

    all_items <- list()

    start <- 0
    limit <- 100

    repeat {

      message(
        "Fetching Zotero articles ",
        start + 1,
        "-",
        start + limit,
        "..."
      )

      url <- paste0(
        zotero_base,
        "/users/",
        user_id,
        "/items"
      )

      req <- httr2::request(
        url
      )

      req <- httr2::req_headers(
        req,
        `Zotero-API-Key` = api_key,
        `User-Agent` = user_agent
      )

      req <- httr2::req_url_query(
        req,
        itemType = "journalArticle",
        limit = limit,
        start = start,
        format = "json"
      )

      response <- httr2::req_perform(
        req
      )

      status <- httr2::resp_status(
        response
      )

      if (status >= 400) {

        stop(
          "Zotero API error: HTTP ",
          status,
          " - ",
          httr2::resp_status_desc(response)
        )
      }

      items <- jsonlite::fromJSON(
        httr2::resp_body_string(response),
        simplifyVector = FALSE
      )

      if (length(items) == 0) {
        break
      }

      all_items <- c(
        all_items,
        items
      )

      message(
        "Fetched ",
        length(all_items),
        " articles..."
      )

      if (length(items) < limit) {
        break
      }

      start <- start + limit
    }

    all_items
  }

  # --------------------------------------------------------------------------
  # Update Zotero item
  # --------------------------------------------------------------------------

  update_zotero_item <- function(item) {

    item_key <- item$key

    if (
      is.null(item_key) ||
      !nzchar(item_key)
    ) {

      stop(
        "Zotero item has no key."
      )
    }

    url <- paste0(
      zotero_base,
      "/users/",
      user_id,
      "/items/",
      item_key
    )

    req <- httr2::request(
      url
    )

    req <- httr2::req_method(
      req,
      "PUT"
    )

    req <- httr2::req_headers(
      req,
      `Zotero-API-Key` = api_key,
      `User-Agent` = user_agent,
      `Content-Type` = "application/json"
    )

    version <- item$version

    if (!is.null(version)) {

      req <- httr2::req_headers(
        req,
        `If-Unmodified-Since-Version` =
          as.character(version)
      )
    }

    req <- httr2::req_body_json(
      req,
      item$data,
      auto_unbox = TRUE
    )

    response <- httr2::req_perform(
      req
    )

    status <- httr2::resp_status(
      response
    )

    if (status != 204) {

      stop(
        "Zotero update failed: HTTP ",
        status,
        " - ",
        httr2::resp_status_desc(response)
      )
    }

    TRUE
  }

  # --------------------------------------------------------------------------
  # Counters
  # --------------------------------------------------------------------------

  updated <- 0
  pmids_added <- 0
  pmcids_added <- 0
  both_added <- 0
  already_complete <- 0
  doi_lookups <- 0
  title_lookups <- 0
  pubmed_fallbacks <- 0
  not_found <- 0
  errors <- 0

  # --------------------------------------------------------------------------
  # Start
  # --------------------------------------------------------------------------

  message("")
  message(
    "======================================================================"
  )
  message(
    "IDFETCHER"
  )
  message(
    "======================================================================"
  )
  message("")

  message(
    "Connecting to Zotero..."
  )

  items <- get_zotero_articles()

  message("")
  message(
    "Total journal articles: ",
    length(items)
  )

  # --------------------------------------------------------------------------
  # Identify articles requiring lookup
  # --------------------------------------------------------------------------

  todo <- list()

  for (item in items) {

    data <- item$data

    doi <- normalize_doi(
      data$DOI %||% ""
    )

    pmid <- get_field(
      item,
      "PMID"
    )

    pmcid <- get_field(
      item,
      "PMCID"
    )

    title <- get_field(
      item,
      "title"
    )

    first_author <- get_first_author(
      item
    )

    if (
      !nzchar(pmid) ||
      !nzchar(pmcid)
    ) {

      todo[
        [length(todo) + 1]
      ] <- list(
        item = item,
        doi = doi,
        title = title,
        first_author = first_author
      )
    }
  }

  message(
    "Missing PMID or PMCID: ",
    length(todo)
  )

  if (length(todo) == 0) {

    message(
      "Nothing needs updating."
    )

    return(
      invisible(
        list(
          total_articles = length(items),
          articles_requiring_lookup = 0,
          updated = 0,
          pmids_added = 0,
          pmcids_added = 0,
          both_added = 0,
          already_complete = 0,
          doi_lookups = 0,
          title_lookups = 0,
          pubmed_fallbacks = 0,
          not_found = 0,
          errors = 0,
          dry_run = dry_run
        )
      )
    )
  }

  # --------------------------------------------------------------------------
  # Process DOI records in batches
  # --------------------------------------------------------------------------

  doi_todo <- todo[
    vapply(
      todo,
      function(x) nzchar(x$doi),
      logical(1)
    )
  ]

  if (length(doi_todo) > 0) {

    for (
      start in seq(
        1,
        length(doi_todo),
        by = batch_size
      )
    ) {

      end <- min(
        start + batch_size - 1,
        length(doi_todo)
      )

      batch <- doi_todo[
        start:end
      ]

      message("")
      message(
        "======================================================================"
      )
      message(
        "Processing DOI batch ",
        start,
        "-",
        end,
        " of ",
        length(doi_todo)
      )
      message(
        "======================================================================"
      )

      doi_map <- list()

      for (entry in batch) {

        if (nzchar(entry$doi)) {

          doi_map[
            [entry$doi]
          ] <- entry
        }
      }

      if (length(doi_map) == 0) {
        next
      }

      message(
        "Checking PMC for ",
        length(doi_map),
        " DOIs..."
      )

      pmc_records <- tryCatch(
        get_pmc_records(
          names(doi_map)
        ),
        error = function(e) {

          message(
            "PMC batch failed: ",
            conditionMessage(e)
          )

          errors <<- errors + length(batch)

          list()
        }
      )

      message(
        "PMC returned ",
        length(pmc_records),
        " records."
      )

      for (doi in names(doi_map)) {

        entry <- doi_map[[doi]]
        item <- entry$item

        current_pmid <- get_field(
          item,
          "PMID"
        )

        current_pmcid <- get_field(
          item,
          "PMCID"
        )

        if (
          nzchar(current_pmid) &&
          nzchar(current_pmcid)
        ) {

          already_complete <-
            already_complete + 1

          next
        }

        record <- pmc_records[[doi]]

        pmid <- ""
        pmcid <- ""

        if (!is.null(record)) {

          doi_lookups <-
            doi_lookups + 1

          pmid <- trimws(
            as.character(
              record$pmid %||% ""
            )
          )

          pmcid <- trimws(
            as.character(
              record$pmcid %||% ""
            )
          )
        }

        # --------------------------------------------------------------
        # PubMed DOI fallback
        # --------------------------------------------------------------

        if (!nzchar(pmid)) {

          message(
            "PubMed DOI fallback: ",
            doi
          )

          pubmed_pmid <- get_pubmed_id_by_doi(
            doi
          )

          if (
            !is.na(pubmed_pmid) &&
            nzchar(pubmed_pmid)
          ) {

            pmid <- pubmed_pmid

            pubmed_fallbacks <-
              pubmed_fallbacks + 1

            message(
              "  PMID -> ",
              pmid
            )
          }
        }

        # --------------------------------------------------------------
        # PubMed title + author fallback
        # --------------------------------------------------------------

        if (
          (
            !nzchar(pmid) ||
            !nzchar(pmcid)
          ) &&
          nzchar(entry$title)
        ) {

          message(
            "PubMed title match: ",
            entry$title
          )

          if (nzchar(entry$first_author)) {

            message(
              "  First author: ",
              entry$first_author
            )
          }

          title_record <- search_pubmed_by_title(
            entry$title,
            entry$first_author
          )

          if (!is.null(title_record)) {

            title_lookups <-
              title_lookups + 1

            message(
              "  MATCH: ",
              title_record$title
            )

            message(
              "  Match type: ",
              title_record$match_type
            )

            message(
              "  Match score: ",
              round(
                title_record$match_score,
                3
              )
            )

            if (
              isTRUE(
                title_record$author_match
              )
            ) {

              message(
                "  First author: MATCH"
              )

            } else {

              message(
                "  First author: no match"
              )
            }

            if (
              nzchar(
                title_record$pmid %||% ""
              )
            ) {

              message(
                "  PMID -> ",
                title_record$pmid
              )

            } else {

              message(
                "  PMID -> NOT FOUND"
              )
            }

            if (
              nzchar(
                title_record$pmcid %||% ""
              )
            ) {

              message(
                "  PMCID -> ",
                title_record$pmcid
              )

            } else {

              message(
                "  PMCID -> NOT FOUND"
              )
            }

            if (!nzchar(pmid)) {

              pmid <- trimws(
                as.character(
                  title_record$pmid %||% ""
                )
              )
            }

            if (!nzchar(pmcid)) {

              pmcid <- trimws(
                as.character(
                  title_record$pmcid %||% ""
                )
              )
            }

          } else {

            message(
              "  No reliable PubMed match."
            )
          }
        }

        # --------------------------------------------------------------
        # Final values
        # --------------------------------------------------------------

        final_pmid <- current_pmid
        final_pmcid <- current_pmcid

        if (
          !nzchar(final_pmid) &&
          nzchar(pmid)
        ) {
          final_pmid <- pmid
        }

        if (
          !nzchar(final_pmcid) &&
          nzchar(pmcid)
        ) {
          final_pmcid <- pmcid
        }

        # --------------------------------------------------------------
        # Report exactly what was found
        # --------------------------------------------------------------

        if (nzchar(final_pmid)) {

          message(
            "  FINAL PMID: ",
            final_pmid
          )

        } else {

          message(
            "  FINAL PMID: NOT FOUND"
          )
        }

        if (nzchar(final_pmcid)) {

          message(
            "  FINAL PMCID: ",
            final_pmcid
          )

        } else {

          message(
            "  FINAL PMCID: NOT FOUND"
          )
        }

        if (
          !nzchar(final_pmid) &&
          !nzchar(final_pmcid)
        ) {

          not_found <-
            not_found + 1

          message(
            "  No PMID or PMCID found."
          )

          next
        }

        add_pmid <-
          nzchar(final_pmid) &&
          !nzchar(current_pmid)

        add_pmcid <-
          nzchar(final_pmcid) &&
          !nzchar(current_pmcid)

        if (
          !add_pmid &&
          !add_pmcid
        ) {

          already_complete <-
            already_complete + 1

          next
        }

        message("")
        message(
          "FOUND: ",
          doi
        )

        if (add_pmid) {

          message(
            "  PMID  -> ",
            final_pmid
          )
        }

        if (add_pmcid) {

          message(
            "  PMCID -> ",
            final_pmcid
          )
        }

        if (dry_run) {

          message(
            "  DRY RUN - Zotero not modified."
          )

          if (add_pmid) {
            pmids_added <-
              pmids_added + 1
          }

          if (add_pmcid) {
            pmcids_added <-
              pmcids_added + 1
          }

          if (
            add_pmid &&
            add_pmcid
          ) {
            both_added <-
              both_added + 1
          }

          next
        }

        tryCatch({

          if (add_pmid) {

            item$data$PMID <-
              as.character(
                final_pmid
              )
          }

          if (add_pmcid) {

            item$data$PMCID <-
              as.character(
                final_pmcid
              )
          }

          update_zotero_item(
            item
          )

          updated <- updated + 1

          if (add_pmid) {
            pmids_added <-
              pmids_added + 1
          }

          if (add_pmcid) {
            pmcids_added <-
              pmcids_added + 1
          }

          if (
            add_pmid &&
            add_pmcid
          ) {
            both_added <-
              both_added + 1
          }

          message(
            "  UPDATED Zotero."
          )

        }, error = function(e) {

          errors <<-
            errors + 1

          message(
            "  ZOTERO ERROR: ",
            conditionMessage(e)
          )
        })
      }

      Sys.sleep(
        pmc_delay
      )
    }
  }

  # --------------------------------------------------------------------------
  # Process articles without DOI
  # --------------------------------------------------------------------------

  no_doi_todo <- todo[
    vapply(
      todo,
      function(x) !nzchar(x$doi),
      logical(1)
    )
  ]

  if (length(no_doi_todo) > 0) {

    message("")
    message(
      "======================================================================"
    )
    message(
      "PUBMED TITLE + AUTHOR SEARCH"
    )
    message(
      "======================================================================"
    )

    message(
      "Articles without DOI: ",
      length(no_doi_todo)
    )

    for (entry in no_doi_todo) {

      item <- entry$item
      title <- entry$title
      first_author <- entry$first_author

      current_pmid <- get_field(
        item,
        "PMID"
      )

      current_pmcid <- get_field(
        item,
        "PMCID"
      )

      if (
        nzchar(current_pmid) &&
        nzchar(current_pmcid)
      ) {

        already_complete <-
          already_complete + 1

        next
      }

      if (!nzchar(title)) {

        not_found <-
          not_found + 1

        message(
          "No title available for PubMed search."
        )

        next
      }

      message("")
      message(
        "PubMed title search: ",
        title
      )

      if (nzchar(first_author)) {

        message(
          "  Zotero first author: ",
          first_author
        )
      } else {

        message(
          "  Zotero first author: NOT AVAILABLE"
        )
      }

      record <- tryCatch(
        search_pubmed_by_title(
          title,
          first_author
        ),
        error = function(e) {

          message(
            "PubMed title search failed: ",
            conditionMessage(e)
          )

          NULL
        }
      )

      if (is.null(record)) {

        message(
          "  No reliable PubMed match."
        )

        not_found <-
          not_found + 1

        next
      }

      title_lookups <-
        title_lookups + 1

      pmid <- trimws(
        as.character(
          record$pmid %||% ""
        )
      )

      pmcid <- trimws(
        as.character(
          record$pmcid %||% ""
        )
      )

      message(
        "  MATCH: ",
        record$title
      )

      message(
        "  Match type: ",
        record$match_type
      )

      message(
        "  Match score: ",
        round(
          record$match_score,
          3
        )
      )

      if (
        isTRUE(
          record$author_match
        )
      ) {

        message(
          "  First author: MATCH"
        )

      } else {

        message(
          "  First author: no match"
        )
      }

      if (nzchar(pmid)) {

        message(
          "  PMID -> ",
          pmid
        )

      } else {

        message(
          "  PMID -> NOT FOUND"
        )
      }

      if (nzchar(pmcid)) {

        message(
          "  PMCID -> ",
          pmcid
        )

      } else {

        message(
          "  PMCID -> NOT FOUND"
        )
      }

      final_pmid <- current_pmid
      final_pmcid <- current_pmcid

      if (
        !nzchar(final_pmid) &&
        nzchar(pmid)
      ) {
        final_pmid <- pmid
      }

      if (
        !nzchar(final_pmcid) &&
        nzchar(pmcid)
      ) {
        final_pmcid <- pmcid
      }

      if (
        !nzchar(final_pmid) &&
        !nzchar(final_pmcid)
      ) {

        message(
          "  PubMed match found, but no identifier was returned."
        )

        not_found <-
          not_found + 1

        next
      }

      add_pmid <-
        nzchar(final_pmid) &&
        !nzchar(current_pmid)

      add_pmcid <-
        nzchar(final_pmcid) &&
        !nzchar(current_pmcid)

      if (
        !add_pmid &&
        !add_pmcid
      ) {

        already_complete <-
          already_complete + 1

        next
      }

      message("")
      message(
        "FOUND: ",
        title
      )

      if (add_pmid) {

        message(
          "  PMID  -> ",
          final_pmid
        )
      }

      if (add_pmcid) {

        message(
          "  PMCID -> ",
          final_pmcid
        )
      }

      if (dry_run) {

        message(
          "  DRY RUN - Zotero not modified."
        )

        if (add_pmid) {
          pmids_added <-
            pmids_added + 1
        }

        if (add_pmcid) {
          pmcids_added <-
            pmcids_added + 1
        }

        if (
          add_pmid &&
          add_pmcid
        ) {
          both_added <-
            both_added + 1
        }

        next
      }

      tryCatch({

        if (add_pmid) {

          item$data$PMID <-
            as.character(
              final_pmid
            )
        }

        if (add_pmcid) {

          item$data$PMCID <-
            as.character(
              final_pmcid
            )
        }

        update_zotero_item(
          item
        )

        updated <- updated + 1

        if (add_pmid) {
          pmids_added <-
            pmids_added + 1
        }

        if (add_pmcid) {
          pmcids_added <-
            pmcids_added + 1
        }

        if (
          add_pmid &&
          add_pmcid
        ) {
          both_added <-
            both_added + 1
        }

        message(
          "  UPDATED Zotero."
        )

      }, error = function(e) {

        errors <<-
          errors + 1

        message(
          "  ZOTERO ERROR: ",
          conditionMessage(e)
        )
      })
    }
  }

  # --------------------------------------------------------------------------
  # Final report
  # --------------------------------------------------------------------------

  message("")
  message("")
  message(
    "======================================================================"
  )
  message(
    "FINAL REPORT"
  )
  message(
    "======================================================================"
  )

  message(
    "Total journal articles:       ",
    length(items)
  )

  message(
    "Articles requiring lookup:    ",
    length(todo)
  )

  message(
    "Zotero items updated:          ",
    updated
  )

  message(
    "PMIDs added:                   ",
    pmids_added
  )

  message(
    "PMCIDs added:                  ",
    pmcids_added
  )

  message(
    "Both PMID + PMCID added:       ",
    both_added
  )

  message(
    "Already complete:              ",
    already_complete
  )

  message(
    "DOI lookups:                   ",
    doi_lookups
  )

  message(
    "PubMed title lookups:          ",
    title_lookups
  )

  message(
    "PubMed DOI fallbacks:          ",
    pubmed_fallbacks
  )

  message(
    "No identifier found:           ",
    not_found
  )

  message(
    "Errors:                        ",
    errors
  )

  message(
    "======================================================================"
  )

  if (dry_run) {

    message("")
    message(
      "DRY RUN COMPLETE."
    )
    message(
      "NOTHING WAS WRITTEN TO ZOTERO."
    )

  } else {

    message("")
    message(
      "UPDATE COMPLETE."
    )
  }

  invisible(
    list(
      total_articles = length(items),
      articles_requiring_lookup = length(todo),
      updated = updated,
      pmids_added = pmids_added,
      pmcids_added = pmcids_added,
      both_added = both_added,
      already_complete = already_complete,
      doi_lookups = doi_lookups,
      title_lookups = title_lookups,
      pubmed_fallbacks = pubmed_fallbacks,
      not_found = not_found,
      errors = errors,
      dry_run = dry_run
    )
  )
}
