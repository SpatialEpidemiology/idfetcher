#' Fetch and write PMID and PMCID identifiers to Zotero
#'
#' Retrieves PMID and PMCID identifiers for Zotero journal articles using:
#'   1. PMC ID Converter for DOI-based lookups
#'   2. PubMed DOI lookup when PMC does not provide a PMID
#'   3. PubMed title + first-author searches
#'   4. PubMed title-only searches as a final fallback
#'
#' Identifiers are written only to Zotero's PMID and PMCID fields.
#'
#' @param dry_run Logical. If TRUE, lookups are performed but Zotero is not
#' modified. Default is TRUE.
#' @param batch_size Number of DOIs sent to PMC in each batch. Default is 50.
#' @param pubmed_delay Seconds to wait between PubMed requests. Default is 1.
#' @param pmc_delay Seconds to wait between PMC batches. Default is 1.5.
#' @param max_retries Maximum number of retries after an API error or HTTP 429.
#' Default is 5.
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
    if (is.null(x) || length(x) == 0) {
      return(y)
    }

    if (length(x) == 1 && is.na(x)) {
      return(y)
    }

    x
  }

  get_field <- function(item, field) {

    value <- item$data[[field]]

    if (is.null(value) || length(value) == 0) {
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

    doi <- sub(
      "[[:punct:]]+$",
      "",
      doi
    )

    trimws(doi)
  }

  normalize_title <- function(title) {

    if (is.null(title) || length(title) == 0) {
      return("")
    }

    title <- as.character(title)[1]

    if (is.na(title)) {
      return("")
    }

    title <- trimws(title)

    if (!nzchar(title)) {
      return("")
    }

    title <- tolower(title)

    title <- gsub(
      "\u2018|\u2019|\u201A|\u201B",
      "'",
      title
    )

    title <- gsub(
      "\u201C|\u201D|\u201E|\u201F",
      "\"",
      title
    )

    title <- gsub(
      "\u2013|\u2014|\u2212",
      "-",
      title
    )

    title <- gsub(
      "&",
      " and ",
      title,
      fixed = TRUE
    )

    title <- gsub(
      "[^[:alnum:]]+",
      " ",
      title
    )

    title <- gsub(
      "[[:space:]]+",
      " ",
      title
    )

    trimws(title)
  }

  title_tokens <- function(title) {

    normalized <- normalize_title(title)

    if (!nzchar(normalized)) {
      return(character(0))
    }

    tokens <- strsplit(
      normalized,
      "[[:space:]]+"
    )[[1]]

    tokens[
      nchar(tokens) >= 3
    ]
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

  get_first_author <- function(item) {

    creators <- item$data$creators

    if (is.null(creators) || length(creators) == 0) {
      return("")
    }

    for (creator in creators) {

      creator_type <- creator$creatorType %||% ""

      if (
        nzchar(creator_type) &&
        identical(
          tolower(as.character(creator_type)),
          "author"
        )
      ) {

        last_name <- creator$lastName %||% ""

        if (
          !is.null(last_name) &&
          length(last_name) > 0 &&
          !is.na(last_name[1])
        ) {

          last_name <- trimws(
            as.character(last_name[1])
          )

          if (nzchar(last_name)) {
            return(last_name)
          }
        }

        name <- creator$name %||% ""

        if (
          !is.null(name) &&
          length(name) > 0 &&
          !is.na(name[1])
        ) {

          name <- trimws(
            as.character(name[1])
          )

          if (nzchar(name)) {
            return(name)
          }
        }
      }
    }

    ""
  }

  normalize_author <- function(author) {

    if (is.null(author) || length(author) == 0) {
      return("")
    }

    author <- as.character(author)[1]

    if (is.na(author)) {
      return("")
    }

    author <- tolower(trimws(author))

    author <- gsub(
      "[^[:alnum:]]+",
      "",
      author
    )

    author
  }

  # --------------------------------------------------------------------------
  # Generic NCBI request
  #
  # IMPORTANT:
  # httr2::req_url_query() does not accept a list through .args.
  # Passing .args = list(...) causes:
  #
  # "All elements of `...` must be either an atomic vector or NULL."
  #
  # Query parameters are therefore added one at a time using do.call().
  # --------------------------------------------------------------------------

  ncbi_request <- function(
    url,
    params,
    parse = c("json", "text"),
    request_name = "NCBI request"
  ) {

    parse <- match.arg(parse)

    if (!is.list(params)) {
      stop("NCBI request parameters must be a list.")
    }

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
          return(
            paste(
              as.character(x),
              collapse = ","
            )
          )
        }

        as.character(x)
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

      response <- tryCatch({

        req <- httr2::request(url)

        req <- httr2::req_headers(
          req,
          `User-Agent` = user_agent
        )

        # Add each query parameter individually.
        # This avoids passing a list into httr2's dynamic dots.
        for (param_name in names(params)) {

          param_value <- params[[param_name]]

          if (
            !is.null(param_value) &&
            length(param_value) > 0
          ) {

            query_arg <- setNames(
              list(param_value),
              param_name
            )

            req <- do.call(
              httr2::req_url_query,
              c(
                list(req),
                query_arg
              )
            )
          }
        }

        httr2::req_perform(req)

      }, error = function(e) {

        message(
          request_name,
          " error: ",
          conditionMessage(e)
        )

        NULL
      })

      if (is.null(response)) {

        if (attempt < max_retries) {

          wait_time <- max(
            2^(attempt - 1),
            1
          )

          message(
            "Retrying in ",
            wait_time,
            " seconds..."
          )

          Sys.sleep(wait_time)

          next
        }

        return(NULL)
      }

      status <- httr2::resp_status(response)

      if (status == 429) {

        wait_time <- max(
          2^(attempt - 1),
          1
        )

        if (attempt < max_retries) {

          message(
            request_name,
            " HTTP 429 Too Many Requests."
          )

          message(
            "Retrying in ",
            wait_time,
            " seconds..."
          )

          Sys.sleep(wait_time)

          next
        }

        message(
          request_name,
          " failed after ",
          max_retries,
          " attempts because of HTTP 429."
        )

        return(NULL)
      }

      if (status >= 400) {

        message(
          request_name,
          " error: HTTP ",
          status,
          " ",
          httr2::resp_status_desc(response)
        )

        if (attempt < max_retries) {

          wait_time <- max(
            2^(attempt - 1),
            1
          )

          message(
            "Retrying in ",
            wait_time,
            " seconds..."
          )

          Sys.sleep(wait_time)

          next
        }

        return(NULL)
      }

      if (parse == "json") {

        text <- httr2::resp_body_string(
          response
        )

        parsed <- tryCatch(
          jsonlite::fromJSON(
            text,
            simplifyVector = FALSE
          ),
          error = function(e) {

            message(
              request_name,
              " JSON parsing error: ",
              conditionMessage(e)
            )

            NULL
          }
        )

        return(parsed)
      }

      return(
        httr2::resp_body_string(
          response
        )
      )
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
      retmax = "10",
      tool = "idfetcher"
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

    if (
      is.null(json$esearchresult) ||
      is.null(json$esearchresult$idlist)
    ) {
      return(NA_character_)
    }

    ids <- json$esearchresult$idlist

    if (is.null(ids) || length(ids) == 0) {
      return(NA_character_)
    }

    ids <- unlist(
      ids,
      use.names = FALSE
    )

    if (length(ids) == 0) {
      return(NA_character_)
    }

    as.character(ids[1])
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
      ids = paste(
        dois,
        collapse = ","
      ),
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

    if (is.null(records) || length(records) == 0) {
      return(list())
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

      if (nzchar(record_doi)) {
        result[[record_doi]] <- record
      }
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
      id = paste(
        pmids,
        collapse = ","
      ),
      rettype = "xml",
      retmode = "xml",
      tool = "idfetcher"
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

    results <- list()

    for (article in articles) {

      pmid_node <- xml2::xml_find_first(
        article,
        ".//MedlineCitation/PMID"
      )

      title_node <- xml2::xml_find_first(
        article,
        ".//Article/ArticleTitle"
      )

      date_node <- xml2::xml_find_first(
        article,
        ".//Article/Journal/JournalIssue/PubDate"
      )

      pmid <- ""

      if (
        !inherits(
          pmid_node,
          "xml_missing"
        )
      ) {

        pmid <- trimws(
          xml2::xml_text(
            pmid_node
          )
        )
      }

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

          year <- trimws(
            xml2::xml_text(year_node)
          )
        }

        if (
          !inherits(
            month_node,
            "xml_missing"
          )
        ) {

          month <- trimws(
            xml2::xml_text(month_node)
          )
        }

        if (
          !inherits(
            day_node,
            "xml_missing"
          )
        ) {

          day <- trimws(
            xml2::xml_text(day_node)
          )
        }

        date_parts <- c(
          year,
          month,
          day
        )

        date_parts <- date_parts[
          nzchar(date_parts)
        ]

        if (length(date_parts) > 0) {

          pub_date <- paste(
            date_parts,
            collapse = "-"
          )
        }
      }

      article_ids <- list()

      id_nodes <- xml2::xml_find_all(
        article,
        ".//PubmedData/ArticleIdList/ArticleId"
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
            nzchar(id_type) &&
            nzchar(id_value)
          ) {

            # CORRECT: [[key]], not [ [key] ]
            article_ids[[tolower(id_type)]] <- id_value
          }
        }
      }

      # ----------------------------------------------------------------------
      # First author
      # ----------------------------------------------------------------------

      first_author <- ""

      author_node <- xml2::xml_find_first(
        article,
        ".//Article/AuthorList/Author[1]"
      )

      if (
        !inherits(
          author_node,
          "xml_missing"
        )
      ) {

        last_name_node <- xml2::xml_find_first(
          author_node,
          "./LastName"
        )

        collective_node <- xml2::xml_find_first(
          author_node,
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
            collective_node,
            "xml_missing"
          )
        ) {

          first_author <- trimws(
            xml2::xml_text(
              collective_node
            )
          )
        }
      }

      record <- list(
        pmid = pmid,
        title = title,
        first_author = first_author,
        pub_date = pub_date,
        doi = article_ids$doi %||% "",
        pmcid = article_ids$pmc %||% "",
        article_ids = article_ids
      )

      if (nzchar(pmid)) {
        results[[pmid]] <- record
      }
    }

    unname(results)
  }

  # --------------------------------------------------------------------------
  # Score PubMed records
  # --------------------------------------------------------------------------

  score_pubmed_record <- function(
    zotero_title,
    zotero_author,
    record
  ) {

    title_score <- title_similarity(
      zotero_title,
      record$title
    )

    zotero_author_norm <- normalize_author(
      zotero_author
    )

    pubmed_author_norm <- normalize_author(
      record$first_author
    )

    author_match <- FALSE

    if (
      nzchar(zotero_author_norm) &&
      nzchar(pubmed_author_norm)
    ) {

      author_match <- identical(
        zotero_author_norm,
        pubmed_author_norm
      )

      if (!author_match) {

        author_match <- grepl(
          zotero_author_norm,
          pubmed_author_norm,
          fixed = TRUE
        ) ||
          grepl(
            pubmed_author_norm,
            zotero_author_norm,
            fixed = TRUE
          )
      }
    }

    score <- title_score

    if (author_match) {

      score <- min(
        1,
        score + 0.05
      )
    }

    list(
      score = score,
      title_score = title_score,
      author_match = author_match
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

    scores <- lapply(
      records,
      function(record) {

        score_pubmed_record(
          zotero_title,
          zotero_author,
          record
        )
      }
    )

    score_values <- vapply(
      scores,
      function(x) x$score,
      numeric(1)
    )

    if (length(score_values) == 0) {
      return(NULL)
    }

    best_index <- which.max(
      score_values
    )

    # CORRECT: [[index]], not [ [index] ]
    best_record <- records[[best_index]]

    # CORRECT: [[index]], not [ [index] ]
    best_score <- scores[[best_index]]

    if (
      best_score$title_score >= 0.94
    ) {

      best_record$match_score <-
        best_score$score

      best_record$title_score <-
        best_score$title_score

      best_record$author_match <-
        best_score$author_match

      if (
        best_score$title_score >= 0.999
      ) {

        best_record$match_type <-
          "exact_title"

      } else if (
        best_score$author_match
      ) {

        best_record$match_type <-
          "strong_title_author"

      } else {

        best_record$match_type <-
          "strong_title"
      }

      return(best_record)
    }

    NULL
  }

  # --------------------------------------------------------------------------
  # Search PubMed
  # --------------------------------------------------------------------------

  search_pubmed_by_title <- function(
    title,
    first_author = ""
  ) {

    normalized <- normalize_title(
      title
    )

    if (!nzchar(normalized)) {
      return(NULL)
    }

    queries <- character(0)

    # ------------------------------------------------------------------------
    # Search 1: Exact title phrase
    # ------------------------------------------------------------------------

    queries <- c(
      queries,
      paste0(
        "\"",
        title,
        "\"[Title]"
      )
    )

    # ------------------------------------------------------------------------
    # Search 2: Title terms + first author
    # ------------------------------------------------------------------------

    words <- title_tokens(
      title
    )

    if (length(words) > 0) {

      title_words <- words[
        seq_len(
          min(
            length(words),
            10
          )
        )
      ]

      title_term <- paste(
        title_words,
        collapse = " AND "
      )

      title_author <- normalize_author(
        first_author
      )

      if (nzchar(title_author)) {

        queries <- c(
          queries,
          paste0(
            title_term,
            "[Title] AND ",
            first_author,
            "[Author]"
          )
        )
      }

      # ----------------------------------------------------------------------
      # Search 3: Longer title-term search without author
      # ----------------------------------------------------------------------

      queries <- c(
        queries,
        paste0(
          title_term,
          "[Title]"
        )
      )

      # ----------------------------------------------------------------------
      # Search 4: Shorter title + author
      # ----------------------------------------------------------------------

      if (length(words) >= 5) {

        short_words <- words[
          seq_len(
            min(
              length(words),
              6
            )
          )
        ]

        short_term <- paste(
          short_words,
          collapse = " AND "
        )

        if (nzchar(title_author)) {

          queries <- c(
            queries,
            paste0(
              short_term,
              "[Title] AND ",
              first_author,
              "[Author]"
            )
          )
        }
      }
    }

    queries <- unique(
      queries
    )

    # ------------------------------------------------------------------------
    # Execute searches
    # ------------------------------------------------------------------------

    all_ids <- character(0)

    for (query_index in seq_along(queries)) {

      query <- queries[
        query_index
      ]

      message(
        "  PubMed query ",
        query_index,
        "/",
        length(queries),
        ": ",
        query
      )

      params <- list(
        db = "pubmed",
        term = query,
        retmode = "json",
        retmax = "50",
        tool = "idfetcher"
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
        request_name = "PubMed title lookup"
      )

      if (!is.null(json)) {

        ids <- NULL

        if (!is.null(json$esearchresult)) {
          ids <- json$esearchresult$idlist
        }

        if (
          !is.null(ids) &&
          length(ids) > 0
        ) {

          ids <- as.character(
            unlist(
              ids,
              use.names = FALSE
            )
          )

          all_ids <- unique(
            c(
              all_ids,
              ids
            )
          )

          message(
            "  PubMed returned ",
            length(ids),
            " candidate PMID(s)."
          )

        } else {

          message(
            "  PubMed returned 0 candidates."
          )
        }
      }

      Sys.sleep(
        pubmed_delay
      )

      # Fetch candidates and test them.
      if (length(all_ids) > 0) {

        fetched <- fetch_pubmed_records(
          all_ids
        )

        if (length(fetched) > 0) {

          match <- choose_best_pubmed_match(
            title,
            first_author,
            fetched
          )

          if (!is.null(match)) {
            return(match)
          }
        }
      }
    }

    NULL
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

  message(
    "Fetching journal articles..."
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

      # CORRECT: append a list element using [[index]]
      todo[[length(todo) + 1]] <- list(
        item = item,
        doi = doi,
        title = title,
        first_author = first_author
      )
    }
  }

  message(
    "Articles missing PMID or PMCID: ",
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

          # CORRECT: named list element using [[key]]
          doi_map[[entry$doi]] <- entry
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

        # CORRECT: named list lookup using [[key]]
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

          doi_lookups <- doi_lookups + 1

          pmid <- record$pmid %||% ""
          pmcid <- record$pmcid %||% ""

          pmid <- trimws(
            as.character(pmid)
          )

          pmcid <- trimws(
            as.character(pmcid)
          )
        }

        # --------------------------------------------------------------------
        # PubMed DOI fallback
        # --------------------------------------------------------------------

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

        # --------------------------------------------------------------------
        # PubMed title + first-author fallback
        # --------------------------------------------------------------------

        if (
          (
            !nzchar(pmid) ||
            !nzchar(pmcid)
          ) &&
          nzchar(entry$title)
        ) {

          message(
            "  PubMed title match search: ",
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
              "  PubMed title match: ",
              title_record$title
            )

            message(
              "  Match score: ",
              round(
                title_record$match_score,
                3
              )
            )

            if (
              nzchar(title_record$first_author)
            ) {

              message(
                "  PubMed first author: ",
                title_record$first_author
              )
            }

            if (isTRUE(title_record$author_match)) {

              message(
                "  First-author match: YES"
              )

            } else {

              message(
                "  First-author match: NO"
              )
            }

            if (
              !nzchar(pmid) &&
              nzchar(title_record$pmid)
            ) {

              pmid <- title_record$pmid
            }

            if (
              !nzchar(pmcid) &&
              nzchar(title_record$pmcid)
            ) {

              pmcid <- title_record$pmcid
            }

            message(
              "  PMID identified: ",
              ifelse(
                nzchar(pmid),
                pmid,
                "NO"
              )
            )

            message(
              "  PMCID identified: ",
              ifelse(
                nzchar(pmcid),
                pmcid,
                "NO"
              )
            )
          }
        }

        # --------------------------------------------------------------------
        # Final values
        # --------------------------------------------------------------------

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

          not_found <- not_found + 1

          message(
            "  No PMID or PMCID found: ",
            doi
          )

          Sys.sleep(
            pubmed_delay
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

          Sys.sleep(
            pubmed_delay
          )

          next
        }

        message("")
        message(
          "FOUND: ",
          doi
        )

        message(
          "  PMID identified: ",
          ifelse(
            nzchar(final_pmid),
            final_pmid,
            "NO"
          )
        )

        message(
          "  PMCID identified: ",
          ifelse(
            nzchar(final_pmcid),
            final_pmcid,
            "NO"
          )
        )

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

        } else {

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

            errors <<- errors + 1

            message(
              "  ZOTERO ERROR: ",
              conditionMessage(e)
            )
          })
        }

        Sys.sleep(
          pubmed_delay
        )
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

        not_found <- not_found + 1

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
          "First author: ",
          first_author
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

      pmid <- record$pmid %||% ""
      pmcid <- record$pmcid %||% ""

      pmid <- trimws(
        as.character(pmid)
      )

      pmcid <- trimws(
        as.character(pmcid)
      )

      message(
        "  PubMed title match: ",
        record$title
      )

      message(
        "  Match score: ",
        round(
          record$match_score,
          3
        )
      )

      if (
        nzchar(record$first_author)
      ) {

        message(
          "  PubMed first author: ",
          record$first_author
        )
      }

      message(
        "  First-author match: ",
        ifelse(
          isTRUE(record$author_match),
          "YES",
          "NO"
        )
      )

      message(
        "  PMID identified: ",
        ifelse(
          nzchar(pmid),
          pmid,
          "NO"
        )
      )

      message(
        "  PMCID identified: ",
        ifelse(
          nzchar(pmcid),
          pmcid,
          "NO"
        )
      )

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

      message(
        "  PMID identified: ",
        ifelse(
          nzchar(final_pmid),
          final_pmid,
          "NO"
        )
      )

      message(
        "  PMCID identified: ",
        ifelse(
          nzchar(final_pmcid),
          final_pmcid,
          "NO"
        )
      )

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

      } else {

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

          errors <<- errors + 1

          message(
            "  ZOTERO ERROR: ",
            conditionMessage(e)
          )
        })
      }

      Sys.sleep(
        pubmed_delay
      )
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
    "Zotero items updated:         ",
    updated
  )

  message(
    "PMIDs added:                  ",
    pmids_added
  )

  message(
    "PMCIDs added:                 ",
    pmcids_added
  )

  message(
    "Both PMID + PMCID added:      ",
    both_added
  )

  message(
    "Already complete:             ",
    already_complete
  )

  message(
    "DOI/PMC lookups:              ",
    doi_lookups
  )

  message(
    "PubMed title lookups:         ",
    title_lookups
  )

  message(
    "PubMed DOI fallbacks:         ",
    pubmed_fallbacks
  )

  message(
    "No identifier found:          ",
    not_found
  )

  message(
    "Errors:                       ",
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
