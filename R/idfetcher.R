#' Fetch and write PMID and PMCID identifiers to Zotero
#'
#' Retrieves PMID and PMCID identifiers for Zotero journal articles using:
#'   1. NCBI PMC ID Converter for DOI-based lookups
#'   2. PubMed DOI searches
#'   3. PubMed title + first-author searches
#'   4. PubMed progressively broader title searches
#'
#' Identifiers are written only to Zotero's PMID and PMCID fields.
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

  required_packages <- c(
    "httr2",
    "jsonlite",
    "xml2"
  )

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Package '", pkg, "' is required. ",
        "Run install.packages('", pkg, "').",
        call. = FALSE
      )
    }
  }

  # --------------------------------------------------------------------------
  # Validate arguments
  # --------------------------------------------------------------------------

  if (
    !is.numeric(batch_size) ||
    length(batch_size) != 1 ||
    is.na(batch_size) ||
    batch_size < 1
  ) {
    stop("batch_size must be a positive number.", call. = FALSE)
  }

  if (
    !is.numeric(max_retries) ||
    length(max_retries) != 1 ||
    is.na(max_retries) ||
    max_retries < 1
  ) {
    stop("max_retries must be a positive number.", call. = FALSE)
  }

  batch_size <- as.integer(batch_size)
  max_retries <- as.integer(max_retries)

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
  # Small helpers
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

    value <- as.character(value[1])

    if (is.na(value)) {
      return("")
    }

    trimws(value)
  }

  normalize_doi <- function(doi) {

    if (
      is.null(doi) ||
      length(doi) == 0 ||
      is.na(doi[1])
    ) {
      return("")
    }

    doi <- trimws(tolower(as.character(doi[1])))

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

    title <- as.character(title[1])

    # Normalize common Unicode punctuation.
    title <- gsub(
      "\u2018|\u2019|\u201A|\u201B",
      "'",
      title,
      perl = TRUE
    )

    title <- gsub(
      "\u201C|\u201D|\u201E",
      "\"",
      title,
      perl = TRUE
    )

    title <- gsub(
      "\u2013|\u2014",
      "-",
      title,
      perl = TRUE
    )

    title <- gsub(
      "\u00A0",
      " ",
      title,
      fixed = TRUE
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

  title_tokens <- function(title) {

    normalized <- normalize_title(title)

    if (!nzchar(normalized)) {
      return(character())
    }

    tokens <- strsplit(
      normalized,
      "\\s+"
    )[[1]]

    tokens[
      nchar(tokens) >= 3
    ]
  }

  title_similarity <- function(x, y) {

    x <- normalize_title(x)
    y <- normalize_title(y)

    if (
      !nzchar(x) ||
      !nzchar(y)
    ) {
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

  clean_author <- function(author) {

    if (
      is.null(author) ||
      length(author) == 0 ||
      is.na(author[1])
    ) {
      return("")
    }

    author <- as.character(author[1])

    author <- gsub(
      "[^A-Za-z0-9'-]",
      " ",
      author
    )

    author <- gsub(
      "\\s+",
      " ",
      author
    )

    trimws(author)
  }

  get_zotero_first_author <- function(item) {

    creators <- item$data$creators

    if (
      is.null(creators) ||
      length(creators) == 0
    ) {
      return("")
    }

    first_creator <- creators[[1]]

    if (is.null(first_creator)) {
      return("")
    }

    last_name <- first_creator$lastName %||% ""

    if (
      is.null(last_name) ||
      length(last_name) == 0
    ) {
      return("")
    }

    clean_author(last_name)
  }

  # --------------------------------------------------------------------------
  # Generic NCBI request
  #
  # Important:
  # Do not use !!!params here. req_url_query() is given the parameters
  # through do.call(), which avoids the earlier httr2 dynamic-dots failure.
  # --------------------------------------------------------------------------

  ncbi_request <- function(
    url,
    params,
    parse = c("json", "text"),
    request_name = "NCBI request"
  ) {

    parse <- match.arg(parse)

    for (attempt in seq_len(max_retries)) {

      result <- tryCatch({

        req <- httr2::request(url)

        req <- httr2::req_headers(
          req,
          `User-Agent` = user_agent
        )

        query_args <- c(
          list(req),
          params
        )

        req <- do.call(
          httr2::req_url_query,
          query_args
        )

        response <- httr2::req_perform(req)

        status <- httr2::resp_status(response)

        if (status == 429) {

          retry_after <- httr2::resp_header(
            response,
            "retry-after"
          )

          if (
            !is.null(retry_after) &&
            nzchar(retry_after)
          ) {

            wait_time <- suppressWarnings(
              as.numeric(retry_after)
            )

            if (
              is.na(wait_time) ||
              wait_time < 1
            ) {
              wait_time <- 2^(attempt - 1)
            }

          } else {

            wait_time <- max(
              2^(attempt - 1),
              1
            )
          }

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

          text <- httr2::resp_body_string(
            response
          )

          return(
            jsonlite::fromJSON(
              text,
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
  # PubMed ESearch
  # --------------------------------------------------------------------------

  pubmed_search <- function(
    term,
    retmax = 50,
    request_name = "PubMed search"
  ) {

    if (
      is.null(term) ||
      !nzchar(trimws(term))
    ) {
      return(character())
    }

    params <- list(
      db = "pubmed",
      term = term,
      retmode = "json",
      retmax = as.character(retmax),
      sort = "relevance"
    )

    if (
      !is.null(ncbi_api_key) &&
      nzchar(ncbi_api_key)
    ) {
      params$api_key <- ncbi_api_key
    }

    # Explicit delay before each PubMed request.
    Sys.sleep(pubmed_delay)

    json <- ncbi_request(
      pubmed_esearch_url,
      params,
      parse = "json",
      request_name = request_name
    )

    if (is.null(json)) {
      return(character())
    }

    search_result <- json$esearchresult

    if (is.null(search_result)) {
      return(character())
    }

    ids <- search_result$idlist

    if (
      is.null(ids) ||
      length(ids) == 0
    ) {
      return(character())
    }

    as.character(
      unlist(ids)
    )
  }

  # --------------------------------------------------------------------------
  # PubMed PMID lookup by DOI
  # --------------------------------------------------------------------------

  get_pubmed_id_by_doi <- function(doi) {

    doi <- normalize_doi(doi)

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

    Sys.sleep(pubmed_delay)

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

    as.character(
      unlist(ids)[1]
    )
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

    Sys.sleep(pmc_delay)

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

    if (
      is.null(records) ||
      length(records) == 0
    ) {
      return(list())
    }

    result <- list()

    for (record in records) {

      if (is.null(record)) {
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
      retmode = "xml"
    )

    if (
      !is.null(ncbi_api_key) &&
      nzchar(ncbi_api_key)
    ) {
      params$api_key <- ncbi_api_key
    }

    Sys.sleep(pubmed_delay)

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

    counter <- 0L

    for (article in articles) {

      counter <- counter + 1L

      # PMID
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

      # Title
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

      # Publication date
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

        if (
          !inherits(
            year_node,
            "xml_missing"
          )
        ) {
          year <- xml2::xml_text(
            year_node
          )
        }

        month <- ""

        if (
          !inherits(
            month_node,
            "xml_missing"
          )
        ) {
          month <- xml2::xml_text(
            month_node
          )
        }

        day <- ""

        if (
          !inherits(
            day_node,
            "xml_missing"
          )
        ) {
          day <- xml2::xml_text(
            day_node
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

        pub_date <- paste(
          date_parts,
          collapse = "-"
        )
      }

      # Article identifiers
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

      # First author
      first_author <- ""

      first_author_node <- xml2::xml_find_first(
        article,
        ".//AuthorList/Author[1]"
      )

      if (
        !inherits(
          first_author_node,
          "xml_missing"
        )
      ) {

        collective_node <- xml2::xml_find_first(
          first_author_node,
          "./CollectiveName"
        )

        lastname_node <- xml2::xml_find_first(
          first_author_node,
          "./LastName"
        )

        if (
          !inherits(
            lastname_node,
            "xml_missing"
          )
        ) {

          first_author <- trimws(
            xml2::xml_text(
              lastname_node
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

      results[[counter]] <- list(
        pmid = pmid,
        pmcid = article_ids$pmc %||% "",
        doi = normalize_doi(
          article_ids$doi %||% ""
        ),
        title = title,
        pub_date = pub_date,
        first_author = first_author,
        article_ids = article_ids
      )
    }

    results[
      vapply(
        results,
        function(x) {
          !is.null(x) &&
            nzchar(x$pmid)
        },
        logical(1)
      )
    ]
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

    zotero_title_normalized <- normalize_title(
      zotero_title
    )

    zotero_author_normalized <- clean_author(
      zotero_author
    )

    scores <- vapply(
      records,
      function(record) {

        title_score <- title_similarity(
          zotero_title,
          record$title
        )

        author_bonus <- 0

        if (
          nzchar(zotero_author_normalized) &&
          nzchar(record$first_author)
        ) {

          a <- tolower(
            clean_author(
              zotero_author_normalized
            )
          )

          b <- tolower(
            clean_author(
              record$first_author
            )
          )

          if (
            identical(a, b)
          ) {
            author_bonus <- 0.03
          }
        }

        min(
          1,
          title_score + author_bonus
        )
      },
      numeric(1)
    )

    if (length(scores) == 0) {
      return(NULL)
    }

    best_index <- which.max(scores)

    best_score <- scores[
      best_index
    ]

    best_record <- records[
      [best_index]
    ]

    raw_title_score <- title_similarity(
      zotero_title,
      best_record$title
    )

    author_match <- FALSE

    if (
      nzchar(zotero_author_normalized) &&
      nzchar(best_record$first_author)
    ) {

      author_match <- identical(
        tolower(
          clean_author(
            zotero_author_normalized
          )
        ),
        tolower(
          clean_author(
            best_record$first_author
          )
        )
      )
    }

    # Exact normalized title.
    if (
      identical(
        normalize_title(
          best_record$title
        ),
        zotero_title_normalized
      )
    ) {

      best_record$match_type <- "exact_title"

      best_record$match_score <- raw_title_score

      best_record$author_match <- author_match

      return(best_record)
    }

    # Strong title match.
    if (raw_title_score >= 0.94) {

      best_record$match_type <- "strong_title"

      best_record$match_score <- raw_title_score

      best_record$author_match <- author_match

      return(best_record)
    }

    # Slightly lower title threshold is allowed only when the
    # first author also matches.
    if (
      raw_title_score >= 0.88 &&
      author_match
    ) {

      best_record$match_type <- "title_author"

      best_record$match_score <- raw_title_score

      best_record$author_match <- TRUE

      return(best_record)
    }

    NULL
  }

  # --------------------------------------------------------------------------
  # PubMed title search
  # --------------------------------------------------------------------------

  search_pubmed_by_title <- function(
    title,
    first_author = ""
  ) {

    if (
      !nzchar(
        normalize_title(title)
      )
    ) {
      return(NULL)
    }

    first_author <- clean_author(
      first_author
    )

    # ------------------------------------------------------------------------
    # Search 1:
    # Exact title phrase.
    # ------------------------------------------------------------------------

    exact_term <- paste0(
      "\"",
      title,
      "\"[Title]"
    )

    ids <- pubmed_search(
      exact_term,
      retmax = 50,
      request_name = "PubMed exact title lookup"
    )

    if (length(ids) > 0) {

      records <- fetch_pubmed_records(
        ids
      )

      match <- choose_best_pubmed_match(
        title,
        first_author,
        records
      )

      if (!is.null(match)) {
        return(match)
      }
    }

    # ------------------------------------------------------------------------
    # Search 2:
    # Exact title + first author.
    #
    # This is particularly useful when PubMed's exact title indexing does
    # not return the record because of punctuation, subtitles, or indexing
    # differences.
    # ------------------------------------------------------------------------

    if (nzchar(first_author)) {

      author_term <- paste0(
        first_author,
        "[Author]"
      )

      title_term <- paste0(
        "\"",
        title,
        "\"[Title]"
      )

      author_query <- paste(
        title_term,
        author_term,
        sep = " AND "
      )

      ids <- pubmed_search(
        author_query,
        retmax = 50,
        request_name = "PubMed title-author lookup"
      )

      if (length(ids) > 0) {

        records <- fetch_pubmed_records(
          ids
        )

        match <- choose_best_pubmed_match(
          title,
          first_author,
          records
        )

        if (!is.null(match)) {
          return(match)
        }
      }
    }

    # ------------------------------------------------------------------------
    # Search 3:
    # Title tokens + first author.
    #
    # Use the most informative words from the title rather than requiring
    # the entire title to match exactly.
    # ------------------------------------------------------------------------

    tokens <- title_tokens(title)

    if (length(tokens) == 0) {
      return(NULL)
    }

    # Keep the first 12 substantial words.
    tokens <- tokens[
      seq_len(
        min(
          length(tokens),
          12
        )
      )
    ]

    if (nzchar(first_author)) {

      token_query <- paste0(
        tokens,
        "[Title]",
        collapse = " AND "
      )

      token_query <- paste(
        token_query,
        paste0(
          first_author,
          "[Author]"
        ),
        sep = " AND "
      )

      ids <- pubmed_search(
        token_query,
        retmax = 100,
        request_name = "PubMed title-author token lookup"
      )

      if (length(ids) > 0) {

        records <- fetch_pubmed_records(
          ids
        )

        match <- choose_best_pubmed_match(
          title,
          first_author,
          records
        )

        if (!is.null(match)) {
          return(match)
        }
      }
    }

    # ------------------------------------------------------------------------
    # Search 4:
    # Progressive title-token searches without requiring every word.
    #
    # This handles articles where PubMed's indexed title differs enough from
    # the Zotero title that an exact title search fails.
    # ------------------------------------------------------------------------

    search_lengths <- unique(
      c(
        min(length(tokens), 10),
        min(length(tokens), 8),
        min(length(tokens), 6),
        min(length(tokens), 5),
        min(length(tokens), 4)
      )
    )

    for (n in search_lengths) {

      if (n < 3) {
        next
      }

      selected_tokens <- tokens[
        seq_len(n)
      ]

      token_query <- paste0(
        selected_tokens,
        "[Title]",
        collapse = " AND "
      )

      if (nzchar(first_author)) {

        token_query <- paste(
          token_query,
          paste0(
            first_author,
            "[Author]"
          ),
          sep = " AND "
        )
      }

      ids <- pubmed_search(
        token_query,
        retmax = 100,
        request_name = paste0(
          "PubMed progressive title lookup (",
          n,
          " words)"
        )
      )

      if (length(ids) == 0) {
        next
      }

      records <- fetch_pubmed_records(
        ids
      )

      match <- choose_best_pubmed_match(
        title,
        first_author,
        records
      )

      if (!is.null(match)) {
        return(match)
      }
    }

    NULL
  }

  # --------------------------------------------------------------------------
  # Get Zotero journal articles
  # --------------------------------------------------------------------------

  get_zotero_articles <- function() {

    all_items <- list()

    start <- 0L
    limit <- 100L

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

      req <- httr2::request(url)

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

    req <- httr2::request(url)

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

    if (
      status != 200 &&
      status != 204
    ) {

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

    first_author <- get_zotero_first_author(
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
  # Counters
  # --------------------------------------------------------------------------

  updated <- 0L
  pmids_added <- 0L
  pmcids_added <- 0L
  both_added <- 0L
  already_complete <- 0L
  doi_lookups <- 0L
  title_lookups <- 0L
  pubmed_fallbacks <- 0L
  not_found <- 0L
  errors <- 0L

  # --------------------------------------------------------------------------
  # Helper for reporting PubMed result
  # --------------------------------------------------------------------------

  report_pubmed_result <- function(
    record,
    source
  ) {

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
      !is.null(record$first_author) &&
      nzchar(record$first_author)
    ) {

      message(
        "  PubMed first author: ",
        record$first_author
      )
    }

    if (
      !is.null(record$pmid) &&
      nzchar(record$pmid)
    ) {

      message(
        "  PMID FOUND: ",
        record$pmid
      )

    } else {

      message(
        "  PMID: not present in PubMed record."
      )
    }

    if (
      !is.null(record$pmcid) &&
      nzchar(record$pmcid)
    ) {

      message(
        "  PMCID FOUND: ",
        record$pmcid
      )

    } else {

      message(
        "  PMCID: not present in PubMed record."
      )
    }

    message(
      "  Match source: ",
      source
    )
  }

  # --------------------------------------------------------------------------
  # Helper to apply identifiers to an item
  # --------------------------------------------------------------------------

  process_found_identifiers <- function(
    item,
    current_pmid,
    current_pmcid,
    pmid,
    pmcid,
    description
  ) {

    pmid <- if (
      is.null(pmid) ||
      is.na(pmid)
    ) {
      ""
    } else {
      trimws(as.character(pmid))
    }

    pmcid <- if (
      is.null(pmcid) ||
      is.na(pmcid)
    ) {
      ""
    } else {
      trimws(as.character(pmcid))
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

    add_pmid <- (
      nzchar(final_pmid) &&
        !nzchar(current_pmid)
    )

    add_pmcid <- (
      nzchar(final_pmcid) &&
        !nzchar(current_pmcid)
    )

    if (
      !add_pmid &&
      !add_pmcid
    ) {

      message(
        "  No new identifiers to add."
      )

      return(
        list(
          status = "already_complete",
          updated = FALSE,
          pmid_added = FALSE,
          pmcid_added = FALSE,
          both_added = FALSE
        )
      )
    }

    message("")
    message(
      "FOUND: ",
      description
    )

    if (add_pmid) {

      message(
        "  PMID -> ",
        final_pmid
      )
    } else {

      message(
        "  PMID already present: ",
        current_pmid
      )
    }

    if (add_pmcid) {

      message(
        "  PMCID -> ",
        final_pmcid
      )
    } else if (nzchar(current_pmcid)) {

      message(
        "  PMCID already present: ",
        current_pmcid
      )
    } else {

      message(
        "  PMCID not identified."
      )
    }

    if (dry_run) {

      message(
        "  DRY RUN - Zotero not modified."
      )

      return(
        list(
          status = "found",
          updated = FALSE,
          pmid_added = add_pmid,
          pmcid_added = add_pmcid,
          both_added = (
            add_pmid &&
              add_pmcid
          )
        )
      )
    }

    if (add_pmid) {

      item$data$PMID <- as.character(
        final_pmid
      )
    }

    if (add_pmcid) {

      item$data$PMCID <- as.character(
        final_pmcid
      )
    }

    update_zotero_item(
      item
    )

    message(
      "  UPDATED Zotero."
    )

    list(
      status = "updated",
      updated = TRUE,
      pmid_added = add_pmid,
      pmcid_added = add_pmcid,
      both_added = (
        add_pmid &&
          add_pmcid
      )
    )
  }

  # --------------------------------------------------------------------------
  # Process DOI articles
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

      doi_names <- vapply(
        batch,
        function(x) x$doi,
        character(1)
      )

      message(
        "Checking PMC for ",
        length(doi_names),
        " DOIs..."
      )

      pmc_records <- tryCatch(
        get_pmc_records(
          doi_names
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

      for (entry in batch) {

        item <- entry$item
        doi <- entry$doi

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

          already_complete <- (
            already_complete + 1L
          )

          next
        }

        pmid <- ""
        pmcid <- ""

        record <- pmc_records[[doi]]

        if (!is.null(record)) {

          doi_lookups <- (
            doi_lookups + 1L
          )

          pmid <- record$pmid %||% ""
          pmcid <- record$pmcid %||% ""

          pmid <- if (
            is.null(pmid) ||
            is.na(pmid)
          ) {
            ""
          } else {
            trimws(as.character(pmid))
          }

          pmcid <- if (
            is.null(pmcid) ||
            is.na(pmcid)
          ) {
            ""
          } else {
            trimws(as.character(pmcid))
          }

          message("")
          message(
            "PMC match for DOI: ",
            doi
          )

          if (nzchar(pmid)) {

            message(
              "  PMC PMID FOUND: ",
              pmid
            )

          } else {

            message(
              "  PMC PMID: not returned."
            )
          }

          if (nzchar(pmcid)) {

            message(
              "  PMC PMCID FOUND: ",
              pmcid
            )

          } else {

            message(
              "  PMC PMCID: not returned."
            )
          }
        }

        # --------------------------------------------------------------------
        # DOI -> PubMed fallback
        # --------------------------------------------------------------------

        if (
          !nzchar(pmid) ||
          !nzchar(pmcid)
        ) {

          message("")
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

            pubmed_fallbacks <- (
              pubmed_fallbacks + 1L
            )

            message(
              "  PMID FOUND: ",
              pmid
            )

          } else {

            message(
              "  PMID not found by DOI search."
            )
          }
        }

        # --------------------------------------------------------------------
        # PubMed title fallback
        # --------------------------------------------------------------------

        if (
          (
            !nzchar(pmid) ||
            !nzchar(pmcid)
          ) &&
          nzchar(entry$title)
        ) {

          message("")
          message(
            "PubMed title fallback: ",
            entry$title
          )

          title_record <- tryCatch(
            search_pubmed_by_title(
              entry$title,
              entry$first_author
            ),
            error = function(e) {

              message(
                "PubMed title search failed: ",
                conditionMessage(e)
              )

              NULL
            }
          )

          if (!is.null(title_record)) {

            title_lookups <- (
              title_lookups + 1L
            )

            report_pubmed_result(
              title_record,
              "title search"
            )

            if (!nzchar(pmid)) {
              pmid <- title_record$pmid %||% ""
            }

            if (!nzchar(pmcid)) {
              pmcid <- title_record$pmcid %||% ""
            }

          } else {

            message(
              "  No reliable PubMed match."
            )
          }
        }

        # --------------------------------------------------------------------
        # Apply result
        # --------------------------------------------------------------------

        if (
          !nzchar(pmid) &&
          !nzchar(pmcid)
        ) {

          not_found <- (
            not_found + 1L
          )

          message(
            "  No PMID or PMCID identified: ",
            entry$title
          )

          next
        }

        result <- tryCatch(
          process_found_identifiers(
            item = item,
            current_pmid = current_pmid,
            current_pmcid = current_pmcid,
            pmid = pmid,
            pmcid = pmcid,
            description = doi
          ),
          error = function(e) {

            errors <<- (
              errors + 1L
            )

            message(
              "  ZOTERO ERROR: ",
              conditionMessage(e)
            )

            NULL
          }
        )

        if (!is.null(result)) {

          if (result$updated) {
            updated <- updated + 1L
          }

          if (result$pmid_added) {
            pmids_added <- pmids_added + 1L
          }

          if (result$pmcid_added) {
            pmcids_added <- pmcids_added + 1L
          }

          if (result$both_added) {
            both_added <- both_added + 1L
          }

          if (
            identical(
              result$status,
              "already_complete"
            )
          ) {
            already_complete <- (
              already_complete + 1L
            )
          }
        }
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
      "PUBMED TITLE SEARCH"
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

        already_complete <- (
          already_complete + 1L
        )

        next
      }

      if (!nzchar(title)) {

        not_found <- (
          not_found + 1L
        )

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

        not_found <- (
          not_found + 1L
        )

        next
      }

      title_lookups <- (
        title_lookups + 1L
      )

      report_pubmed_result(
        record,
        "title search"
      )

      pmid <- record$pmid %||% ""
      pmcid <- record$pmcid %||% ""

      result <- tryCatch(
        process_found_identifiers(
          item = item,
          current_pmid = current_pmid,
          current_pmcid = current_pmcid,
          pmid = pmid,
          pmcid = pmcid,
          description = title
        ),
        error = function(e) {

          errors <<- (
            errors + 1L
          )

          message(
            "  ZOTERO ERROR: ",
            conditionMessage(e)
          )

          NULL
        }
      )

      if (!is.null(result)) {

        if (result$updated) {
          updated <- updated + 1L
        }

        if (result$pmid_added) {
          pmids_added <- pmids_added + 1L
        }

        if (result$pmcid_added) {
          pmcids_added <- pmcids_added + 1L
        }

        if (result$both_added) {
          both_added <- both_added + 1L
        }

        if (
          identical(
            result$status,
            "already_complete"
          )
        ) {
          already_complete <- (
            already_complete + 1L
          )
        }
      }
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
