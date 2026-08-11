#' Fetch and write PMID and PMCID identifiers to Zotero
#'
#' Retrieves PMID and PMCID identifiers for Zotero journal articles using:
#'   1. NCBI PMC ID Converter for DOI-based lookups
#'   2. PubMed DOI lookup as a fallback
#'   3. PubMed title searches for articles without DOI
#'
#' Identifiers are written only to Zotero's dedicated PMID and PMCID fields.
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
  # Validate arguments
  # --------------------------------------------------------------------------

  if (length(dry_run) != 1 || is.na(dry_run)) {
    stop("dry_run must be TRUE or FALSE.", call. = FALSE)
  }

  if (length(batch_size) != 1 ||
      is.na(batch_size) ||
      batch_size < 1) {
    stop("batch_size must be a positive integer.", call. = FALSE)
  }

  if (length(pubmed_delay) != 1 ||
      is.na(pubmed_delay) ||
      pubmed_delay < 0) {
    stop("pubmed_delay must be zero or greater.", call. = FALSE)
  }

  if (length(pmc_delay) != 1 ||
      is.na(pmc_delay) ||
      pmc_delay < 0) {
    stop("pmc_delay must be zero or greater.", call. = FALSE)
  }

  if (length(max_retries) != 1 ||
      is.na(max_retries) ||
      max_retries < 1) {
    stop("max_retries must be at least 1.", call. = FALSE)
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

  user_agent <- "idfetcher/1.0"

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

    if (is.null(item$data) || !is.list(item$data)) {
      return("")
    }

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

    doi <- trimws(doi)

    # Remove accidental trailing punctuation commonly copied with DOIs.
    doi <- sub("[[:punct:]]+$", "", doi)

    doi
  }

  normalize_title <- function(title) {

    if (is.null(title) || length(title) == 0) {
      return("")
    }

    title <- as.character(title)[1]

    if (is.na(title)) {
      return("")
    }

    title <- tolower(title)

    title <- gsub(
      "&",
      " and ",
      title,
      fixed = TRUE
    )

    # Normalize common Unicode punctuation before removing punctuation.
    title <- gsub(
      "[\u2018\u2019\u201A\u201B]",
      "'",
      title,
      perl = TRUE
    )

    title <- gsub(
      "[\u201C\u201D\u201E\u201F]",
      "\"",
      title,
      perl = TRUE
    )

    title <- gsub(
      "[\u2013\u2014]",
      "-",
      title,
      perl = TRUE
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

    score <- 1 - (distance / max_length)

    max(0, min(1, score))
  }

  # --------------------------------------------------------------------------
  # NCBI request helper
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

        # Add query parameters without using !!!.
        if (length(params) > 0) {

          for (param_name in names(params)) {

            param_value <- params[[param_name]]

            if (!is.null(param_value)) {

              req <- httr2::req_url_query(
                req,
                .list = setNames(
                  list(param_value),
                  param_name
                )
              )
            }
          }
        }

        response <- httr2::req_perform(req)

        status <- httr2::resp_status(response)

        if (status == 429) {

          wait_time <- max(
            2^(attempt - 1),
            1
          )

          message(
            request_name,
            " rate limit (429). Waiting ",
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
            ": ",
            httr2::resp_status_desc(response)
          )
        }

        if (parse == "json") {

          text <- httr2::resp_body_string(response)

          if (!nzchar(trimws(text))) {
            stop("Empty response.")
          }

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
      retmax = 10
    )

    if (!is.null(ncbi_api_key) &&
        nzchar(ncbi_api_key)) {

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

    ids <- NULL

    if (!is.null(json$esearchresult)) {
      ids <- json$esearchresult$idlist
    }

    if (is.null(ids) || length(ids) == 0) {
      return(NA_character_)
    }

    ids <- as.character(unlist(ids))

    if (length(ids) == 0 || !nzchar(ids[1])) {
      return(NA_character_)
    }

    ids[1]
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
      !is.na(pmids) & nzchar(pmids)
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

    if (!is.null(ncbi_api_key) &&
        nzchar(ncbi_api_key)) {

      params$api_key <- ncbi_api_key
    }

    xml_text <- ncbi_request(
      pubmed_efetch_url,
      params,
      parse = "text",
      request_name = "PubMed record fetch"
    )

    if (is.null(xml_text) ||
        !nzchar(trimws(xml_text))) {

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
        ".//PMID"
      )

      title_node <- xml2::xml_find_first(
        article,
        ".//ArticleTitle"
      )

      date_node <- xml2::xml_find_first(
        article,
        ".//PubDate"
      )

      pmid <- ""

      if (!inherits(pmid_node, "xml_missing")) {

        pmid <- trimws(
          xml2::xml_text(pmid_node)
        )
      }

      title <- ""

      if (!inherits(title_node, "xml_missing")) {

        title <- trimws(
          xml2::xml_text(title_node)
        )
      }

      pub_date <- ""

      if (!inherits(date_node, "xml_missing")) {

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

        if (!inherits(year_node, "xml_missing")) {
          year <- trimws(
            xml2::xml_text(year_node)
          )
        }

        if (!inherits(month_node, "xml_missing")) {
          month <- trimws(
            xml2::xml_text(month_node)
          )
        }

        if (!inherits(day_node, "xml_missing")) {
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

          if (!is.na(id_type) &&
              nzchar(id_type) &&
              nzchar(id_value)) {

            article_ids[[tolower(id_type)]] <- id_value
          }
        }
      }

      doi <- ""

      if (!is.null(article_ids$doi)) {
        doi <- normalize_doi(
          article_ids$doi
        )
      }

      pmcid <- ""

      if (!is.null(article_ids$pmc)) {
        pmcid <- trimws(
          as.character(article_ids$pmc)
        )
      }

      record <- list(
        pmid = pmid,
        title = title,
        pub_date = pub_date,
        doi = doi,
        pmcid = pmcid,
        article_ids = article_ids
      )

      results[[length(results) + 1]] <- record
    }

    results <- results[
      vapply(
        results,
        function(x) {
          !is.null(x$pmid) &&
            nzchar(x$pmid)
        },
        logical(1)
      )
    ]

    results
  }

  # --------------------------------------------------------------------------
  # Choose best PubMed title match
  # --------------------------------------------------------------------------

  choose_best_pubmed_match <- function(
    zotero_title,
    records
  ) {

    if (length(records) == 0) {
      return(NULL)
    }

    similarities <- vapply(
      records,
      function(record) {

        title_similarity(
          zotero_title,
          record$title
        )
      },
      numeric(1)
    )

    if (length(similarities) == 0) {
      return(NULL)
    }

    best_index <- which.max(
      similarities
    )

    best_score <- similarities[
      best_index
    ]

    # Correct R list indexing.
    best_record <- records[[best_index]]

    if (best_score >= 0.999) {

      best_record$match_type <- "exact_title"
      best_record$match_score <- best_score

      return(best_record)
    }

    if (best_score >= 0.94) {

      best_record$match_type <- "strong_title"
      best_record$match_score <- best_score

      return(best_record)
    }

    NULL
  }

  # --------------------------------------------------------------------------
  # PubMed title search
  # --------------------------------------------------------------------------

  search_pubmed_by_title <- function(title) {

    title_normalized <- normalize_title(title)

    if (!nzchar(title_normalized)) {
      return(NULL)
    }

    # ------------------------------------------------------------------------
    # Search 1: exact title phrase
    # ------------------------------------------------------------------------

    exact_params <- list(
      db = "pubmed",
      term = paste0(
        "\"",
        title,
        "\"[Title]"
      ),
      retmode = "json",
      retmax = 20
    )

    if (!is.null(ncbi_api_key) &&
        nzchar(ncbi_api_key)) {

      exact_params$api_key <- ncbi_api_key
    }

    json <- ncbi_request(
      pubmed_esearch_url,
      exact_params,
      parse = "json",
      request_name = "PubMed exact title lookup"
    )

    if (!is.null(json) &&
        !is.null(json$esearchresult)) {

      ids <- json$esearchresult$idlist

      if (!is.null(ids) &&
          length(ids) > 0) {

        ids <- as.character(
          unlist(ids)
        )

        fetched <- fetch_pubmed_records(
          ids
        )

        if (length(fetched) > 0) {

          match <- choose_best_pubmed_match(
            title,
            fetched
          )

          if (!is.null(match)) {
            return(match)
          }
        }
      }
    }

    # ------------------------------------------------------------------------
    # Search 2: normalized title terms
    # ------------------------------------------------------------------------

    words <- strsplit(
      title_normalized,
      "\\s+"
    )[[1]]

    words <- words[
      nchar(words) >= 3
    ]

    if (length(words) == 0) {
      return(NULL)
    }

    # Use the most informative title words.
    # Keep enough words to distinguish long article titles.
    if (length(words) > 18) {
      words <- words[seq_len(18)]
    }

    broad_query <- paste(
      paste0(
        words,
        "[Title]"
      ),
      collapse = " AND "
    )

    broad_params <- list(
      db = "pubmed",
      term = broad_query,
      retmode = "json",
      retmax = 100
    )

    if (!is.null(ncbi_api_key) &&
        nzchar(ncbi_api_key)) {

      broad_params$api_key <- ncbi_api_key
    }

    json <- ncbi_request(
      pubmed_esearch_url,
      broad_params,
      parse = "json",
      request_name = "PubMed broad title lookup"
    )

    if (is.null(json) ||
        is.null(json$esearchresult)) {

      return(NULL)
    }

    ids <- json$esearchresult$idlist

    if (is.null(ids) ||
        length(ids) == 0) {

      return(NULL)
    }

    ids <- as.character(
      unlist(ids)
    )

    fetched <- fetch_pubmed_records(
      ids
    )

    if (length(fetched) == 0) {
      return(NULL)
    }

    choose_best_pubmed_match(
      title,
      fetched
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
      ids = paste(dois, collapse = ","),
      idtype = "doi",
      format = "json",
      tool = "idfetcher"
    )

    if (!is.null(ncbi_api_key) &&
        nzchar(ncbi_api_key)) {

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

    if (is.null(records) ||
        length(records) == 0) {

      return(list())
    }

    # PMC's JSON response can be a list of records.
    # Normalize it into a named list keyed by DOI.
    result <- list()

    if (is.list(records)) {

      for (record in records) {

        if (!is.list(record)) {
          next
        }

        record_doi <- ""

        if (!is.null(record$doi)) {
          record_doi <- normalize_doi(
            record$doi
          )
        }

        if (!nzchar(record_doi) &&
            !is.null(record$requested_id)) {

          record_doi <- normalize_doi(
            record$requested_id
          )
        }

        if (!nzchar(record_doi) &&
            !is.null(record[["requested-id"]])) {

          record_doi <- normalize_doi(
            record[["requested-id"]]
          )
        }

        if (nzchar(record_doi)) {

          result[[record_doi]] <- record
        }
      }
    }

    result
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

      response <- tryCatch(
        httr2::req_perform(req),
        error = function(e) {

          stop(
            "Zotero request failed: ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )

      status <- httr2::resp_status(
        response
      )

      if (status >= 400) {

        stop(
          "Zotero API error: HTTP ",
          status,
          " - ",
          httr2::resp_status_desc(response),
          call. = FALSE
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

    if (is.null(item_key) ||
        !nzchar(item_key)) {

      stop(
        "Zotero item has no key.",
        call. = FALSE
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

    if (!is.null(item$version)) {

      req <- httr2::req_headers(
        req,
        `If-Unmodified-Since-Version` =
          as.character(item$version)
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
        httr2::resp_status_desc(response),
        call. = FALSE
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

    if (!nzchar(pmid) ||
        !nzchar(pmcid)) {

      todo[[length(todo) + 1]] <- list(
        item = item,
        doi = doi,
        title = title
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
  # DOI records
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

      dois <- unique(
        vapply(
          batch,
          function(x) x$doi,
          character(1)
        )
      )

      message(
        "Checking PMC for ",
        length(dois),
        " DOIs..."
      )

      pmc_records <- tryCatch(
        get_pmc_records(dois),
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

        if (nzchar(current_pmid) &&
            nzchar(current_pmcid)) {

          already_complete <-
            already_complete + 1

          next
        }

        pmid <- ""
        pmcid <- ""

        record <- NULL

        if (!is.null(pmc_records[[doi]])) {

          record <- pmc_records[[doi]]

          doi_lookups <-
            doi_lookups + 1
        }

        if (!is.null(record)) {

          if (!is.null(record$pmid)) {

            pmid <- trimws(
              as.character(record$pmid)[1]
            )
          }

          if (!is.null(record$pmcid)) {

            pmcid <- trimws(
              as.character(record$pmcid)[1]
            )
          }
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

          if (!is.na(pubmed_pmid) &&
              nzchar(pubmed_pmid)) {

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
        # PubMed title fallback
        # --------------------------------------------------------------------

        if ((!nzchar(pmid) ||
             !nzchar(pmcid)) &&
            nzchar(entry$title)) {

          title_record <- tryCatch(
            search_pubmed_by_title(
              entry$title
            ),
            error = function(e) {

              message(
                "PubMed title fallback failed: ",
                conditionMessage(e)
              )

              NULL
            }
          )

          if (!is.null(title_record)) {

            title_lookups <-
              title_lookups + 1

            if (!nzchar(pmid)) {
              pmid <- title_record$pmid
            }

            if (!nzchar(pmcid)) {
              pmcid <- title_record$pmcid
            }
          }
        }

        final_pmid <- current_pmid
        final_pmcid <- current_pmcid

        if (!nzchar(final_pmid) &&
            nzchar(pmid)) {

          final_pmid <- pmid
        }

        if (!nzchar(final_pmcid) &&
            nzchar(pmcid)) {

          final_pmcid <- pmcid
        }

        if (!nzchar(final_pmid) &&
            !nzchar(final_pmcid)) {

          not_found <- not_found + 1

          message(
            "No PMID or PMCID: ",
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

        if (!add_pmid &&
            !add_pmcid) {

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

          if (add_pmid &&
              add_pmcid) {

            both_added <-
              both_added + 1
          }

          Sys.sleep(
            pubmed_delay
          )

          next
        }

        tryCatch({

          if (add_pmid) {

            item$data$PMID <-
              as.character(final_pmid)
          }

          if (add_pmcid) {

            item$data$PMCID <-
              as.character(final_pmcid)
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

          if (add_pmid &&
              add_pmcid) {

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
  # Articles without DOI
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

      current_pmid <- get_field(
        item,
        "PMID"
      )

      current_pmcid <- get_field(
        item,
        "PMCID"
      )

      if (nzchar(current_pmid) &&
          nzchar(current_pmcid)) {

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

      record <- tryCatch(
        search_pubmed_by_title(
          title
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

        Sys.sleep(
          pubmed_delay
        )

        next
      }

      title_lookups <-
        title_lookups + 1

      pmid <- record$pmid %||% ""
      pmcid <- record$pmcid %||% ""

      pmid <- trimws(
        as.character(pmid)[1]
      )

      pmcid <- trimws(
        as.character(pmcid)[1]
      )

      message(
        "  Match: ",
        record$title
      )

      message(
        "  Match score: ",
        round(
          record$match_score,
          3
        )
      )

      if (nzchar(pmid)) {

        message(
          "  PMID -> ",
          pmid
        )
      }

      if (nzchar(pmcid)) {

        message(
          "  PMCID -> ",
          pmcid
        )
      }

      final_pmid <- current_pmid
      final_pmcid <- current_pmcid

      if (!nzchar(final_pmid) &&
          nzchar(pmid)) {

        final_pmid <- pmid
      }

      if (!nzchar(final_pmcid) &&
          nzchar(pmcid)) {

        final_pmcid <- pmcid
      }

      if (!nzchar(final_pmid) &&
          !nzchar(final_pmcid)) {

        message(
          "  PubMed match found, but no PMID or PMCID was returned."
        )

        not_found <-
          not_found + 1

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

      if (!add_pmid &&
          !add_pmcid) {

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

        if (add_pmid &&
            add_pmcid) {

          both_added <-
            both_added + 1
        }

        Sys.sleep(
          pubmed_delay
        )

        next
      }

      tryCatch({

        if (add_pmid) {

          item$data$PMID <-
            as.character(final_pmid)
        }

        if (add_pmcid) {

          item$data$PMCID <-
            as.character(final_pmcid)
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

        if (add_pmid &&
            add_pmcid) {

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
    "PubMed fallbacks:              ",
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
