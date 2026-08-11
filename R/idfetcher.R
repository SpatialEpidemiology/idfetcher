#' Fetch and write PMID and PMCID identifiers to Zotero
#'
#' Retrieves PMID and PMCID identifiers for Zotero journal articles using:
#'   1. NCBI PMC ID Converter for DOI-based lookups
#'   2. PubMed title searches for articles without DOI or when DOI lookup
#'      does not provide an identifier
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
      is.na(value)
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

  # Normalize titles for comparison.
  #
  # This intentionally removes punctuation and collapses whitespace so that
  # PubMed titles such as:
  #
  # Menopausal hormone use and ovarian cancer risk:
  # individual participant meta-analysis...
  #
  # can be compared against Zotero titles with different punctuation.

  normalize_title <- function(title) {

    if (
      is.null(title) ||
      length(title) == 0 ||
      is.na(title)
    ) {
      return("")
    }

    title <- as.character(title)[1]

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

    1 - (distance / max_length)
  }

  # --------------------------------------------------------------------------
  # Generic NCBI request with retry / 429 handling
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

        req <- httr2::req_url_query(
          req,
          !!!params
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
        '"',
        doi,
        '"[DOI]'
      ),
      retmode = "json",
      retmax = 10
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

    as.character(ids[[1]])
  }

  # --------------------------------------------------------------------------
  # PMC DOI batch lookup
  # --------------------------------------------------------------------------

  get_pmc_records <- function(dois) {

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

    result <- list()

    for (record in records) {

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
  # PubMed title search
  # --------------------------------------------------------------------------

  search_pubmed_by_title <- function(title) {

    if (!nzchar(normalize_title(title))) {
      return(NULL)
    }

    # First search:
    #
    # Exact PubMed title phrase.
    #
    # This is the primary title fallback.

    exact_params <- list(
      db = "pubmed",
      term = paste0(
        '"',
        title,
        '"[Title]'
      ),
      retmode = "json",
      retmax = 20
    )

    if (
      !is.null(ncbi_api_key) &&
      nzchar(ncbi_api_key)
    ) {
      exact_params$api_key <- ncbi_api_key
    }

    json <- ncbi_request(
      pubmed_esearch_url,
      exact_params,
      parse = "json",
      request_name = "PubMed title lookup"
    )

    if (is.null(json)) {
      return(NULL)
    }

    ids <- json$esearchresult$idlist

    if (
      !is.null(ids) &&
      length(ids) > 0
    ) {

      ids <- as.character(unlist(ids))

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

    # ------------------------------------------------------------------------
    # Second search:
    #
    # PubMed title search without the quoted phrase.
    #
    # This handles differences in punctuation, subtitles, hyphens,
    # apostrophes, Unicode characters, etc.
    # ------------------------------------------------------------------------

    clean_title <- normalize_title(title)

    words <- strsplit(
      clean_title,
      "\\s+"
    )[[1]]

    words <- words[nchar(words) >= 3]

    if (length(words) == 0) {
      return(NULL)
    }

    # Avoid making an enormous PubMed query.
    #
    # The longest/most informative first portion of the title is generally
    # sufficient to retrieve the article.

    words <- words[seq_len(
      min(length(words), 12)
    )]

    broad_query <- paste0(
      words,
      "[Title]",
      collapse = " AND "
    )

    broad_params <- list(
      db = "pubmed",
      term = broad_query,
      retmode = "json",
      retmax = 50
    )

    if (
      !is.null(ncbi_api_key) &&
      nzchar(ncbi_api_key)
    ) {
      broad_params$api_key <- ncbi_api_key
    }

    json <- ncbi_request(
      pubmed_esearch_url,
      broad_params,
      parse = "json",
      request_name = "PubMed broad title lookup"
    )

    if (is.null(json)) {
      return(NULL)
    }

    ids <- json$esearchresult$idlist

    if (
      is.null(ids) ||
      length(ids) == 0
    ) {
      return(NULL)
    }

    ids <- as.character(unlist(ids))

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
  # Fetch PubMed records as XML
  # --------------------------------------------------------------------------

  fetch_pubmed_records <- function(pmids) {

    if (length(pmids) == 0) {
      return(list())
    }

    pmids <- unique(
      as.character(pmids)
    )

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

      pmid <- if (
        inherits(pmid_node, "xml_missing")
      ) {
        ""
      } else {
        trimws(
          xml2::xml_text(
            pmid_node
          )
        )
      }

      title <- if (
        inherits(title_node, "xml_missing")
      ) {
        ""
      } else {
        trimws(
          xml2::xml_text(
            title_node
          )
        )
      }

      pub_date <- ""

      if (
        !inherits(date_node, "xml_missing")
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

        year <- if (
          inherits(year_node, "xml_missing")
        ) {
          ""
        } else {
          xml2::xml_text(year_node)
        }

        month <- if (
          inherits(month_node, "xml_missing")
        ) {
          ""
        } else {
          xml2::xml_text(month_node)
        }

        day <- if (
          inherits(day_node, "xml_missing")
        ) {
          ""
        } else {
          xml2::xml_text(day_node)
        }

        pub_date <- paste(
          c(year, month, day)[
            nzchar(c(year, month, day))
          ],
          collapse = "-"
        )
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

          if (
            !is.na(id_type) &&
            nzchar(id_value)
          ) {

            article_ids[[tolower(id_type)]] <-
              id_value
          }
        }
      }

      results[[counter]] <- list(
        pmid = pmid,
        title = title,
        pub_date = pub_date,
        doi = article_ids$doi %||% "",
        pmcid = article_ids$pmc %||% "",
        article_ids = article_ids
      )
    }

    results[
      vapply(
        results,
        function(x) nzchar(x$pmid),
        logical(1)
      )
    ]
  }

  # --------------------------------------------------------------------------
  # Choose the best PubMed title match
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

    best_record <- records[
      [best_index]
    ]

    # Exact normalized title match.
    if (best_score >= 0.999) {
      best_record$match_type <- "exact_title"
      best_record$match_score <- best_score
      return(best_record)
    }

    # Very strong title match.
    if (best_score >= 0.94) {
      best_record$match_type <- "strong_title"
      best_record$match_score <- best_score
      return(best_record)
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

      response <- httr2::req_perform(req)

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

    # IMPORTANT:
    # Zotero item updates require PUT.
    # The previous implementation was sending the default GET request,
    # which produced HTTP 405 Method Not Allowed.

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

    body <- item$data

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
      body,
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

    # An article needs lookup if either identifier is missing.
    #
    # DOI articles can use PMC ID Converter.
    # Non-DOI articles can use PubMed title search.

    if (
      !nzchar(pmid) ||
      !nzchar(pmcid)
    ) {

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

          doi_map[[entry$doi]] <-
            entry
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

        pmid <- NULL
        pmcid <- NULL

        if (!is.null(record)) {

          doi_lookups <- doi_lookups + 1

          pmid <- record$pmid %||% NULL
          pmcid <- record$pmcid %||% NULL

          if (!is.null(pmid)) {

            pmid <- trimws(
              as.character(pmid)
            )
          }

          if (!is.null(pmcid)) {

            pmcid <- trimws(
              as.character(pmcid)
            )
          }
        }

        # --------------------------------------------------------------------
        # PubMed DOI fallback
        # --------------------------------------------------------------------

        if (
          is.null(pmid) ||
          !nzchar(pmid)
        ) {

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
        # If PMC supplied PMID but no PMCID, try title lookup.
        #
        # This can recover records where PubMed has the article but PMC
        # conversion did not return a PMCID.
        # --------------------------------------------------------------------

        if (
          (
            is.null(pmcid) ||
            !nzchar(pmcid)
          ) &&
          nzchar(entry$title)
        ) {

          title_record <- search_pubmed_by_title(
            entry$title
          )

          if (!is.null(title_record)) {

            title_lookups <-
              title_lookups + 1

            if (
              is.null(pmid) ||
              !nzchar(pmid)
            ) {
              pmid <- title_record$pmid
            }

            if (
              is.null(pmcid) ||
              !nzchar(pmcid)
            ) {
              pmcid <-
                title_record$pmcid %||%
                ""
            }
          }
        }

        Sys.sleep(
          pubmed_delay
        )

        # --------------------------------------------------------------------
        # Final values
        # --------------------------------------------------------------------

        final_pmid <- current_pmid
        final_pmcid <- current_pmcid

        if (
          !nzchar(final_pmid) &&
          !is.null(pmid) &&
          !is.na(pmid) &&
          nzchar(pmid)
        ) {
          final_pmid <- pmid
        }

        if (
          !nzchar(final_pmcid) &&
          !is.null(pmcid) &&
          !is.na(pmcid) &&
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
            "No PMID or PMCID: ",
            doi
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
  # Process articles without DOI using PubMed title search
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
        as.character(pmid)
      )

      pmcid <- trimws(
        as.character(pmcid)
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

        Sys.sleep(
          pubmed_delay
        )

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
