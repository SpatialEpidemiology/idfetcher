#' Fetch and write PMID and PMCID identifiers to Zotero
#'
#' Uses DOI-based PMC lookup first, followed by PubMed DOI lookup and
#' PubMed title matching when necessary.
#'
#' @param dry_run Logical. If TRUE, no Zotero records are modified.
#' @param batch_size Number of DOIs per PMC lookup batch.
#' @param pubmed_delay Minimum delay between PubMed requests.
#' @param pmc_delay Delay between PMC batches.
#' @param max_retries Maximum number of retries for failed requests.
#' @param ncbi_api_key Optional NCBI API key.
#'
#' @return Invisibly returns a run summary.
#' @export
idfetcher <- function(
    dry_run = TRUE,
    batch_size = 50,
    pubmed_delay = 1,
    pmc_delay = 1.5,
    max_retries = 5,
    ncbi_api_key = ""
) {

  # ------------------------------------------------------------------------
  # Package checks
  # ------------------------------------------------------------------------

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

  # ------------------------------------------------------------------------
  # Credentials
  # ------------------------------------------------------------------------

  credentials <- idfetcher_get_credentials()

  user_id <- credentials$user_id
  api_key <- credentials$api_key

  # ------------------------------------------------------------------------
  # URLs
  # ------------------------------------------------------------------------

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

  # Tracks the last PubMed request so that all PubMed calls are throttled.
  last_pubmed_request <- 0

  # ------------------------------------------------------------------------
  # Helpers
  # ------------------------------------------------------------------------

  `%||%` <- function(x, y) {
    if (is.null(x)) {
      return(y)
    }

    x
  }

  safe_character <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return("")
    }

    x <- as.character(x)[1]

    if (is.na(x)) {
      return("")
    }

    trimws(x)
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

    doi <- safe_character(doi)

    if (!nzchar(doi)) {
      return("")
    }

    doi <- tolower(doi)

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

    doi <- sub(
      "[[:space:]]+$",
      "",
      doi
    )

    doi
  }

  normalize_title <- function(title) {

    title <- safe_character(title)

    if (!nzchar(title)) {
      return("")
    }

    title <- tolower(title)

    # Normalize common HTML/XML entities.
    title <- gsub(
      "&amp;",
      " and ",
      title,
      fixed = TRUE
    )

    title <- gsub(
      "&",
      " and ",
      title,
      fixed = TRUE
    )

    # Convert punctuation to spaces.
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

  # ------------------------------------------------------------------------
  # NCBI request helper
  # ------------------------------------------------------------------------

  ncbi_request <- function(
    url,
    params,
    parse = c("json", "text"),
    request_name = "NCBI request",
    pubmed_request = FALSE
  ) {

    parse <- match.arg(parse)

    for (attempt in seq_len(max_retries)) {

      # --------------------------------------------------------------
      # Enforce PubMed request spacing.
      # --------------------------------------------------------------

      if (pubmed_request) {

        elapsed <- Sys.time() -
          as.POSIXct(
            last_pubmed_request,
            origin = "1970-01-01"
          )

        elapsed_seconds <- as.numeric(
          elapsed,
          units = "secs"
        )

        if (
          is.finite(elapsed_seconds) &&
          elapsed_seconds < pubmed_delay
        ) {
          Sys.sleep(
            pubmed_delay - elapsed_seconds
          )
        }

        last_pubmed_request <<-
          as.numeric(Sys.time())
      }

      result <- tryCatch({

        req <- httr2::request(url)

        req <- httr2::req_headers(
          req,
          `User-Agent` = user_agent
        )

        # Do not use !!!params here.
        #
        # Building the query with do.call avoids the
        # "All elements of ... must be either an atomic vector
        # or NULL" error.

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
          } else {
            wait_time <- 2^(attempt - 1)
          }

          wait_time <- max(
            1,
            min(wait_time, 60)
          )

          message(
            request_name,
            " rate limit (429). Waiting ",
            round(wait_time, 1),
            " seconds..."
          )

          Sys.sleep(wait_time)

          return(NULL)
        }

        if (status >= 400) {

          stop(
            "HTTP ",
            status,
            " - ",
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

          wait_time <- 2^(attempt - 1)

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

  # ------------------------------------------------------------------------
  # PubMed PMID lookup by DOI
  # ------------------------------------------------------------------------

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
      request_name = "PubMed DOI lookup",
      pubmed_request = TRUE
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

    ids[[1]]
  }

  # ------------------------------------------------------------------------
  # PMC DOI batch lookup
  # ------------------------------------------------------------------------

  get_pmc_records <- function(dois) {

    if (length(dois) == 0) {
      return(list())
    }

    dois <- unique(
      normalize_doi(dois)
    )

    dois <- dois[nzchar(dois)]

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
      request_name = "PMC lookup",
      pubmed_request = FALSE
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

  # ------------------------------------------------------------------------
  # Fetch PubMed XML records
  # ------------------------------------------------------------------------

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

    xml_text <- ncbi_request(
      pubmed_efetch_url,
      params,
      parse = "text",
      request_name = "PubMed record fetch",
      pubmed_request = TRUE
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

    counter <- 0

    for (article in articles) {

      counter <- counter + 1

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
        pmid <- safe_character(
          xml2::xml_text(pmid_node)
        )
      }

      title <- ""

      if (
        !inherits(
          title_node,
          "xml_missing"
        )
      ) {
        title <- safe_character(
          xml2::xml_text(title_node)
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

        if (
          !inherits(
            year_node,
            "xml_missing"
          )
        ) {
          year <- safe_character(
            xml2::xml_text(year_node)
          )
        }

        month <- ""

        if (
          !inherits(
            month_node,
            "xml_missing"
          )
        ) {
          month <- safe_character(
            xml2::xml_text(month_node)
          )
        }

        day <- ""

        if (
          !inherits(
            day_node,
            "xml_missing"
          )
        ) {
          day <- safe_character(
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

        pub_date <- paste(
          date_parts,
          collapse = "-"
        )
      }

      # --------------------------------------------------------------
      # Extract ArticleId values.
      # --------------------------------------------------------------

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

          id_value <- safe_character(
            xml2::xml_text(node)
          )

          if (
            !is.na(id_type) &&
            nzchar(id_value)
          ) {

            id_type <- tolower(
              safe_character(id_type)
            )

            article_ids[[id_type]] <- id_value
          }
        }
      }

      # --------------------------------------------------------------
      # PMCID can sometimes appear in PubMed XML under different
      # structures. Check all ArticleId nodes as a secondary fallback.
      # --------------------------------------------------------------

      if (
        is.null(article_ids$pmc) ||
        !nzchar(article_ids$pmc)
      ) {

        all_id_nodes <- xml2::xml_find_all(
          article,
          ".//ArticleId"
        )

        if (length(all_id_nodes) > 0) {

          for (node in all_id_nodes) {

            id_type <- tolower(
              safe_character(
                xml2::xml_attr(
                  node,
                  "IdType"
                )
              )
            )

            id_value <- safe_character(
              xml2::xml_text(node)
            )

            if (
              identical(id_type, "pmc") &&
              nzchar(id_value)
            ) {
              article_ids$pmc <- id_value
            }
          }
        }
      }

      results[[counter]] <- list(
        pmid = pmid,
        title = title,
        pub_date = pub_date,
        doi = safe_character(
          article_ids$doi %||% ""
        ),
        pmcid = safe_character(
          article_ids$pmc %||% ""
        ),
        article_ids = article_ids
      )
    }

    results[
      vapply(
        results,
        function(x) {
          nzchar(x$pmid)
        },
        logical(1)
      )
    ]
  }

  # ------------------------------------------------------------------------
  # Choose best PubMed match
  # ------------------------------------------------------------------------

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

    # Correct R indexing:
    # records[[best_index]]
    best_record <- records[[best_index]]

    if (best_score >= 0.999) {

      best_record$match_type <-
        "exact_title"

      best_record$match_score <-
        best_score

      return(best_record)
    }

    if (best_score >= 0.94) {

      best_record$match_type <-
        "strong_title"

      best_record$match_score <-
        best_score

      return(best_record)
    }

    NULL
  }

  # ------------------------------------------------------------------------
  # PubMed title search
  # ------------------------------------------------------------------------

  search_pubmed_by_title <- function(title) {

    title <- safe_character(title)

    if (!nzchar(normalize_title(title))) {
      return(NULL)
    }

    # --------------------------------------------------------------
    # First attempt: exact title phrase
    # --------------------------------------------------------------

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
      request_name = "PubMed title lookup",
      pubmed_request = TRUE
    )

    if (!is.null(json)) {

      ids <- json$esearchresult$idlist

      if (
        !is.null(ids) &&
        length(ids) > 0
      ) {

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

    # --------------------------------------------------------------
    # Second attempt: title-word search.
    #
    # This is intentionally only reached if the exact title search
    # did not produce a sufficiently good match.
    # --------------------------------------------------------------

    clean_title <- normalize_title(title)

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

    # Use the most informative first 12 words.
    words <- words[
      seq_len(
        min(
          length(words),
          12
        )
      )
    ]

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
      request_name = "PubMed broad title lookup",
      pubmed_request = TRUE
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

  # ------------------------------------------------------------------------
  # Get Zotero journal articles
  # ------------------------------------------------------------------------

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

  # ------------------------------------------------------------------------
  # Update Zotero item
  # ------------------------------------------------------------------------

  update_zotero_item <- function(item) {

    item_key <- safe_character(
      item$key
    )

    if (!nzchar(item_key)) {
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

  # ------------------------------------------------------------------------
  # Counters
  # ------------------------------------------------------------------------

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

  # ------------------------------------------------------------------------
  # Start
  # ------------------------------------------------------------------------

  message("")
  message(
    "======================================================================"
  )
  message("IDFETCHER")
  message(
    "======================================================================"
  )
  message("")

  message("Connecting to Zotero...")
  message("Fetching journal articles...")

  items <- get_zotero_articles()

  message("")
  message(
    "Total journal articles: ",
    length(items)
  )

  # ------------------------------------------------------------------------
  # Identify articles requiring lookup
  # ------------------------------------------------------------------------

  todo <- list()

  for (item in items) {

    doi <- normalize_doi(
      item$data$DOI %||% ""
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

  # ------------------------------------------------------------------------
  # Process DOI articles
  # ------------------------------------------------------------------------

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

      dois <- vapply(
        batch,
        function(x) x$doi,
        character(1)
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

          errors <<-
            errors + length(batch)

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

        record <- pmc_records[[doi]]

        pmid <- ""
        pmcid <- ""

        if (!is.null(record)) {

          doi_lookups <-
            doi_lookups + 1

          pmid <- safe_character(
            record$pmid %||% ""
          )

          pmcid <- safe_character(
            record$pmcid %||% ""
          )
        }

        # --------------------------------------------------------------
        # If PMC did not provide PMID, ask PubMed by DOI.
        # --------------------------------------------------------------

        if (!nzchar(pmid)) {

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

            pubmed_fallbacks <-
              pubmed_fallbacks + 1

            message(
              "  PMID -> ",
              pmid
            )

          } else {

            message(
              "  PMID -> not found by DOI"
            )
          }
        }

        # --------------------------------------------------------------
        # If PMCID is still missing, use PubMed title search.
        #
        # This is also useful because the PubMed XML record can provide
        # the PMCID even when the PMC DOI converter did not.
        # --------------------------------------------------------------

        if (
          !nzchar(pmcid) &&
          nzchar(entry$title)
        ) {

          title_record <- tryCatch(
            search_pubmed_by_title(
              entry$title
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

            title_lookups <-
              title_lookups + 1

            if (!nzchar(pmid)) {
              pmid <- safe_character(
                title_record$pmid
              )
            }

            if (!nzchar(pmcid)) {
              pmcid <- safe_character(
                title_record$pmcid
              )
            }

            message("")
            message(
              "PubMed title match: ",
              title_record$title
            )

            message(
              "  Match score: ",
              round(
                title_record$match_score,
                3
              )
            )

            message(
              "  PMID -> ",
              if (
                nzchar(title_record$pmid)
              ) {
                title_record$pmid
              } else {
                "not returned"
              }
            )

            message(
              "  PMCID -> ",
              if (
                nzchar(title_record$pmcid)
              ) {
                title_record$pmcid
              } else {
                "not available"
              }
            )
          }
        }

        # --------------------------------------------------------------
        # Determine final values.
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
        # Explicit identifier report.
        # --------------------------------------------------------------

        message("")
        message(
          "RESULT: ",
          if (nzchar(entry$title)) {
            entry$title
          } else {
            doi
          }
        )

        message(
          "  PMID identified: ",
          if (nzchar(final_pmid)) {
            final_pmid
          } else {
            "NO"
          }
        )

        message(
          "  PMCID identified: ",
          if (nzchar(final_pmcid)) {
            final_pmcid
          } else {
            "NO"
          }
        )

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

          message(
            "  Zotero: no new identifiers to add."
          )

          already_complete <-
            already_complete + 1

          next
        }

        if (add_pmid) {
          message(
            "  ADD PMID -> ",
            final_pmid
          )
        }

        if (add_pmcid) {
          message(
            "  ADD PMCID -> ",
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

        # --------------------------------------------------------------
        # Write Zotero
        # --------------------------------------------------------------

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

          updated <-
            updated + 1

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

  # ------------------------------------------------------------------------
  # Process articles without DOI
  # ------------------------------------------------------------------------

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

        message(
          "No title available for PubMed search."
        )

        not_found <-
          not_found + 1

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

        next
      }

      title_lookups <-
        title_lookups + 1

      pmid <- safe_character(
        record$pmid
      )

      pmcid <- safe_character(
        record$pmcid
      )

      message("")
      message(
        "PubMed title match: ",
        record$title
      )

      message(
        "  Match score: ",
        round(
          record$match_score,
          3
        )
      )

      message(
        "  PMID -> ",
        if (nzchar(pmid)) {
          pmid
        } else {
          "not returned"
        }
      )

      message(
        "  PMCID -> ",
        if (nzchar(pmcid)) {
          pmcid
        } else {
          "not available"
        }
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

      message("")
      message(
        "RESULT: ",
        title
      )

      message(
        "  PMID identified: ",
        if (nzchar(final_pmid)) {
          final_pmid
        } else {
          "NO"
        }
      )

      message(
        "  PMCID identified: ",
        if (nzchar(final_pmcid)) {
          final_pmcid
        } else {
          "NO"
        }
      )

      if (
        !nzchar(final_pmid) &&
        !nzchar(final_pmcid)
      ) {

        message(
          "  No identifier available to add."
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

        message(
          "  Zotero: no new identifiers to add."
        )

        already_complete <-
          already_complete + 1

        next
      }

      message("")

      if (add_pmid) {

        message(
          "  ADD PMID -> ",
          final_pmid
        )
      }

      if (add_pmcid) {

        message(
          "  ADD PMCID -> ",
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

        updated <-
          updated + 1

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

  # ------------------------------------------------------------------------
  # Final report
  # ------------------------------------------------------------------------

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
