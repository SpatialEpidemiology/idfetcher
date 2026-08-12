#' Fetch and write PMID and PMCID identifiers to Zotero
#'
#' Retrieves PMID and PMCID identifiers for Zotero journal articles using:
#'   1. NCBI PMC ID Converter for DOI-based lookups
#'   2. PubMed DOI lookup as a fallback
#'   3. PubMed title + first-author searches for articles without DOI
#'   4. Additional PubMed title searches when DOI lookup is incomplete
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
#' @param ncbi_api_key Optional NCBI API key.
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

    if (length(x) == 0) {
      return(y)
    }

    x
  }

  safe_character <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x[1])) {
      return("")
    }

    trimws(as.character(x[1]))
  }

  get_field <- function(item, field) {

    value <- item$data[[field]]

    safe_character(value)
  }

  normalize_doi <- function(doi) {

    doi <- safe_character(doi)

    if (!nzchar(doi)) {
      return("")
    }

    doi <- tolower(doi)

    doi <- gsub(
      "^https?://(dx\\.)?doi\\.org/",
      "",
      doi
    )

    doi <- sub(
      "^doi:\\s*",
      "",
      doi
    )

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

  normalize_author <- function(author) {

    author <- safe_character(author)

    if (!nzchar(author)) {
      return("")
    }

    author <- tolower(author)

    author <- gsub(
      "[^a-z0-9]+",
      "",
      author
    )

    author
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

  # --------------------------------------------------------------------------
  # Generic NCBI request
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

        # IMPORTANT:
        # req_url_query() requires named atomic values.
        # Do not use !!!params here.
        for (param_name in names(params)) {

          param_value <- params[[param_name]]

          if (
            is.null(param_value) ||
            length(param_value) == 0
          ) {
            next
          }

          param_value <- as.character(param_value[1])

          req <- httr2::req_url_query(
            req,
            !!!setNames(
              list(param_value),
              param_name
            )
          )
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
  # PubMed DOI lookup
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

    as.character(unlist(ids))[1]
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
  # Fetch PubMed XML records
  # --------------------------------------------------------------------------

  fetch_pubmed_records <- function(pmids) {

    if (length(pmids) == 0) {
      return(list())
    }

    pmids <- unique(
      as.character(unlist(pmids))
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

        if (length(date_parts) > 0) {
          pub_date <- paste(
            date_parts,
            collapse = "-"
          )
        }
      }

      # ------------------------------------------------------------
      # Article IDs
      # ------------------------------------------------------------

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

          id_value <- safe_character(
            xml2::xml_text(node)
          )

          if (
            !is.na(id_type) &&
            nzchar(id_type) &&
            nzchar(id_value)
          ) {

            key <- tolower(
              as.character(id_type)
            )

            # VALID R SYNTAX:
            article_ids[[key]] <- id_value
          }
        }
      }

      results[[counter]] <- list(
        pmid = pmid,
        title = title,
        pub_date = pub_date,
        doi = safe_character(
          article_ids[["doi"]]
        ),
        pmcid = safe_character(
          article_ids[["pmc"]]
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

  # --------------------------------------------------------------------------
  # Extract first author from PubMed XML
  # --------------------------------------------------------------------------

  get_first_author <- function(article) {

    author_node <- xml2::xml_find_first(
      article,
      ".//AuthorList/Author[1]"
    )

    if (
      inherits(
        author_node,
        "xml_missing"
      )
    ) {
      return("")
    }

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

      return(
        safe_character(
          xml2::xml_text(last_name_node)
        )
      )
    }

    if (
      !inherits(
        collective_node,
        "xml_missing"
      )
    ) {

      return(
        safe_character(
          xml2::xml_text(collective_node)
        )
      )
    }

    ""
  }

  # --------------------------------------------------------------------------
  # PubMed title + author search
  # --------------------------------------------------------------------------

  search_pubmed_by_title_author <- function(
    title,
    first_author = ""
  ) {

    title_clean <- normalize_title(title)
    author_clean <- normalize_author(first_author)

    if (!nzchar(title_clean)) {
      return(NULL)
    }

    # ------------------------------------------------------------
    # Search 1: exact title
    # ------------------------------------------------------------

    exact_params <- list(
      db = "pubmed",
      term = paste0(
        "\"",
        title,
        "\"[Title]"
      ),
      retmode = "json",
      retmax = 50
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
      request_name = "PubMed exact title lookup"
    )

    ids <- character(0)

    if (!is.null(json)) {

      ids <- json$esearchresult$idlist

      if (!is.null(ids)) {
        ids <- as.character(
          unlist(ids)
        )
      }
    }

    if (length(ids) > 0) {

      records <- fetch_pubmed_records(ids)

      if (length(records) > 0) {

        best <- choose_best_pubmed_match(
          title,
          records,
          first_author
        )

        if (!is.null(best)) {
          return(best)
        }
      }
    }

    # ------------------------------------------------------------
    # Search 2: title words
    # ------------------------------------------------------------

    words <- strsplit(
      title_clean,
      "\\s+"
    )[[1]]

    words <- words[
      nchar(words) >= 3
    ]

    if (length(words) == 0) {
      return(NULL)
    }

    # Use distinctive title words.
    #
    # PubMed can perform poorly with an entire long title as a
    # Boolean query, so use the first 12 meaningful words.

    words <- words[
      seq_len(
        min(
          length(words),
          12
        )
      )
    ]

    title_query <- paste0(
      words,
      "[Title]",
      collapse = " AND "
    )

    broad_params <- list(
      db = "pubmed",
      term = title_query,
      retmode = "json",
      retmax = 100
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

    ids <- character(0)

    if (!is.null(json)) {

      ids <- json$esearchresult$idlist

      if (!is.null(ids)) {
        ids <- as.character(
          unlist(ids)
        )
      }
    }

    if (length(ids) == 0) {
      return(NULL)
    }

    records <- fetch_pubmed_records(ids)

    if (length(records) == 0) {
      return(NULL)
    }

    choose_best_pubmed_match(
      title,
      records,
      first_author
    )
  }

  # --------------------------------------------------------------------------
  # Choose best PubMed match
  # --------------------------------------------------------------------------

  choose_best_pubmed_match <- function(
    zotero_title,
    records,
    zotero_first_author = ""
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

    best_index <- which.max(
      title_scores
    )

    best_score <- title_scores[
      best_index
    ]

    best_record <- records[
      [best_index]
    ]

    # The line above must use [[ ]] in R.
    # Replace it explicitly here to avoid accidental malformed indexing.
    best_record <- records[[best_index]]

    if (best_score < 0.90) {
      return(NULL)
    }

    best_record$match_score <- best_score

    if (best_score >= 0.999) {
      best_record$match_type <- "exact_title"
    } else if (best_score >= 0.94) {
      best_record$match_type <- "strong_title"
    } else {
      best_record$match_type <- "title"
    }

    best_record
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

    first_author <- get_field(
      item,
      "creators"
    )

    # Zotero creators is normally a list, so obtain the first
    # creator's lastName when available.

    if (
      !is.null(data$creators) &&
      length(data$creators) > 0
    ) {

      creator <- data$creators[[1]]

      if (!is.null(creator$lastName)) {

        first_author <- safe_character(
          creator$lastName
        )
      }
    }

    if (
      !nzchar(pmid) ||
      !nzchar(pmcid)
    ) {

      todo[[length(todo) + 1]] <- list(
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
  # Helper to write identifiers
  # --------------------------------------------------------------------------

  process_found_identifiers <- function(
    entry,
    pmid,
    pmcid
  ) {

    item <- entry$item

    current_pmid <- get_field(
      item,
      "PMID"
    )

    current_pmcid <- get_field(
      item,
      "PMCID"
    )

    pmid <- safe_character(pmid)
    pmcid <- safe_character(pmcid)

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

      already_complete <<-
        already_complete + 1

      return(
        invisible(FALSE)
      )
    }

    message("")
    message(
      "FOUND: ",
      entry$title
    )

    if (add_pmid) {
      message(
        "  PMID  -> ",
        final_pmid
      )
    } else {
      message(
        "  PMID  -> already present"
      )
    }

    if (add_pmcid) {
      message(
        "  PMCID -> ",
        final_pmcid
      )
    } else {
      message(
        "  PMCID -> not identified"
      )
    }

    if (dry_run) {

      message(
        "  DRY RUN - Zotero not modified."
      )

      if (add_pmid) {
        pmids_added <<-
          pmids_added + 1
      }

      if (add_pmcid) {
        pmcids_added <<-
          pmcids_added + 1
      }

      if (
        add_pmid &&
        add_pmcid
      ) {
        both_added <<-
          both_added + 1
      }

      return(
        invisible(TRUE)
      )
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

      updated <<-
        updated + 1

      if (add_pmid) {
        pmids_added <<-
          pmids_added + 1
      }

      if (add_pmcid) {
        pmcids_added <<-
          pmcids_added + 1
      }

      if (
        add_pmid &&
        add_pmcid
      ) {
        both_added <<-
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

    invisible(TRUE)
  }

  # --------------------------------------------------------------------------
  # Process DOI articles
  # --------------------------------------------------------------------------

  doi_todo <- todo[
    vapply(
      todo,
      function(x) {
        nzchar(x$doi)
      },
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
        function(x) {
          x$doi
        },
        character(1)
      )

      message(
        "Checking PMC for ",
        length(dois),
        " DOIs..."
      )

      pmc_records <- get_pmc_records(
        dois
      )

      message(
        "PMC returned ",
        length(pmc_records),
        " records."
      )

      for (entry in batch) {

        doi <- entry$doi

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

        pmid <- ""
        pmcid <- ""

        record <- pmc_records[[doi]]

        if (!is.null(record)) {

          doi_lookups <-
            doi_lookups + 1

          pmid <- safe_character(
            record$pmid
          )

          pmcid <- safe_character(
            record$pmcid
          )
        }

        # ------------------------------------------------------------
        # PubMed DOI fallback
        # ------------------------------------------------------------

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

        # ------------------------------------------------------------
        # PubMed title + author fallback
        # ------------------------------------------------------------

        if (
          !nzchar(pmid) ||
          !nzchar(pmcid)
        ) {

          if (nzchar(entry$title)) {

            message(
              "PubMed title search: ",
              entry$title
            )

            record <- tryCatch(
              search_pubmed_by_title_author(
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

            if (!is.null(record)) {

              title_lookups <-
                title_lookups + 1

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

              if (nzchar(record$pmid)) {

                if (!nzchar(pmid)) {
                  pmid <- record$pmid
                }

                message(
                  "  PMID identified: ",
                  record$pmid
                )
              }

              if (nzchar(record$pmcid)) {

                if (!nzchar(pmcid)) {
                  pmcid <- record$pmcid
                }

                message(
                  "  PMCID identified: ",
                  record$pmcid
                )
              }

              if (!nzchar(record$pmcid)) {

                message(
                  "  PMCID identified: NO"
                )
              }

            } else {

              message(
                "  No reliable PubMed match."
              )
            }
          }
        }

        # ------------------------------------------------------------
        # Report result
        # ------------------------------------------------------------

        if (
          !nzchar(pmid) &&
          !nzchar(pmcid)
        ) {

          not_found <-
            not_found + 1

          message(
            "  RESULT: no PMID or PMCID identified."
          )

          next
        }

        process_found_identifiers(
          entry,
          pmid,
          pmcid
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
      function(x) {
        !nzchar(x$doi)
      },
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

      if (!nzchar(entry$title)) {

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
        entry$title
      )

      if (nzchar(entry$first_author)) {

        message(
          "  First author: ",
          entry$first_author
        )
      }

      record <- tryCatch(
        search_pubmed_by_title_author(
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

      pmid <- safe_character(
        record$pmid
      )

      pmcid <- safe_character(
        record$pmcid
      )

      if (nzchar(pmid)) {

        message(
          "  PMID identified: ",
          pmid
        )
      } else {

        message(
          "  PMID identified: NO"
        )
      }

      if (nzchar(pmcid)) {

        message(
          "  PMCID identified: ",
          pmcid
        )
      } else {

        message(
          "  PMCID identified: NO"
        )
      }

      if (
        !nzchar(pmid) &&
        !nzchar(pmcid)
      ) {

        message(
          "  PubMed match found, but no identifiers returned."
        )

        not_found <-
          not_found + 1

        next
      }

      process_found_identifiers(
        entry,
        pmid,
        pmcid
      )

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
    "DOI lookups:                  ",
    doi_lookups
  )

  message(
    "PubMed title lookups:         ",
    title_lookups
  )

  message(
    "PubMed fallbacks:             ",
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
