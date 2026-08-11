#' Fetch and write PMID and PMCID identifiers to Zotero
#'
#' Retrieves PMID and PMCID identifiers for Zotero journal articles
#' using the NCBI PMC ID Converter and PubMed APIs.
#'
#' Lookup order:
#'   1. PMC ID Converter using DOI
#'   2. PubMed using DOI
#'   3. PubMed using title + first author + year
#'
#' Identifiers are written only to Zotero's dedicated PMID and PMCID fields.
#'
#' @param dry_run Logical. If TRUE, lookups are performed but Zotero is not
#' modified. Default is TRUE.
#' @param batch_size Number of DOIs sent to PMC in each batch. Default is 50.
#' @param pubmed_delay Seconds to wait between PubMed requests. Default is 0.5.
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
    pubmed_delay = 0.5,
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

  # --------------------------------------------------------------------------
  # Credentials
  # --------------------------------------------------------------------------

  credentials <- idfetcher_get_credentials()

  user_id <- credentials$user_id
  api_key <- credentials$api_key

  if (
    is.null(user_id) ||
    !nzchar(as.character(user_id))
  ) {
    stop(
      "No Zotero user_id found in stored credentials.",
      call. = FALSE
    )
  }

  if (
    is.null(api_key) ||
    !nzchar(as.character(api_key))
  ) {
    stop(
      "No Zotero api_key found in stored credentials.",
      call. = FALSE
    )
  }

  user_id <- as.character(user_id)
  api_key <- as.character(api_key)

  # --------------------------------------------------------------------------
  # URLs
  # --------------------------------------------------------------------------

  zotero_base <- "https://api.zotero.org"

  pmc_url <- paste0(
    "https://pmc.ncbi.nlm.nih.gov/",
    "tools/idconv/api/v1/articles/"
  )

  pubmed_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/",
    "entrez/eutils/esearch.fcgi"
  )

  pubmed_summary_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/",
    "entrez/eutils/esummary.fcgi"
  )

  user_agent <- "IDFetcher/1.1"

  # --------------------------------------------------------------------------
  # Utility functions
  # --------------------------------------------------------------------------

  `%||%` <- function(x, y) {
    if (is.null(x)) {
      return(y)
    }

    x
  }

  normalize_doi <- function(doi) {

    if (
      is.null(doi) ||
      length(doi) == 0
    ) {
      return("")
    }

    doi <- as.character(doi)[1]

    if (is.na(doi)) {
      return("")
    }

    doi <- trimws(
      tolower(doi)
    )

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
      length(title) == 0
    ) {
      return("")
    }

    title <- as.character(title)[1]

    if (is.na(title)) {
      return("")
    }

    title <- tolower(title)

    title <- gsub(
      "[[:punct:]]+",
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

  get_year <- function(item) {

    date_value <- get_field(
      item,
      "date"
    )

    if (!nzchar(date_value)) {
      return("")
    }

    match <- regmatches(
      date_value,
      regexpr(
        "[0-9]{4}",
        date_value
      )
    )

    if (
      length(match) == 0 ||
      is.na(match)
    ) {
      return("")
    }

    match
  }

  get_first_author <- function(item) {

    creators <- item$data$creators

    if (
      is.null(creators) ||
      length(creators) == 0
    ) {
      return("")
    }

    creator <- creators[[1]]

    if (
      !is.null(creator$lastName) &&
      nzchar(
        as.character(
          creator$lastName
        )
      )
    ) {
      return(
        as.character(
          creator$lastName
        )
      )
    }

    if (
      !is.null(creator$name) &&
      nzchar(
        as.character(
          creator$name
        )
      )
    ) {
      return(
        as.character(
          creator$name
        )
      )
    }

    ""
  }

  make_ncbi_params <- function(params) {

    if (
      !is.null(ncbi_api_key) &&
      nzchar(ncbi_api_key)
    ) {
      params$api_key <- ncbi_api_key
    }

    params
  }

  # --------------------------------------------------------------------------
  # PubMed DOI lookup
  # --------------------------------------------------------------------------

  get_pubmed_id <- function(doi) {

    doi <- normalize_doi(doi)

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
      retmode = "json"
    )

    params <- make_ncbi_params(
      params
    )

    for (
      attempt in seq_len(max_retries)
    ) {

      result <- tryCatch({

        req <- httr2::request(
          pubmed_url
        )

        req <- httr2::req_headers(
          req,
          `User-Agent` = user_agent
        )

        req <- httr2::req_url_query(
          req,
          !!!params
        )

        response <- httr2::req_perform(
          req
        )

        status <- httr2::resp_status(
          response
        )

        if (status == 429) {

          wait_time <- 2^(attempt - 1)

          message(
            "PubMed rate limit (429). Waiting ",
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
            httr2::resp_status_desc(
              response
            )
          )
        }

        json <- jsonlite::fromJSON(
          httr2::resp_body_string(
            response
          )
        )

        ids <- json$esearchresult$idlist

        if (
          is.null(ids) ||
          length(ids) == 0
        ) {
          return(NA_character_)
        }

        as.character(
          ids[[1]]
        )

      }, error = function(e) {

        if (
          attempt < max_retries
        ) {

          wait_time <- 2^(attempt - 1)

          message(
            "PubMed error: ",
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
          "PubMed lookup failed: ",
          conditionMessage(e)
        )

        NA_character_
      })

      if (!is.null(result)) {
        return(result)
      }
    }

    NA_character_
  }

  # --------------------------------------------------------------------------
  # PubMed metadata fallback
  # --------------------------------------------------------------------------

  get_pubmed_id_by_metadata <- function(
    title,
    first_author = "",
    year = ""
  ) {

    title <- trimws(title)

    if (!nzchar(title)) {
      return(NA_character_)
    }

    title_query <- paste0(
      '"',
      gsub(
        '"',
        "",
        title,
        fixed = TRUE
      ),
      '"[Title]'
    )

    search_terms <- title_query

    if (nzchar(first_author)) {

      author_clean <- gsub(
        "[^A-Za-z0-9 -]",
        "",
        first_author
      )

      if (nzchar(author_clean)) {

        search_terms <- paste0(
          search_terms,
          " AND ",
          author_clean,
          "[Author]"
        )
      }
    }

    if (
      nzchar(year) &&
      grepl(
        "^[0-9]{4}$",
        year
      )
    ) {

      search_terms <- paste0(
        search_terms,
        " AND ",
        year,
        "[pdat]"
      )
    }

    params <- list(
      db = "pubmed",
      term = search_terms,
      retmode = "json",
      retmax = 10
    )

    params <- make_ncbi_params(
      params
    )

    for (
      attempt in seq_len(max_retries)
    ) {

      result <- tryCatch({

        req <- httr2::request(
          pubmed_url
        )

        req <- httr2::req_headers(
          req,
          `User-Agent` = user_agent
        )

        req <- httr2::req_url_query(
          req,
          !!!params
        )

        response <- httr2::req_perform(
          req
        )

        status <- httr2::resp_status(
          response
        )

        if (status == 429) {

          wait_time <- 2^(attempt - 1)

          message(
            "PubMed metadata rate limit (429). Waiting ",
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
            httr2::resp_status_desc(
              response
            )
          )
        }

        json <- jsonlite::fromJSON(
          httr2::resp_body_string(
            response
          )
        )

        ids <- json$esearchresult$idlist

        if (
          is.null(ids) ||
          length(ids) == 0
        ) {
          return(NA_character_)
        }

        ids <- as.character(ids)

        if (length(ids) == 1) {
          return(ids[[1]])
        }

        summary_params <- list(
          db = "pubmed",
          id = paste(
            ids,
            collapse = ","
          ),
          retmode = "json"
        )

        summary_params <- make_ncbi_params(
          summary_params
        )

        summary_req <- httr2::request(
          pubmed_summary_url
        )

        summary_req <- httr2::req_headers(
          summary_req,
          `User-Agent` = user_agent
        )

        summary_req <- httr2::req_url_query(
          summary_req,
          !!!summary_params
        )

        summary_response <-
          httr2::req_perform(
            summary_req
          )

        if (
          httr2::resp_status(
            summary_response
          ) >= 400
        ) {
          return(ids[[1]])
        }

        summary_json <- jsonlite::fromJSON(
          httr2::resp_body_string(
            summary_response
          ),
          simplifyVector = FALSE
        )

        result_data <- summary_json$result

        target_title <- normalize_title(
          title
        )

        scores <- numeric(
          length(ids)
        )

        for (
          i in seq_along(ids)
        ) {

          candidate <-
            result_data[[ids[[i]]]]

          if (is.null(candidate)) {
            next
          }

          candidate_title <-
            normalize_title(
              candidate$title %||% ""
            )

          if (!nzchar(candidate_title)) {
            next
          }

          if (
            identical(
              candidate_title,
              target_title
            )
          ) {

            scores[i] <- 1

          } else {

            target_words <- unique(
              unlist(
                strsplit(
                  target_title,
                  "\\s+"
                )
              )
            )

            candidate_words <- unique(
              unlist(
                strsplit(
                  candidate_title,
                  "\\s+"
                )
              )
            )

            if (
              length(target_words) > 0 &&
              length(candidate_words) > 0
            ) {

              overlap <- sum(
                target_words %in%
                  candidate_words
              )

              scores[i] <-
                overlap /
                length(target_words)
            }
          }
        }

        best <- which.max(
          scores
        )

        if (
          length(best) == 0 ||
          scores[best] < 0.80
        ) {
          return(
            NA_character_
          )
        }

        ids[[best]]

      }, error = function(e) {

        if (
          attempt < max_retries
        ) {

          wait_time <- 2^(attempt - 1)

          message(
            "PubMed metadata error: ",
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
          "PubMed metadata lookup failed: ",
          conditionMessage(e)
        )

        NA_character_
      })

      if (!is.null(result)) {
        return(result)
      }
    }

    NA_character_
  }

  # --------------------------------------------------------------------------
  # PMC ID Converter
  # --------------------------------------------------------------------------

  get_pmc_records <- function(dois) {

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

    params <- make_ncbi_params(
      params
    )

    for (
      attempt in seq_len(max_retries)
    ) {

      result <- tryCatch({

        req <- httr2::request(
          pmc_url
        )

        req <- httr2::req_headers(
          req,
          `User-Agent` = user_agent
        )

        req <- httr2::req_url_query(
          req,
          !!!params
        )

        response <- httr2::req_perform(
          req
        )

        status <- httr2::resp_status(
          response
        )

        if (status == 429) {

          wait_time <- 2^(attempt - 1)

          message(
            "PMC rate limit (429). Waiting ",
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
            httr2::resp_status_desc(
              response
            )
          )
        }

        json <- jsonlite::fromJSON(
          httr2::resp_body_string(
            response
          ),
          simplifyVector = FALSE
        )

        records <- json$records

        if (is.null(records)) {
          return(list())
        }

        result <- list()

        for (record in records) {

          record_doi <- normalize_doi(
            record$doi %||%
              record$requested_id %||%
              record[["requested-id"]]
          )

          if (nzchar(record_doi)) {

            result[[record_doi]] <-
              record
          }
        }

        result

      }, error = function(e) {

        if (
          attempt < max_retries
        ) {

          wait_time <- 2^(attempt - 1)

          message(
            "PMC error: ",
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
          "PMC lookup failed: ",
          conditionMessage(e)
        )

        list()
      })

      if (!is.null(result)) {
        return(result)
      }
    }

    list()
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
          httr2::resp_status_desc(
            response
          )
        )
      }

      items <- jsonlite::fromJSON(
        httr2::resp_body_string(
          response
        ),
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

    # Explicitly use PUT for a full-item update.
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
        httr2::resp_status_desc(
          response
        )
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
  message("IDFETCHER")
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
  # Find articles requiring lookup
  # --------------------------------------------------------------------------

  todo <- list()

  for (item in items) {

    pmid <- get_field(
      item,
      "PMID"
    )

    pmcid <- get_field(
      item,
      "PMCID"
    )

    if (
      !nzchar(pmid) ||
      !nzchar(pmcid)
    ) {

      todo[[length(todo) + 1]] <-
        item
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
          pubmed_doi_lookups = 0,
          pubmed_metadata_fallbacks = 0,
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
  pubmed_doi_lookups <- 0
  pubmed_metadata_fallbacks <- 0
  not_found <- 0
  errors <- 0

  # --------------------------------------------------------------------------
  # Process batches
  # --------------------------------------------------------------------------

  for (
    start in seq(
      1,
      length(todo),
      by = batch_size
    )
  ) {

    end <- min(
      start + batch_size - 1,
      length(todo)
    )

    batch <- todo[start:end]

    message("")
    message(
      "======================================================================"
    )

    message(
      "Processing ",
      start,
      "-",
      end,
      " of ",
      length(todo)
    )

    message(
      "======================================================================"
    )

    # ------------------------------------------------------------------------
    # Build DOI map
    # ------------------------------------------------------------------------

    doi_map <- list()

    for (item in batch) {

      doi <- normalize_doi(
        item$data$DOI %||% ""
      )

      if (nzchar(doi)) {

        doi_map[[doi]] <- item
      }
    }

    # ------------------------------------------------------------------------
    # PMC lookup
    # ------------------------------------------------------------------------

    pmc_records <- list()

    if (length(doi_map) > 0) {

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

          errors <<- errors +
            length(doi_map)

          list()
        }
      )

      message(
        "PMC returned ",
        length(pmc_records),
        " records."
      )
    }

    # ------------------------------------------------------------------------
    # Process each item
    # ------------------------------------------------------------------------

    for (item in batch) {

      current_pmid <- get_field(
        item,
        "PMID"
      )

      current_pmcid <- get_field(
        item,
        "PMCID"
      )

      doi <- normalize_doi(
        item$data$DOI %||% ""
      )

      title <- get_field(
        item,
        "title"
      )

      first_author <- get_first_author(
        item
      )

      year <- get_year(
        item
      )

      # ----------------------------------------------------------------------
      # Already complete
      # ----------------------------------------------------------------------

      if (
        nzchar(current_pmid) &&
        nzchar(current_pmcid)
      ) {

        already_complete <-
          already_complete + 1

        message(
          "Already complete: ",
          ifelse(
            nzchar(doi),
            doi,
            title
          )
        )

        next
      }

      pmid <- NULL
      pmcid <- NULL

      # ----------------------------------------------------------------------
      # PMC result
      # ----------------------------------------------------------------------

      if (nzchar(doi)) {

        record <- pmc_records[[doi]]

        if (!is.null(record)) {

          pmid <-
            record$pmid %||% NULL

          pmcid <-
            record$pmcid %||% NULL

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
      }

      # ----------------------------------------------------------------------
      # PubMed DOI fallback
      # ----------------------------------------------------------------------

      if (
        is.null(pmid) ||
        !nzchar(pmid)
      ) {

        if (nzchar(doi)) {

          message(
            "PubMed DOI lookup: ",
            doi
          )

          candidate_pmid <-
            get_pubmed_id(
              doi
            )

          if (
            !is.na(candidate_pmid) &&
            nzchar(candidate_pmid)
          ) {

            pmid <- candidate_pmid

            pubmed_doi_lookups <-
              pubmed_doi_lookups + 1

            message(
              "  PMID: ",
              pmid
            )
          }
        }
      }

      Sys.sleep(
        pubmed_delay
      )

      # ----------------------------------------------------------------------
      # PubMed metadata fallback
      # ----------------------------------------------------------------------

      if (
        is.null(pmid) ||
        !nzchar(pmid)
      ) {

        if (nzchar(title)) {

          message(
            "PubMed metadata fallback: ",
            title
          )

          candidate_pmid <-
            get_pubmed_id_by_metadata(
              title = title,
              first_author = first_author,
              year = year
            )

          if (
            !is.na(candidate_pmid) &&
            nzchar(candidate_pmid)
          ) {

            pmid <- candidate_pmid

            pubmed_metadata_fallbacks <-
              pubmed_metadata_fallbacks + 1

            message(
              "  PMID: ",
              pmid
            )

          } else {

            message(
              "  No reliable PubMed match."
            )
          }
        }
      }

      Sys.sleep(
        pubmed_delay
      )

      # ----------------------------------------------------------------------
      # Preserve existing values
      # ----------------------------------------------------------------------

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

      # ----------------------------------------------------------------------
      # Nothing found
      # ----------------------------------------------------------------------

      if (
        !nzchar(final_pmid) &&
        !nzchar(final_pmcid)
      ) {

        not_found <- not_found + 1

        message(
          "No PMID or PMCID: ",
          ifelse(
            nzchar(doi),
            doi,
            title
          )
        )

        next
      }

      # ----------------------------------------------------------------------
      # Determine what needs to be added
      # ----------------------------------------------------------------------

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
        ifelse(
          nzchar(doi),
          doi,
          title
        )
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

      # ----------------------------------------------------------------------
      # Dry run
      # ----------------------------------------------------------------------

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

      # ----------------------------------------------------------------------
      # Update Zotero
      # ----------------------------------------------------------------------

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
      pmc_delay
    )
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
    "PubMed DOI lookups:            ",
    pubmed_doi_lookups
  )

  message(
    "PubMed metadata fallbacks:     ",
    pubmed_metadata_fallbacks
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
      articles_requiring_lookup =
        length(todo),
      updated = updated,
      pmids_added = pmids_added,
      pmcids_added = pmcids_added,
      both_added = both_added,
      already_complete = already_complete,
      pubmed_doi_lookups =
        pubmed_doi_lookups,
      pubmed_metadata_fallbacks =
        pubmed_metadata_fallbacks,
      not_found = not_found,
      errors = errors,
      dry_run = dry_run
    )
  )
}
