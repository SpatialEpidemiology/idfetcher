#' Fetch and write PMID and PMCID identifiers to Zotero
#'
#' Retrieves PMID and PMCID identifiers for Zotero journal articles using
#' the NCBI PMC ID Converter and PubMed APIs for compliance with NIH policy. Identifiers are written only
#' to Zotero's dedicated PMID and PMCID fields.
#'
#' @param user_id Zotero user/library ID.
#' @param api_key Zotero API key.
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
    user_id = NULL,
    api_key = NULL,
    dry_run = TRUE,
    batch_size = 50,
    pubmed_delay = 0.5,
    pmc_delay = 1.5,
    max_retries = 5,
    ncbi_api_key = ""
) {

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

  if (is.null(user_id) || !nzchar(as.character(user_id))) {
    stop(
      "You must provide your Zotero user_id.",
      call. = FALSE
    )
  }

  if (is.null(api_key) || !nzchar(as.character(api_key))) {
    stop(
      "You must provide your Zotero api_key.",
      call. = FALSE
    )
  }

  user_id <- as.character(user_id)
  api_key <- as.character(api_key)

  zotero_base <- "https://api.zotero.org"

  pmc_url <- paste0(
    "https://pmc.ncbi.nlm.nih.gov/",
    "tools/idconv/api/v1/articles/"
  )

  pubmed_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/",
    "entrez/eutils/esearch.fcgi"
  )

  user_agent <- "IDFetcher/1.0"

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

  `%||%` <- function(x, y) {

    if (is.null(x)) {
      return(y)
    }

    x
  }

  get_pubmed_id <- function(doi) {

    params <- list(
      db = "pubmed",
      term = paste0('"', doi, '"[DOI]'),
      retmode = "json"
    )

    if (
      !is.null(ncbi_api_key) &&
      nzchar(ncbi_api_key)
    ) {
      params$api_key <- ncbi_api_key
    }

    for (attempt in seq_len(max_retries)) {

      result <- tryCatch({

        req <- httr2::request(pubmed_url)

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
            httr2::resp_status_desc(response)
          )
        }

        json <- jsonlite::fromJSON(
          httr2::resp_body_string(response)
        )

        ids <- json$esearchresult$idlist

        if (
          is.null(ids) ||
          length(ids) == 0
        ) {
          return(NA_character_)
        }

        as.character(ids[[1]])

      }, error = function(e) {

        if (attempt < max_retries) {

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

    for (attempt in seq_len(max_retries)) {

      result <- tryCatch({

        req <- httr2::request(pmc_url)

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
            httr2::resp_status_desc(response)
          )
        }

        json <- jsonlite::fromJSON(
          httr2::resp_body_string(response),
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
            result[[record_doi]] <- record
          }
        }

        result

      }, error = function(e) {

        if (attempt < max_retries) {

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

      status <- httr2::resp_status(response)

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

  update_zotero_item <- function(item) {

    item_key <- item$key

    if (
      is.null(item_key) ||
      !nzchar(item_key)
    ) {
      stop("Zotero item has no key.")
    }

    url <- paste0(
      zotero_base,
      "/users/",
      user_id,
      "/items/",
      item_key
    )

    req <- httr2::request(url)

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

    response <- httr2::req_perform(req)

    status <- httr2::resp_status(response)

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

  todo <- list()

  for (item in items) {

    data <- item$data

    doi <- normalize_doi(
      data$DOI %||% ""
    )

    if (!nzchar(doi)) {
      next
    }

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
      todo[[length(todo) + 1]] <- item
    }
  }

  message(
    "Articles missing PMID or PMCID: ",
    length(todo)
  )

  if (length(todo) == 0) {

    message("Nothing needs updating.")

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
          pubmed_fallbacks = 0,
          not_found = 0,
          errors = 0,
          dry_run = dry_run
        )
      )
    )
  }

  updated <- 0
  pmids_added <- 0
  pmcids_added <- 0
  both_added <- 0
  already_complete <- 0
  pubmed_fallbacks <- 0
  not_found <- 0
  errors <- 0

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

    doi_map <- list()

    for (item in batch) {

      doi <- normalize_doi(
        item$data$DOI %||% ""
      )

      if (nzchar(doi)) {
        doi_map[[doi]] <- item
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

      item <- doi_map[[doi]]

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

        message(
          "Already complete: ",
          doi
        )

        next
      }

      record <- pmc_records[[doi]]

      pmid <- NULL
      pmcid <- NULL

      if (!is.null(record)) {

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

      if (
        is.null(pmcid) ||
        !nzchar(pmcid)
      ) {

        message(
          "No PMCID, checking PubMed: ",
          doi
        )

        if (
          is.null(pmid) ||
          !nzchar(pmid)
        ) {

          pmid <- get_pubmed_id(doi)

          if (
            !is.na(pmid) &&
            nzchar(pmid)
          ) {

            pubmed_fallbacks <-
              pubmed_fallbacks + 1

            message(
              "  PubMed PMID: ",
              pmid
            )

          } else {

            pmid <- NULL

            message(
              "  No PMID found."
            )
          }

        } else {

          message(
            "  PMC supplied PMID: ",
            pmid
          )
        }

        Sys.sleep(pubmed_delay)

      } else if (
        is.null(pmid) ||
        !nzchar(pmid)
      ) {

        message(
          "PMCID found but PMID missing: ",
          doi
        )

        pmid <- get_pubmed_id(doi)

        if (
          !is.na(pmid) &&
          nzchar(pmid)
        ) {

          pubmed_fallbacks <-
            pubmed_fallbacks + 1

          message(
            "  PubMed PMID: ",
            pmid
          )

        } else {

          pmid <- NULL

          message(
            "  No PMID found."
          )
        }

        Sys.sleep(pubmed_delay)
      }

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
          pmids_added <- pmids_added + 1
        }

        if (add_pmcid) {
          pmcids_added <- pmcids_added + 1
        }

        if (
          add_pmid &&
          add_pmcid
        ) {
          both_added <- both_added + 1
        }

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

        update_zotero_item(item)

        updated <- updated + 1

        if (add_pmid) {
          pmids_added <- pmids_added + 1
        }

        if (add_pmcid) {
          pmcids_added <- pmcids_added + 1
        }

        if (
          add_pmid &&
          add_pmcid
        ) {
          both_added <- both_added + 1
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

    Sys.sleep(pmc_delay)
  }

  message("")
  message("")
  message(
    "======================================================================"
  )
  message("FINAL REPORT")
  message(
    "======================================================================"
  )

  message(
    "Total journal articles:   ",
    length(items)
  )

  message(
    "Articles requiring lookup: ",
    length(todo)
  )

  message(
    "Zotero items updated:      ",
    updated
  )

  message(
    "PMIDs added:               ",
    pmids_added
  )

  message(
    "PMCIDs added:              ",
    pmcids_added
  )

  message(
    "Both PMID + PMCID added:   ",
    both_added
  )

  message(
    "Already complete:          ",
    already_complete
  )

  message(
    "PubMed fallbacks:          ",
    pubmed_fallbacks
  )

  message(
    "No identifier found:       ",
    not_found
  )

  message(
    "Errors:                    ",
    errors
  )

  message(
    "======================================================================"
  )

  if (dry_run) {

    message("")
    message("DRY RUN COMPLETE.")
    message("NOTHING WAS WRITTEN TO ZOTERO.")

  } else {

    message("")
    message("UPDATE COMPLETE.")
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
      pubmed_fallbacks = pubmed_fallbacks,
      not_found = not_found,
      errors = errors,
      dry_run = dry_run
    )
  )
}
