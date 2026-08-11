#' Fetch and write PMID and PMCID identifiers to Zotero
#'
#' Retrieves PMID and PMCID identifiers for Zotero journal articles using
#' the NCBI PMC ID Converter and PubMed APIs. Articles are matched using
#' DOI when available, with PubMed title/author metadata as a fallback.
#'
#' @param dry_run Logical. If TRUE, lookups are performed but Zotero is not
#' modified. Default is TRUE.
#' @param batch_size Number of DOI/PMID identifiers sent to PMC in each batch.
#' Default is 50.
#' @param pubmed_delay Seconds to wait between PubMed requests. Default is 0.5.
#' @param pmc_delay Seconds to wait between PMC batches. Default is 1.5.
#' @param max_retries Maximum number of retries after an API error or HTTP 429.
#' Default is 5.
#' @param ncbi_api_key Optional NCBI API key.
#' @return Invisibly returns a list containing the run summary.
#' @export
idfetcher <- function(
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

  credentials <- idfetcher_get_credentials()

  user_id <- credentials$user_id
  api_key <- credentials$api_key

  user_id <- as.character(user_id)
  api_key <- as.character(api_key)

  zotero_base <- "https://api.zotero.org"

  pmc_url <- paste0(
    "https://pmc.ncbi.nlm.nih.gov/",
    "tools/idconv/api/v1/articles/"
  )

  pubmed_esearch_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/",
    "entrez/eutils/esearch.fcgi"
  )

  pubmed_esummary_url <- paste0(
    "https://eutils.ncbi.nlm.nih.gov/",
    "entrez/eutils/esummary.fcgi"
  )

  user_agent <- "IDFetcher/1.0"

  `%||%` <- function(x, y) {
    if (is.null(x)) {
      return(y)
    }
    x
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

    doi <- sub(
      "[[:punct:]]+$",
      "",
      doi
    )

    doi
  }

  normalize_text <- function(x) {

    if (is.null(x) || length(x) == 0 || is.na(x)) {
      return("")
    }

    x <- as.character(x)[1]

    if (is.na(x)) {
      return("")
    }

    x <- tolower(x)

    x <- gsub(
      "&",
      " and ",
      x,
      fixed = TRUE
    )

    x <- gsub(
      "[^a-z0-9]+",
      " ",
      x
    )

    x <- gsub(
      "\\s+",
      " ",
      x
    )

    trimws(x)
  }

  title_tokens <- function(x) {

    x <- normalize_text(x)

    if (!nzchar(x)) {
      return(character(0))
    }

    tokens <- strsplit(
      x,
      " ",
      fixed = TRUE
    )[[1]]

    stopwords <- c(
      "a",
      "an",
      "and",
      "are",
      "as",
      "at",
      "by",
      "for",
      "from",
      "in",
      "into",
      "is",
      "of",
      "on",
      "or",
      "the",
      "to",
      "using",
      "with"
    )

    tokens[
      nchar(tokens) >= 2 &
        !tokens %in% stopwords
    ]
  }

  title_similarity <- function(title_a, title_b) {

    a <- title_tokens(title_a)
    b <- title_tokens(title_b)

    if (
      length(a) == 0 ||
      length(b) == 0
    ) {
      return(0)
    }

    a_unique <- unique(a)
    b_unique <- unique(b)

    intersection <- intersect(
      a_unique,
      b_unique
    )

    union <- union(
      a_unique,
      b_unique
    )

    if (length(union) == 0) {
      return(0)
    }

    jaccard <- length(intersection) / length(union)

    coverage_a <-
      length(intersection) / length(a_unique)

    coverage_b <-
      length(intersection) / length(b_unique)

    max(
      jaccard,
      min(coverage_a, coverage_b)
    )
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

  get_first_author <- function(item) {

    creators <- item$data$creators

    if (
      is.null(creators) ||
      length(creators) == 0
    ) {
      return("")
    }

    creator <- creators[[1]]

    if (is.null(creator)) {
      return("")
    }

    if (
      !is.null(creator$lastName) &&
      nzchar(as.character(creator$lastName))
    ) {
      return(
        trimws(as.character(creator$lastName))
      )
    }

    if (
      !is.null(creator$name) &&
      nzchar(as.character(creator$name))
    ) {
      return(
        trimws(as.character(creator$name))
      )
    }

    ""
  }

  ncbi_query <- function(
    url,
    params
  ) {

    if (
      !is.null(ncbi_api_key) &&
      nzchar(ncbi_api_key)
    ) {
      params$api_key <- ncbi_api_key
    }

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

          wait_time <- 2^(attempt - 1)

          message(
            "NCBI rate limit (429). Waiting ",
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

        jsonlite::fromJSON(
          httr2::resp_body_string(response),
          simplifyVector = FALSE
        )

      }, error = function(e) {

        if (attempt < max_retries) {

          wait_time <- 2^(attempt - 1)

          message(
            "NCBI error: ",
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
          "NCBI lookup failed: ",
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
      retmax = 5
    )

    json <- ncbi_query(
      pubmed_esearch_url,
      params
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

  get_pubmed_summary <- function(pmids) {

    if (
      is.null(pmids) ||
      length(pmids) == 0
    ) {
      return(list())
    }

    pmids <- as.character(pmids)

    params <- list(
      db = "pubmed",
      id = paste(
        pmids,
        collapse = ","
      ),
      retmode = "json"
    )

    json <- ncbi_query(
      pubmed_esummary_url,
      params
    )

    if (is.null(json)) {
      return(list())
    }

    result <- json$result

    if (is.null(result)) {
      return(list())
    }

    result
  }

  get_pubmed_id_by_metadata <- function(
    title,
    author = "",
    year = "",
    doi = ""
  ) {

    if (!nzchar(title)) {
      return(
        list(
          pmid = NA_character_,
          title = "",
          score = 0
        )
      )
    }

    original_title <- title

    title_clean <- normalize_text(title)

    if (!nzchar(title_clean)) {
      return(
        list(
          pmid = NA_character_,
          title = "",
          score = 0
        )
      )
    }

    queries <- character(0)

    queries <- c(
      queries,
      paste0(
        '"',
        title_clean,
        '"[Title]'
      )
    )

    queries <- c(
      queries,
      paste0(
        '"',
        title_clean,
        '"'
      )
    )

    title_words <- title_tokens(title_clean)

    if (length(title_words) >= 4) {

      core_words <- title_words[
        seq_len(
          min(
            length(title_words),
            12
          )
        )
      ]

      queries <- c(
        queries,
        paste(
          core_words,
          collapse = " "
        )
      )
    }

    if (
      nzchar(author) &&
      length(title_words) >= 3
    ) {

      author_clean <- normalize_text(author)

      queries <- c(
        queries,
        paste0(
          '"',
          paste(
            title_words[
              seq_len(
                min(
                  length(title_words),
                  8
                )
            ]
            ],
collapse = " "
              ),
'" AND ',
author_clean,
'[Author]'
          )
        )
    }

    if (
      nzchar(doi)
    ) {

      doi_id <- get_pubmed_id_by_doi(doi)

      if (
        !is.na(doi_id) &&
        nzchar(doi_id)
      ) {

        summary <- get_pubmed_summary(
          doi_id
        )

        record <- summary[[doi_id]]

        if (!is.null(record)) {

          return(
            list(
              pmid = as.character(doi_id),
              title = record$title %||% "",
              score = 1
            )
          )
        }
      }
    }

    candidate_pmids <- character(0)

    for (query in unique(queries)) {

      params <- list(
        db = "pubmed",
        term = query,
        retmode = "json",
        retmax = 20
      )

      if (
        nzchar(year) &&
        grepl(
          "^[0-9]{4}$",
          year
        )
      ) {

        params$term <- paste0(
          "(",
          query,
          ") AND ",
          year,
          "[dp]"
        )
      }

      json <- ncbi_query(
        pubmed_esearch_url,
        params
      )

      if (!is.null(json)) {

        ids <- json$esearchresult$idlist

        if (
          !is.null(ids) &&
          length(ids) > 0
        ) {

          candidate_pmids <- c(
            candidate_pmids,
            as.character(ids)
          )
        }
      }

      Sys.sleep(pubmed_delay)
    }

    candidate_pmids <- unique(
      candidate_pmids
    )

    if (length(candidate_pmids) == 0) {

      return(
        list(
          pmid = NA_character_,
          title = "",
          score = 0
        )
      )
    }

    summaries <- get_pubmed_summary(
      candidate_pmids
    )

    if (
      length(summaries) == 0
    ) {

      return(
        list(
          pmid = NA_character_,
          title = "",
          score = 0
        )
      )
    }

    candidates <- list()

    for (pmid in candidate_pmids) {

      record <- summaries[[pmid]]

      if (is.null(record)) {
        next
      }

      pubmed_title <- record$title %||% ""

      score <- title_similarity(
        original_title,
        pubmed_title
      )

      candidate_author_score <- 0

      if (
        nzchar(author) &&
        !is.null(record$authors)
      ) {

        pubmed_authors <- vapply(
          record$authors,
          function(x) {

            if (
              !is.null(x$name)
            ) {
              return(
                normalize_text(x$name)
              )
            }

            ""
          },
          character(1)
        )

        author_clean <- normalize_text(
          author
        )

        if (
          nzchar(author_clean) &&
          length(pubmed_authors) > 0
        ) {

          candidate_author_score <-
            as.numeric(
              any(
                grepl(
                  author_clean,
                  pubmed_authors,
                  fixed = TRUE
                ) |
                  grepl(
                    pubmed_authors,
                    author_clean,
                    fixed = TRUE
                  )
              )
            )
        }
      }

      final_score <-
        score +
        (0.10 * candidate_author_score)

      candidates[[length(candidates) + 1]] <- list(
        pmid = as.character(pmid),
        title = pubmed_title,
        score = final_score
      )
    }

    if (length(candidates) == 0) {

      return(
        list(
          pmid = NA_character_,
          title = "",
          score = 0
        )
      )
    }

    scores <- vapply(
      candidates,
      function(x) x$score,
      numeric(1)
    )

    best_index <- which.max(scores)

    best <- candidates[[best_index]]

    # Require a strong title match.
    # Exact/near-exact PubMed title matches generally score >= 0.85.
    if (best$score < 0.70) {

      return(
        list(
          pmid = NA_character_,
          title = best$title,
          score = best$score
        )
      )
    }

    best
  }

  get_pmc_records <- function(ids, idtype) {

    if (
      is.null(ids) ||
      length(ids) == 0
    ) {
      return(list())
    }

    params <- list(
      ids = paste(
        ids,
        collapse = ","
      ),
      idtype = idtype,
      format = "json",
      tool = "idfetcher"
    )

    json <- ncbi_query(
      pmc_url,
      params
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

      key <- record$requested_id %||%
        record[["requested-id"]] %||%
        record$doi %||%
        record$pmid %||%
        ""

      if (
        !is.null(key) &&
        nzchar(as.character(key))
      ) {

        key <- as.character(key)

        if (
          idtype == "doi"
        ) {
          key <- normalize_doi(key)
        }

        result[[key]] <- record
      }
    }

    result
  }

  get_pmc_record_by_pmid <- function(pmid) {

    if (
      is.null(pmid) ||
      is.na(pmid) ||
      !nzchar(as.character(pmid))
    ) {
      return(NULL)
    }

    records <- get_pmc_records(
      as.character(pmid),
      "pmid"
    )

    record <- records[[as.character(pmid)]]

    if (!is.null(record)) {
      return(record)
    }

    if (length(records) > 0) {
      return(records[[1]])
    }

    NULL
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

    response <- httr2::req_perform(req)

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
          metadata_fallbacks = 0,
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
  doi_lookups <- 0
  metadata_fallbacks <- 0
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

    pmc_records <- list()

    if (length(doi_map) > 0) {

      message(
        "Checking PMC by DOI for ",
        length(doi_map),
        " articles..."
      )

      pmc_records <- get_pmc_records(
        names(doi_map),
        "doi"
      )

      message(
        "PMC returned ",
        length(pmc_records),
        " DOI records."
      )
    }

    for (item in batch) {

      data <- item$data

      doi <- normalize_doi(
        data$DOI %||% ""
      )

      title <- get_field(
        item,
        "title"
      )

      author <- get_first_author(
        item
      )

      year <- get_field(
        item,
        "date"
      )

      year_match <- regmatches(
        year,
        regexpr(
          "[0-9]{4}",
          year
        )
      )

      if (
        length(year_match) == 0 ||
        is.na(year_match)
      ) {
        year_match <- ""
      }

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
          title
        )

        next
      }

      pmid <- NULL
      pmcid <- NULL

      # ---------------------------------------------------------------
      # 1. DOI -> PMC ID Converter
      # ---------------------------------------------------------------

      if (
        nzchar(doi)
      ) {

        record <- pmc_records[[doi]]

        if (!is.null(record)) {

          doi_lookups <- doi_lookups + 1

          pmid <- record$pmid %||%
            NULL

          pmcid <- record$pmcid %||%
            NULL

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

      # ---------------------------------------------------------------
      # 2. DOI -> PubMed
      # ---------------------------------------------------------------

      if (
        is.null(pmid) ||
        !nzchar(pmid)
      ) {

        if (nzchar(doi)) {

          message(
            "PubMed DOI lookup: ",
            doi
          )

          doi_pmid <- get_pubmed_id_by_doi(
            doi
          )

          if (
            !is.na(doi_pmid) &&
            nzchar(doi_pmid)
          ) {

            pmid <- doi_pmid

            pubmed_fallbacks <-
              pubmed_fallbacks + 1

            message(
              "  PMID -> ",
              pmid
            )
          }
        }
      }

      # ---------------------------------------------------------------
      # 3. PubMed PMID -> PMC
      # ---------------------------------------------------------------

      if (
        !is.null(pmid) &&
        !is.na(pmid) &&
        nzchar(pmid) &&
        (
          is.null(pmcid) ||
          !nzchar(pmcid)
        )
      ) {

        message(
          "Checking PMC by PMID: ",
          pmid
        )

        pmc_record <- get_pmc_record_by_pmid(
          pmid
        )

        if (!is.null(pmc_record)) {

          pmcid <- pmc_record$pmcid %||%
            NULL

          if (!is.null(pmcid)) {

            pmcid <- trimws(
              as.character(pmcid)
            )

            if (nzchar(pmcid)) {

              message(
                "  PMCID -> ",
                pmcid
              )
            }
          }
        }
      }

      # ---------------------------------------------------------------
      # 4. No DOI or DOI failed:
      #    PubMed metadata search
      # ---------------------------------------------------------------

      if (
        is.null(pmid) ||
        is.na(pmid) ||
        !nzchar(pmid)
      ) {

        message(
          "PubMed metadata fallback: ",
          title
        )

        metadata_result <-
          get_pubmed_id_by_metadata(
            title = title,
            author = author,
            year = year_match,
            doi = doi
          )

        if (
          !is.na(metadata_result$pmid) &&
          nzchar(metadata_result$pmid)
        ) {

          pmid <- metadata_result$pmid

          metadata_fallbacks <-
            metadata_fallbacks + 1

          message(
            "  PubMed match: ",
            metadata_result$title
          )

          message(
            "  PMID -> ",
            pmid
          )

          message(
            "  Match score: ",
            round(
              metadata_result$score,
              3
            )
          )

        } else {

          message(
            "  No reliable PubMed match."
          )
        }
      }

      # ---------------------------------------------------------------
      # 5. Metadata PMID -> PMC
      # ---------------------------------------------------------------

      if (
        !is.null(pmid) &&
        !is.na(pmid) &&
        nzchar(pmid) &&
        (
          is.null(pmcid) ||
          !nzchar(pmcid)
        )
      ) {

        message(
          "Checking PMC by PMID: ",
          pmid
        )

        pmc_record <- get_pmc_record_by_pmid(
          pmid
        )

        if (!is.null(pmc_record)) {

          pmcid <- pmc_record$pmcid %||%
            NULL

          if (!is.null(pmcid)) {

            pmcid <- trimws(
              as.character(pmcid)
            )

            if (nzchar(pmcid)) {

              message(
                "  PMCID -> ",
                pmcid
              )
            }
          }
        }
      }

      Sys.sleep(pubmed_delay)

      # ---------------------------------------------------------------
      # Final values
      # ---------------------------------------------------------------

      final_pmid <- current_pmid
      final_pmcid <- current_pmcid

      if (
        !nzchar(final_pmid) &&
        !is.null(pmid) &&
        !is.na(pmid) &&
        nzchar(pmid)
      ) {

        final_pmid <- as.character(
          pmid
        )
      }

      if (
        !nzchar(final_pmcid) &&
        !is.null(pmcid) &&
        !is.na(pmcid) &&
        nzchar(pmcid)
      ) {

        final_pmcid <- as.character(
          pmcid
        )
      }

      if (
        !nzchar(final_pmid) &&
        !nzchar(final_pmcid)
      ) {

        not_found <- not_found + 1

        message(
          "No PMID or PMCID: ",
          title
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
        ifelse(
          nzchar(title),
          title,
          doi
        )
      )

      if (nzchar(doi)) {
        message(
          "  DOI -> ",
          doi
        )
      }

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

    Sys.sleep(pmc_delay)
  }

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
    "Total journal articles:     ",
    length(items)
  )

  message(
    "Articles requiring lookup:  ",
    length(todo)
  )

  message(
    "Zotero items updated:        ",
    updated
  )

  message(
    "PMIDs added:                 ",
    pmids_added
  )

  message(
    "PMCIDs added:                ",
    pmcids_added
  )

  message(
    "Both PMID + PMCID added:     ",
    both_added
  )

  message(
    "Already complete:            ",
    already_complete
  )

  message(
    "DOI lookups:                 ",
    doi_lookups
  )

  message(
    "Metadata fallbacks:          ",
    metadata_fallbacks
  )

  message(
    "PubMed fallbacks:            ",
    pubmed_fallbacks
  )

  message(
    "No identifier found:         ",
    not_found
  )

  message(
    "Errors:                      ",
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
      metadata_fallbacks = metadata_fallbacks,
      pubmed_fallbacks = pubmed_fallbacks,
      not_found = not_found,
      errors = errors,
      dry_run = dry_run
    )
  )
}
