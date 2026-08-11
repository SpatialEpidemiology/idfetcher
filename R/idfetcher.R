#' Fetch and write PMID and PMCID identifiers to Zotero
#'
#' Retrieves PMID and PMCID identifiers for Zotero journal articles
#' using the NCBI PMC ID Converter and PubMed APIs.
#'
#' Lookup order:
#'   1. PMC ID Converter using DOI
#'   2. PubMed using DOI
#'   3. PubMed using exact title
#'   4. PubMed using title words
#'   5. PubMed using title + author
#'   6. PubMed using title + year
#'   7. PubMed candidate scoring using title, author, and year
#'   8. PMC ID Converter using the recovered PMID
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

  user_agent <- "IDFetcher/1.2"

  # --------------------------------------------------------------------------
  # Utility
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

  normalize_text <- function(x) {

    if (
      is.null(x) ||
      length(x) == 0
    ) {
      return("")
    }

    x <- as.character(x)[1]

    if (is.na(x)) {
      return("")
    }

    x <- tolower(x)

    x <- iconv(
      x,
      from = "",
      to = "ASCII//TRANSLIT"
    )

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

  get_field <- function(item, field) {

    value <- item$data[[field]]

    if (
      is.null(value) ||
      length(value) == 0
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

  get_journal <- function(item) {

    journal <- get_field(
      item,
      "publicationTitle"
    )

    if (nzchar(journal)) {
      return(journal)
    }

    get_field(
      item,
      "journalAbbreviation"
    )
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
  # Stop words
  # --------------------------------------------------------------------------

  stop_words <- c(
    "a",
    "an",
    "and",
    "are",
    "as",
    "at",
    "be",
    "by",
    "for",
    "from",
    "in",
    "into",
    "is",
    "it",
    "of",
    "on",
    "or",
    "that",
    "the",
    "their",
    "this",
    "to",
    "with",
    "using",
    "use",
    "among",
    "between",
    "associated",
    "association",
    "study",
    "analysis",
    "approach"
  )

  get_title_words <- function(title) {

    title <- normalize_text(title)

    if (!nzchar(title)) {
      return(character())
    }

    words <- unlist(
      strsplit(
        title,
        "\\s+"
      )
    )

    words <- words[
      nchar(words) >= 3
    ]

    words <- words[
      !words %in% stop_words
    ]

    unique(words)
  }

  get_distinctive_title_words <- function(
    title,
    max_words = 12
  ) {

    words <- get_title_words(
      title
    )

    if (length(words) == 0) {
      return(character())
    }

    if (
      length(words) <= max_words
    ) {
      return(words)
    }

    # Prefer longer words because they tend to be more discriminative.
    words <- words[
      order(
        nchar(words),
        decreasing = TRUE
      )
    ]

    words[
      seq_len(
        min(
          max_words,
          length(words)
        )
      )
    ]
  }

  # --------------------------------------------------------------------------
  # PubMed ESearch
  # --------------------------------------------------------------------------

  pubmed_search <- function(
    term,
    retmax = 20
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
      retmax = retmax,
      sort = "relevance"
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

          Sys.sleep(
            wait_time
          )

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

        ids <- json$esearchresult$idlist

        if (
          is.null(ids) ||
          length(ids) == 0
        ) {
          return(
            character()
          )
        }

        as.character(ids)

      }, error = function(e) {

        if (
          attempt < max_retries
        ) {

          wait_time <- 2^(attempt - 1)

          message(
            "PubMed search error: ",
            conditionMessage(e)
          )

          message(
            "Retrying in ",
            wait_time,
            " seconds..."
          )

          Sys.sleep(
            wait_time
          )

          return(NULL)
        }

        message(
          "PubMed search failed: ",
          conditionMessage(e)
        )

        character()
      })

      if (!is.null(result)) {
        return(result)
      }
    }

    character()
  }

  # --------------------------------------------------------------------------
  # PubMed ESummary
  # --------------------------------------------------------------------------

  get_pubmed_summaries <- function(
    ids
  ) {

    ids <- unique(
      as.character(ids)
    )

    ids <- ids[
      nzchar(ids)
    ]

    if (length(ids) == 0) {
      return(list())
    }

    params <- list(
      db = "pubmed",
      id = paste(
        ids,
        collapse = ","
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
          pubmed_summary_url
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
            "PubMed summary rate limit (429). Waiting ",
            wait_time,
            " seconds..."
          )

          Sys.sleep(
            wait_time
          )

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

        json$result %||% list()

      }, error = function(e) {

        if (
          attempt < max_retries
        ) {

          wait_time <- 2^(attempt - 1)

          message(
            "PubMed summary error: ",
            conditionMessage(e)
          )

          message(
            "Retrying in ",
            wait_time,
            " seconds..."
          )

          Sys.sleep(
            wait_time
          )

          return(NULL)
        }

        message(
          "PubMed summary failed: ",
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
  # Extract PubMed author
  # --------------------------------------------------------------------------

  get_pubmed_first_author <- function(
    record
  ) {

    authors <- record$authors

    if (
      is.null(authors) ||
      length(authors) == 0
    ) {
      return("")
    }

    first <- authors[[1]]

    if (
      !is.null(first$name) &&
      nzchar(
        as.character(
          first$name
        )
      )
    ) {

      return(
        as.character(
          first$name
        )
      )
    }

    if (
      !is.null(first$authtype) &&
      !is.null(first$name)
    ) {

      return(
        as.character(
          first$name
        )
      )
    }

    ""
  }

  # --------------------------------------------------------------------------
  # Extract PubMed year
  # --------------------------------------------------------------------------

  get_pubmed_year <- function(
    record
  ) {

    candidates <- c(
      record$pubdate %||% "",
      record$epubdate %||% "",
      record$sortpubdate %||% ""
    )

    for (value in candidates) {

      value <- as.character(
        value
      )[1]

      if (
        !is.na(value) &&
        grepl(
          "[0-9]{4}",
          value
        )
      ) {

        match <- regmatches(
          value,
          regexpr(
            "[0-9]{4}",
            value
          )
        )

        if (
          length(match) > 0 &&
          !is.na(match)
        ) {
          return(match)
        }
      }
    }

    ""
  }

  # --------------------------------------------------------------------------
  # Extract PubMed identifiers
  # --------------------------------------------------------------------------

  get_pubmed_identifiers <- function(
    record
  ) {

    pmid <- ""

    pmcid <- ""

    doi <- ""

    if (
      !is.null(record$uid) &&
      nzchar(
        as.character(
          record$uid
        )
      )
    ) {

      pmid <- as.character(
        record$uid
      )
    }

    article_ids <- record$articleids

    if (
      !is.null(article_ids) &&
      length(article_ids) > 0
    ) {

      for (article_id in article_ids) {

        if (
          is.null(article_id)
        ) {
          next
        }

        id_type <- tolower(
          as.character(
            article_id$idtype %||% ""
          )
        )

        value <- as.character(
          article_id$value %||% ""
        )

        if (
          id_type == "pmc" ||
          id_type == "pmcid"
        ) {

          if (
            nzchar(value)
          ) {

            pmcid <- value

            if (
              !startsWith(
                pmcid,
                "PMC"
              )
            ) {

              pmcid <- paste0(
                "PMC",
                pmcid
              )
            }
          }
        }

        if (
          id_type == "doi"
        ) {

          doi <- normalize_doi(
            value
          )
        }

        if (
          id_type == "pubmed"
        ) {

          pmid <- value
        }
      }
    }

    list(
      pmid = trimws(pmid),
      pmcid = trimws(pmcid),
      doi = trimws(doi)
    )
  }

  # --------------------------------------------------------------------------
  # Title similarity
  # --------------------------------------------------------------------------

  title_similarity <- function(
    title_a,
    title_b
  ) {

    a <- normalize_text(
      title_a
    )

    b <- normalize_text(
      title_b
    )

    if (
      !nzchar(a) ||
      !nzchar(b)
    ) {
      return(0)
    }

    if (
      identical(a, b)
    ) {
      return(1)
    }

    a_words <- unique(
      strsplit(
        a,
        "\\s+"
      )[[1]]
    )

    b_words <- unique(
      strsplit(
        b,
        "\\s+"
      )[[1]]
    )

    if (
      length(a_words) == 0 ||
      length(b_words) == 0
    ) {
      return(0)
    }

    intersection <- sum(
      a_words %in% b_words
    )

    union <- length(
      unique(
        c(
          a_words,
          b_words
        )
      )
    )

    jaccard <- intersection / union

    coverage_a <-
      intersection /
      length(a_words)

    coverage_b <-
      intersection /
      length(b_words)

    # Favor coverage of the Zotero title.
    score <- (
      0.50 * coverage_a
    ) +
      (
        0.30 * coverage_b
      ) +
      (
        0.20 * jaccard
      )

    min(
      max(
        score,
        0
      ),
      1
    )
  }

  # --------------------------------------------------------------------------
  # Candidate scoring
  # --------------------------------------------------------------------------

  score_pubmed_candidate <- function(
    zotero_title,
    zotero_author,
    zotero_year,
    record
  ) {

    candidate_title <-
      as.character(
        record$title %||% ""
      )

    candidate_author <-
      get_pubmed_first_author(
        record
      )

    candidate_year <-
      get_pubmed_year(
        record
      )

    title_score <- title_similarity(
      zotero_title,
      candidate_title
    )

    author_score <- 0

    if (
      nzchar(zotero_author) &&
      nzchar(candidate_author)
    ) {

      z_author <- normalize_text(
        zotero_author
      )

      p_author <- normalize_text(
        candidate_author
      )

      if (
        identical(
          z_author,
          p_author
        )
      ) {

        author_score <- 1

      } else if (
        grepl(
          z_author,
          p_author,
          fixed = TRUE
        ) ||
        grepl(
          p_author,
          z_author,
          fixed = TRUE
        )
      ) {

        author_score <- 0.8
      }
    }

    year_score <- 0

    if (
      nzchar(zotero_year) &&
      nzchar(candidate_year)
    ) {

      if (
        identical(
          zotero_year,
          candidate_year
        )
      ) {

        year_score <- 1

      } else {

        z_year <- suppressWarnings(
          as.integer(
            zotero_year
          )
        )

        p_year <- suppressWarnings(
          as.integer(
            candidate_year
          )
        )

        if (
          !is.na(z_year) &&
          !is.na(p_year) &&
          abs(
            z_year - p_year
          ) == 1
        ) {

          year_score <- 0.5
        }
      }
    }

    total <- (
      0.75 * title_score
    ) +
      (
        0.15 * author_score
      ) +
      (
        0.10 * year_score
      )

    list(
      total = total,
      title = title_score,
      author = author_score,
      year = year_score
    )
  }

  # --------------------------------------------------------------------------
  # PubMed metadata matching
  # --------------------------------------------------------------------------

  find_pubmed_by_metadata <- function(
    title,
    first_author = "",
    year = "",
    journal = ""
  ) {

    if (
      !nzchar(
        trimws(title)
      )
    ) {
      return(NULL)
    }

    candidate_ids <- character()

    # ------------------------------------------------------------------------
    # Search 1: exact title
    # ------------------------------------------------------------------------

    exact_title <- gsub(
      '"',
      "",
      title,
      fixed = TRUE
    )

    term <- paste0(
      '"',
      exact_title,
      '"[Title]'
    )

    ids <- pubmed_search(
      term,
      retmax = 20
    )

    candidate_ids <- unique(
      c(
        candidate_ids,
        ids
      )
    )

    Sys.sleep(
      pubmed_delay
    )

    # ------------------------------------------------------------------------
    # Search 2: exact title + author
    # ------------------------------------------------------------------------

    if (
      length(candidate_ids) == 0 &&
      nzchar(first_author)
    ) {

      author <- gsub(
        "[^A-Za-z0-9 -]",
        "",
        first_author
      )

      if (nzchar(author)) {

        term <- paste0(
          '"',
          exact_title,
          '"[Title] AND ',
          author,
          "[Author]"
        )

        ids <- pubmed_search(
          term,
          retmax = 20
        )

        candidate_ids <- unique(
          c(
            candidate_ids,
            ids
          )
        )

        Sys.sleep(
          pubmed_delay
        )
      }
    }

    # ------------------------------------------------------------------------
    # Search 3: exact title + year
    # ------------------------------------------------------------------------

    if (
      length(candidate_ids) == 0 &&
      grepl(
        "^[0-9]{4}$",
        year
      )
    ) {

      term <- paste0(
        '"',
        exact_title,
        '"[Title] AND ',
        year,
        "[pdat]"
      )

      ids <- pubmed_search(
        term,
        retmax = 20
      )

      candidate_ids <- unique(
        c(
          candidate_ids,
          ids
        )
      )

      Sys.sleep(
        pubmed_delay
      )
    }

    # ------------------------------------------------------------------------
    # Search 4: distinctive title words
    # ------------------------------------------------------------------------

    distinctive_words <-
      get_distinctive_title_words(
        title,
        max_words = 10
      )

    if (
      length(distinctive_words) >= 3
    ) {

      title_word_query <- paste(
        distinctive_words,
        collapse = " AND "
      )

      term <- paste0(
        "(",
        title_word_query,
        ")[Title/Abstract]"
      )

      ids <- pubmed_search(
        term,
        retmax = 50
      )

      candidate_ids <- unique(
        c(
          candidate_ids,
          ids
        )
      )

      Sys.sleep(
        pubmed_delay
      )
    }

    # ------------------------------------------------------------------------
    # Search 5: distinctive title words + author
    # ------------------------------------------------------------------------

    if (
      length(distinctive_words) >= 3 &&
      nzchar(first_author)
    ) {

      author <- gsub(
        "[^A-Za-z0-9 -]",
        "",
        first_author
      )

      if (nzchar(author)) {

        title_word_query <- paste(
          distinctive_words,
          collapse = " AND "
        )

        term <- paste0(
          "(",
          title_word_query,
          ")[Title/Abstract] AND ",
          author,
          "[Author]"
        )

        ids <- pubmed_search(
          term,
          retmax = 50
        )

        candidate_ids <- unique(
          c(
            candidate_ids,
            ids
          )
        )

        Sys.sleep(
          pubmed_delay
        )
      }
    }

    # ------------------------------------------------------------------------
    # Search 6: title words + year
    # ------------------------------------------------------------------------

    if (
      length(distinctive_words) >= 3 &&
      grepl(
        "^[0-9]{4}$",
        year
      )
    ) {

      title_word_query <- paste(
        distinctive_words,
        collapse = " AND "
      )

      term <- paste0(
        "(",
        title_word_query,
        ")[Title/Abstract] AND ",
        year,
        "[pdat]"
      )

      ids <- pubmed_search(
        term,
        retmax = 50
      )

      candidate_ids <- unique(
        c(
          candidate_ids,
          ids
        )
      )

      Sys.sleep(
        pubmed_delay
      )
    }

    # ------------------------------------------------------------------------
    # Search 7: title words + author + year
    # ------------------------------------------------------------------------

    if (
      length(distinctive_words) >= 3 &&
      nzchar(first_author) &&
      grepl(
        "^[0-9]{4}$",
        year
      )
    ) {

      author <- gsub(
        "[^A-Za-z0-9 -]",
        "",
        first_author
      )

      if (nzchar(author)) {

        title_word_query <- paste(
          distinctive_words,
          collapse = " AND "
        )

        term <- paste0(
          "(",
          title_word_query,
          ")[Title/Abstract] AND ",
          author,
          "[Author] AND ",
          year,
          "[pdat]"
        )

        ids <- pubmed_search(
          term,
          retmax = 50
        )

        candidate_ids <- unique(
          c(
            candidate_ids,
            ids
          )
        )

        Sys.sleep(
          pubmed_delay
        )
      }
    }

    # ------------------------------------------------------------------------
    # Search 8: journal + title words
    # ------------------------------------------------------------------------

    if (
      length(distinctive_words) >= 3 &&
      nzchar(journal)
    ) {

      journal_clean <- gsub(
        '"',
        "",
        journal,
        fixed = TRUE
      )

      title_word_query <- paste(
        distinctive_words,
        collapse = " AND "
      )

      term <- paste0(
        "(",
        title_word_query,
        ")[Title/Abstract] AND ",
        '"',
        journal_clean,
        '"[Journal]'
      )

      ids <- pubmed_search(
        term,
        retmax = 50
      )

      candidate_ids <- unique(
        c(
          candidate_ids,
          ids
        )
      )

      Sys.sleep(
        pubmed_delay
      )
    }

    if (
      length(candidate_ids) == 0
    ) {
      return(NULL)
    }

    # ------------------------------------------------------------------------
    # Retrieve candidate metadata
    # ------------------------------------------------------------------------

    summaries <- get_pubmed_summaries(
      candidate_ids
    )

    if (
      length(summaries) == 0
    ) {
      return(NULL)
    }

    scored <- list()

    for (id in candidate_ids) {

      record <- summaries[[id]]

      if (
        is.null(record)
      ) {
        next
      }

      scores <- score_pubmed_candidate(
        zotero_title = title,
        zotero_author = first_author,
        zotero_year = year,
        record = record
      )

      identifiers <-
        get_pubmed_identifiers(
          record
        )

      scored[[id]] <- list(
        id = id,
        record = record,
        identifiers = identifiers,
        scores = scores
      )
    }

    if (
      length(scored) == 0
    ) {
      return(NULL)
    }

    totals <- vapply(
      scored,
      function(x) {
        x$scores$total
      },
      numeric(1)
    )

    order_idx <- order(
      totals,
      decreasing = TRUE
    )

    scored <- scored[
      order_idx
    ]

    best <- scored[[1]]

    # ------------------------------------------------------------------------
    # Acceptance rules
    # ------------------------------------------------------------------------

    best_title_score <-
      best$scores$title

    best_total <-
      best$scores$total

    second_total <- 0

    if (
      length(scored) >= 2
    ) {

      second_total <-
        scored[[2]]$scores$total
    }

    # Exact or nearly exact title.
    exact_match <-
      best_title_score >= 0.97

    strong_match <-
      best_title_score >= 0.90 &&
      best_total >= 0.82

    supported_match <-
      best_title_score >= 0.84 &&
      best_total >= 0.78 &&
      (
        best_total - second_total >= 0.05 ||
          best$scores$author >= 0.8 ||
          best$scores$year >= 1
      )

    if (
      !exact_match &&
      !strong_match &&
      !supported_match
    ) {

      return(NULL)
    }

    best
  }

  # --------------------------------------------------------------------------
  # PubMed DOI lookup
  # --------------------------------------------------------------------------

  get_pubmed_id_by_doi <- function(
    doi
  ) {

    doi <- normalize_doi(
      doi
    )

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

          Sys.sleep(
            wait_time
          )

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

          Sys.sleep(
            wait_time
          )

          return(NULL)
        }

        NA_character_
      })

      if (!is.null(result)) {
        return(result)
      }
    }

    NA_character_
  }

  # --------------------------------------------------------------------------
  # PMC lookup by DOI or PMID
  # --------------------------------------------------------------------------

  get_pmc_records <- function(
    ids,
    idtype = NULL
  ) {

    if (
      length(ids) == 0
    ) {
      return(list())
    }

    ids <- unique(
      as.character(ids)
    )

    ids <- ids[
      nzchar(ids)
    ]

    if (
      length(ids) == 0
    ) {
      return(list())
    }

    params <- list(
      ids = paste(
        ids,
        collapse = ","
      ),
      format = "json",
      tool = "idfetcher"
    )

    if (!is.null(idtype)) {

      params$idtype <- idtype
    }

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

          Sys.sleep(
            wait_time
          )

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

        if (
          is.null(records)
        ) {
          return(list())
        }

        result <- list()

        for (record in records) {

          requested <- normalize_doi(
            record$doi %||%
              record$requested_id %||%
              record[["requested-id"]]
          )

          key <- requested

          if (
            is.null(key) ||
            !nzchar(key)
          ) {

            key <-
              as.character(
                record$pmid %||%
                  record$pmcid %||%
                  ""
              )
          }

          if (
            nzchar(key)
          ) {

            result[[key]] <- record
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

          Sys.sleep(
            wait_time
          )

          return(NULL)
        }

        list()
      })

      if (!is.null(result)) {
        return(result)
      }
    }

    list()
  }

  # --------------------------------------------------------------------------
  # Get Zotero articles
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

      if (
        status >= 400
      ) {

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

      if (
        length(items) == 0
      ) {
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

      if (
        length(items) < limit
      ) {
        break
      }

      start <- start + limit
    }

    all_items
  }

  # --------------------------------------------------------------------------
  # Update Zotero
  # --------------------------------------------------------------------------

  update_zotero_item <- function(
    item
  ) {

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

    if (
      !is.null(version)
    ) {

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
      status != 204
    ) {

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
  # Determine which items need work
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

  if (
    length(todo) == 0
  ) {

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
          pmc_doi_lookups = 0,
          pubmed_doi_lookups = 0,
          pubmed_metadata_lookups = 0,
          pubmed_metadata_matches = 0,
          pmc_pmid_lookups = 0,
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

  pmc_doi_lookups <- 0
  pubmed_doi_lookups <- 0
  pubmed_metadata_lookups <- 0
  pubmed_metadata_matches <- 0
  pmc_pmid_lookups <- 0

  not_found <- 0
  errors <- 0

  # --------------------------------------------------------------------------
  # Process items
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
    # DOI batch
    # ------------------------------------------------------------------------

    doi_map <- list()

    for (item in batch) {

      doi <- normalize_doi(
        item$data$DOI %||% ""
      )

      if (
        nzchar(doi)
      ) {

        doi_map[[doi]] <- item
      }
    }

    pmc_doi_records <- list()

    if (
      length(doi_map) > 0
    ) {

      message(
        "Checking PMC for ",
        length(doi_map),
        " DOIs..."
      )

      pmc_doi_records <- tryCatch(
        get_pmc_records(
          names(doi_map),
          idtype = "doi"
        ),
        error = function(e) {

          message(
            "PMC DOI batch failed: ",
            conditionMessage(e)
          )

          list()
        }
      )

      pmc_doi_lookups <-
        pmc_doi_lookups +
        length(doi_map)

      message(
        "PMC returned ",
        length(pmc_doi_records),
        " records."
      )
    }

    # ------------------------------------------------------------------------
    # Each item
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

      journal <- get_journal(
        item
      )

      display_name <- if (
        nzchar(doi)
      ) {
        doi
      } else {
        title
      }

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
          display_name
        )

        next
      }

      pmid <- NULL
      pmcid <- NULL

      # ----------------------------------------------------------------------
      # STEP 1: PMC DOI result
      # ----------------------------------------------------------------------

      if (
        nzchar(doi)
      ) {

        record <-
          pmc_doi_records[[doi]]

        if (
          !is.null(record)
        ) {

          pmid <-
            record$pmid %||% NULL

          pmcid <-
            record$pmcid %||% NULL

          if (
            !is.null(pmid)
          ) {

            pmid <- trimws(
              as.character(pmid)
            )
          }

          if (
            !is.null(pmcid)
          ) {

            pmcid <- trimws(
              as.character(pmcid)
            )
          }
        }
      }

      # ----------------------------------------------------------------------
      # STEP 2: PubMed DOI
      # ----------------------------------------------------------------------

      if (
        is.null(pmid) ||
        !nzchar(pmid)
      ) {

        if (
          nzchar(doi)
        ) {

          message(
            "PubMed DOI lookup: ",
            doi
          )

          candidate_pmid <-
            get_pubmed_id_by_doi(
              doi
            )

          pubmed_doi_lookups <-
            pubmed_doi_lookups + 1

          if (
            !is.na(candidate_pmid) &&
            nzchar(candidate_pmid)
          ) {

            pmid <- candidate_pmid

            message(
              "  PMID -> ",
              pmid
            )
          }
        }

        Sys.sleep(
          pubmed_delay
        )
      }

      # ----------------------------------------------------------------------
      # STEP 3: Metadata matching
      # ----------------------------------------------------------------------

      if (
        is.null(pmid) ||
        !nzchar(pmid)
      ) {

        pubmed_metadata_lookups <-
          pubmed_metadata_lookups + 1

        message(
          "PubMed metadata search: ",
          title
        )

        match <- find_pubmed_by_metadata(
          title = title,
          first_author = first_author,
          year = year,
          journal = journal
        )

        if (
          !is.null(match)
        ) {

          pmid <-
            match$identifiers$pmid

          if (
            !nzchar(pmid)
          ) {

            pmid <- match$id
          }

          if (
            nzchar(
              match$identifiers$pmcid
            )
          ) {

            pmcid <-
              match$identifiers$pmcid
          }

          pubmed_metadata_matches <-
            pubmed_metadata_matches + 1

          message(
            "  PubMed match -> ",
            pmid
          )

          message(
            "  Title score -> ",
            sprintf(
              "%.3f",
              match$scores$title
            )
          )

          message(
            "  Match score -> ",
            sprintf(
              "%.3f",
              match$scores$total
            )
          )

        } else {

          message(
            "  No reliable PubMed match."
          )
        }

        Sys.sleep(
          pubmed_delay
        )
      }

      # ----------------------------------------------------------------------
      # STEP 4: PMID -> PMC
      # ----------------------------------------------------------------------

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
          "Checking PMC using PMID: ",
          pmid
        )

        pmc_pmid_records <- tryCatch(
          get_pmc_records(
            pmid,
            idtype = "pmid"
          ),
          error = function(e) {

            message(
              "  PMC PMID lookup failed: ",
              conditionMessage(e)
            )

            list()
          }
        )

        pmc_pmid_lookups <-
          pmc_pmid_lookups + 1

        if (
          length(pmc_pmid_records) > 0
        ) {

          record <- pmc_pmid_records[[1]]

          if (
            !is.null(record$pmcid)
          ) {

            candidate_pmcid <-
              trimws(
                as.character(
                  record$pmcid
                )
              )

            if (
              nzchar(candidate_pmcid)
            ) {

              pmcid <-
                candidate_pmcid

              message(
                "  PMCID -> ",
                pmcid
              )
            }
          }
        }

        Sys.sleep(
          pubmed_delay
        )
      }

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
          display_name
        )

        next
      }

      # ----------------------------------------------------------------------
      # Determine changes
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
        display_name
      )

      if (
        add_pmid
      ) {

        message(
          "  PMID  -> ",
          final_pmid
        )
      }

      if (
        add_pmcid
      ) {

        message(
          "  PMCID -> ",
          final_pmcid
        )
      }

      # ----------------------------------------------------------------------
      # Dry run
      # ----------------------------------------------------------------------

      if (
        dry_run
      ) {

        message(
          "  DRY RUN - Zotero not modified."
        )

        if (
          add_pmid
        ) {

          pmids_added <-
            pmids_added + 1
        }

        if (
          add_pmcid
        ) {

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
      # Zotero update
      # ----------------------------------------------------------------------

      tryCatch({

        if (
          add_pmid
        ) {

          item$data$PMID <-
            as.character(
              final_pmid
            )
        }

        if (
          add_pmcid
        ) {

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

        if (
          add_pmid
        ) {

          pmids_added <-
            pmids_added + 1
        }

        if (
          add_pmcid
        ) {

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
    "PMC DOI lookups:               ",
    pmc_doi_lookups
  )

  message(
    "PubMed DOI lookups:            ",
    pubmed_doi_lookups
  )

  message(
    "PubMed metadata lookups:       ",
    pubmed_metadata_lookups
  )

  message(
    "PubMed metadata matches:       ",
    pubmed_metadata_matches
  )

  message(
    "PMID -> PMC lookups:            ",
    pmc_pmid_lookups
  )

  message(
    "No identifier found:            ",
    not_found
  )

  message(
    "Errors:                         ",
    errors
  )

  message(
    "======================================================================"
  )

  if (
    dry_run
  ) {

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
      pmc_doi_lookups =
        pmc_doi_lookups,
      pubmed_doi_lookups =
        pubmed_doi_lookups,
      pubmed_metadata_lookups =
        pubmed_metadata_lookups,
      pubmed_metadata_matches =
        pubmed_metadata_matches,
      pmc_pmid_lookups =
        pmc_pmid_lookups,
      not_found = not_found,
      errors = errors,
      dry_run = dry_run
    )
  )
}
