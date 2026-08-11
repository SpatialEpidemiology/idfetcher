#' Set Zotero credentials for idfetcher
#'
#' Stores the Zotero user ID and API key for the current R session.
#'
#' @param user_id Zotero user ID.
#' @param api_key Zotero API key.
#' @return Invisibly returns TRUE.
#' @export
idfetcher_set_credentials <- function(user_id, api_key) {

  if (
    missing(user_id) ||
    is.null(user_id) ||
    !nzchar(trimws(user_id))
  ) {
    stop("You must provide a Zotero user_id.")
  }

  if (
    missing(api_key) ||
    is.null(api_key) ||
    !nzchar(trimws(api_key))
  ) {
    stop("You must provide a Zotero api_key.")
  }

  options(
    idfetcher.user_id =
      trimws(as.character(user_id)),
    idfetcher.api_key =
      trimws(as.character(api_key))
  )

  message(
    "Zotero credentials saved for this R session."
  )

  invisible(TRUE)
}


#' Get Zotero credentials
#'
#' Retrieves the Zotero credentials currently stored
#' for the R session.
#'
#' @return A list containing user_id and api_key.
#' @export
idfetcher_get_credentials <- function() {

  user_id <-
    getOption(
      "idfetcher.user_id",
      ""
    )

  api_key <-
    getOption(
      "idfetcher.api_key",
      ""
    )

  if (
    !nzchar(user_id) ||
    !nzchar(api_key)
  ) {

    stop(
      "No Zotero credentials have been set. ",
      "Run idfetcher_set_credentials(user_id, api_key)."
    )
  }

  list(
    user_id = user_id,
    api_key = api_key
  )
}


#' Clear Zotero credentials
#'
#' Removes Zotero credentials from the current R session.
#'
#' @return Invisibly returns TRUE.
#' @export
idfetcher_clear_credentials <- function() {

  options(
    idfetcher.user_id = NULL,
    idfetcher.api_key = NULL
  )

  message(
    "Zotero credentials cleared."
  )

  invisible(TRUE)
}
