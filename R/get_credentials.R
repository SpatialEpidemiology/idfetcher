idfetcher_get_credentials <- function() {

  user_id <- getOption("idfetcher.user_id", "")
  api_key <- getOption("idfetcher.api_key", "")

  if (!nzchar(user_id)) {
    stop(
      "Zotero user ID has not been set. ",
      "Run idfetcher_set_credentials() first."
    )
  }

  if (!nzchar(api_key)) {
    stop(
      "Zotero API key has not been set. ",
      "Run idfetcher_set_credentials() first."
    )
  }

  list(
    user_id = user_id,
    api_key = api_key
  )
}
