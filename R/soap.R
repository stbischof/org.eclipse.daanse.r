#' Send a SOAP Discover request to an XMLA server.
#' @param url endpoint URL.
#' @param username username (unused, kept for compatibility).
#' @param password password (unused, kept for compatibility).
#' @export
discover_catalogs <- function(url, username, password) {
  auth <- BasicAuth$new(username = username, password = password)
  discover_catalogs_auth(url, auth)
}

#' Send a SOAP Discover request to an XMLA server.
#' Prints response data.
#' @param url endpoint URL.
#' @param auth an authentication handler.
#' @export
discover_catalogs_auth <- function(url, auth) {
  conn <- Connection$new(url, auth)
  doc <- conn$discover("DBSCHEMA_CATALOGS", properties = list(Format = "Tabular"))
  
  cat("\n--- Response ---\n")
  cat(as.character(doc), "\n")
}
