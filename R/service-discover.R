#' Send a Discover request and return a data.frame
#'
#' Wraps \code{Connection$discover()} and parses the response rows into a
#' data.frame with one column per XML child element.
#'
#' @param conn A \code{Connection} object.
#' @param request_type The XMLA discover request type string.
#' @param restrictions Named list of restriction key/value pairs.
#' @param properties Named list of property key/value pairs.
#'   \code{Format = "Tabular"} is added automatically if not present.
#' @return A data.frame with one row per result.
#' @export
xmla_discover <- function(conn,
                          request_type,
                          restrictions = list(),
                          properties = list()) {
  if (is.null(properties[["Format"]])) {
    properties[["Format"]] <- "Tabular"
  }
  xml <- conn$discover(request_type, restrictions, properties)
  if (is.null(xml))
    return(data.frame())
  rows <- xml_get_discover_rows(xml)
  xml_nodes_to_df(rows)
}

#' Send a Discover request and return raw XML row nodes
#'
#' Like \code{xmla_discover} but returns the xml2 row nodes instead of a
#' data.frame, for use by typed helpers that build R6 objects.
#'
#' @inheritParams xmla_discover
#' @return A nodeset of row elements.
#' @keywords internal
xmla_discover_raw <- function(conn,
                              request_type,
                              restrictions = list(),
                              properties = list()) {
  if (is.null(properties[["Format"]])) {
    properties[["Format"]] <- "Tabular"
  }
  xml <- conn$discover(request_type, restrictions, properties)
  if (is.null(xml))
    return(list())
  xml_get_discover_rows(xml)
}
