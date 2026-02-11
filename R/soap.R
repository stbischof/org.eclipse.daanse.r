library(httr2)


#' Send a SOAP Discover request to an XMLA server.
#' @param url endpoint URL.
#' @param username username.
#' @param password password.
discover_catalogs <- function(url, username, password) {
  
auth <- NoAuth$new()

discover_catalogs_auth(url, auth)
}

#' Send a SOAP Discover request to an XMLA server.
#' print response data.
#' @param url endpoint URL.
#' @param auth an authentication handler
discover_catalogs_auth <- function(url, auth) {

  soap_xml_body <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<SOAP-ENV:Envelope',
    ' xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"',
    ' xmlns:xmla="urn:schemas-microsoft-com:xml-analysis">',
    '<SOAP-ENV:Header>',
    '</SOAP-ENV:Header>',
    '<SOAP-ENV:Body>',
    '<Discover xmlns="urn:schemas-microsoft-com:xml-analysis">',
    '<RequestType>DBSCHEMA_CATALOGS</RequestType>',
    '<Restrictions><RestrictionList/></Restrictions>',
    '<Properties><PropertyList><Format>Tabular</Format></PropertyList></Properties>',
    '</Discover>',
    '</SOAP-ENV:Body>',
    '</SOAP-ENV:Envelope>'
  )
  
  req <- httr2::request(url)
  
  req <- auth$apply_auth(req)
  
  req <- req_headers(req, "Content-Type" = "text/xml; charset=UTF-8")
  req <- req_body_raw(req, soap_xml_body, type = "text/xml; charset=UTF-8")
  req <- req_error(req, is_error = function(resp) FALSE)
  
  
  resp <- req_perform(req)
  cat("\n--- HTTP-Status ---\n")
  cat(resp_status(resp), "\n\n")
  cat("\n--- HEADER ---\n")
  print(resp_headers(resp))
  cat("\n--- BODY ---\n")
  cat(resp_body_string(resp), "\n")
}
