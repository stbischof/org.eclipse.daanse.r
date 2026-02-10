library(httr2)

#' Send a SOAP Discover request to an XMLA server.
#' print response data.
#' @param url endpoint URL.
#' @param username username.
#' @param password password.
discover_catalogs <- function(url, username, password) {

  soap_xml_body <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<SOAP-ENV:Envelope',
    ' xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/"',
    ' xmlns:xmla="urn:schemas-microsoft-com:xml-analysis">',
    '<SOAP-ENV:Header>',
    '<_username>', username, '</_username>',
    '<_password>', password, '</_password>',
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
  
  req <- request(url)
  req <- req_headers(req, "Content-Type" = "text/xml; charset=UTF-8")
  req <- req_body_raw(req, soap_xml_body, type = "text/xml; charset=UTF-8")
  req <- req_error(req, is_error = function(resp) FALSE)
  
  resp <- req_perform(req)
  cat("HTTP", resp_status(resp), "\n\n")
  cat(rawToChar(resp_body_raw(resp)), "\n")
}
