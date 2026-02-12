norm <- function(x) {
  x <- gsub(">\\s+<", "><", x)
  x <- gsub("\\s+", " ", x)
  x <- gsub("\\s+/>", "/>", x)
  trimws(x)
}

testthat::test_that("discover_request serializes with SOAP-ENV prefix and default xmla namespace",
                    {
                      env <- soap_envelope(body = list(
                        discover_request(
                          request_type = "DBSCHEMA_CATALOGS",
                          restrictions = list(),
                          properties = list(Format = "Tabular")
                        )
                      ))
                      
                      xml <- to_xml_string(env)
                      
                      expected <- paste0(
                        '<?xml version="1.0" encoding="UTF-8"?>',
                        '<SOAP-ENV:Envelope xmlns:SOAP-ENV="',
                        XmlNamespaces$soap,
                        '" xmlns:xmla="',
                        XmlNamespaces$xmla,
                        '">',
                        '<SOAP-ENV:Header></SOAP-ENV:Header>',
                        '<SOAP-ENV:Body>',
                        '<Discover xmlns="',
                        XmlNamespaces$xmla,
                        '">',
                        '<RequestType>DBSCHEMA_CATALOGS</RequestType>',
                        '<Restrictions></Restrictions>',
                        '<Properties><PropertyList><Format>Tabular</Format></PropertyList></Properties>',
                        '</Discover>',
                        '</SOAP-ENV:Body>',
                        '</SOAP-ENV:Envelope>'
                      )
                      
                      testthat::expect_equal(norm(xml), norm(expected))
                    })

testthat::test_that("discover_request with restrictions includes RestrictionList",
                    {
                      env <- soap_envelope(body = list(
                        discover_request(
                          request_type = "MDSCHEMA_CUBES",
                          restrictions = list(CATALOG_NAME = "Sales"),
                          properties = list()
                        )
                      ))
                      
                      xml <- to_xml_string(env)
                      
                      testthat::expect_match(
                        xml,
                        "<Restrictions><RestrictionList><CATALOG_NAME>Sales</CATALOG_NAME></RestrictionList></Restrictions>"
                      )
                    })

testthat::test_that("discover_request empty restrictions omits RestrictionList", {
  env <- soap_envelope(body = list(
    discover_request(
      "DBSCHEMA_CATALOGS",
      restrictions = list(),
      properties = list()
    )
  ))
  
  xml <- to_xml_string(env)
  
  testthat::expect_match(xml, "<Restrictions></Restrictions>")
  testthat::expect_false(grepl("RestrictionList", xml))
})

testthat::test_that("execute_request serializes with default xmla namespace", {
  env <- soap_envelope(body = list(execute_request(
    "SELECT FROM [Sales]",
    properties = list(Format = "Multidimensional")
  )))
  
  xml <- to_xml_string(env)
  
  expected <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<SOAP-ENV:Envelope xmlns:SOAP-ENV="',
    XmlNamespaces$soap,
    '" xmlns:xmla="',
    XmlNamespaces$xmla,
    '">',
    '<SOAP-ENV:Header></SOAP-ENV:Header>',
    '<SOAP-ENV:Body>',
    '<Execute xmlns="',
    XmlNamespaces$xmla,
    '">',
    '<Command><Statement>SELECT FROM [Sales]</Statement></Command>',
    '<Properties><PropertyList><Format>Multidimensional</Format></PropertyList></Properties>',
    '</Execute>',
    '</SOAP-ENV:Body>',
    '</SOAP-ENV:Envelope>'
  )
  
  testthat::expect_equal(norm(xml), norm(expected))
})

testthat::test_that("execute_request empty properties omits PropertyList", {
  env <- soap_envelope(body = list(execute_request("SELECT 1", properties = list())))
  
  xml <- to_xml_string(env)
  
  testthat::expect_match(xml, "<Properties></Properties>")
  testthat::expect_false(grepl("PropertyList", xml))
})

testthat::test_that("SOAP header combines extra nodes", {
  auth <- BasicAuth$new("u", "p")
  conn <- Connection$new("http://localhost/xmla", auth)
  
  combine <- conn$.__enclos_env__$private$combine_header_nodes
  extra <- list(xml_node("", "BeginSession", attrs = list(xmlns = XmlNamespaces$xmla)))
  nodes <- combine(extra)
  
  testthat::expect_length(nodes, 1)
  testthat::expect_equal(nodes[[1]]$tag, "BeginSession")
})

testthat::test_that("NoAuth produces empty SOAP header", {
  auth <- NoAuth$new()
  conn <- Connection$new("http://localhost/xmla", auth)
  
  combine <- conn$.__enclos_env__$private$combine_header_nodes
  nodes <- combine()
  
  testthat::expect_length(nodes, 0)
})

testthat::test_that("send_soap_request does not send SOAPAction header", {
  testthat::skip_if_not_installed("httr2")
  
  captured_req <- NULL
  local_mocked_bindings(
    req_perform = function(req) {
      captured_req <<- req
      httr2::response(
        status_code = 200L,
        headers = list("Content-Type" = "text/xml"),
        body = charToRaw(
          paste0(
            '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">',
            '<SOAP-ENV:Body><ok/></SOAP-ENV:Body></SOAP-ENV:Envelope>'
          )
        )
      )
    },
    .package = "httr2"
  )
  
  send_soap_request("http://localhost/xmla", "<dummy/>", NoAuth$new())
  
  header_names <- names(captured_req$headers)
  testthat::expect_false("SOAPAction" %in% header_names)
})

testthat::test_that("send_soap_request detects SOAP Fault", {
  testthat::skip_if_not_installed("httr2")
  
  fault_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">',
    '<soap:Body>',
    '<soap:Fault>',
    '<faultcode>soap:Server</faultcode>',
    '<faultstring>Something went wrong</faultstring>',
    '</soap:Fault>',
    '</soap:Body>',
    '</soap:Envelope>'
  )
  
  local_mocked_bindings(
    req_perform = function(req) {
      httr2::response(
        status_code = 200L,
        headers = list("Content-Type" = "text/xml"),
        body = charToRaw(fault_xml)
      )
    },
    .package = "httr2"
  )
  
  testthat::expect_error(
    send_soap_request("http://localhost/xmla", "<dummy/>", NoAuth$new()),
    "SOAP Fault.*Something went wrong"
  )
})

testthat::test_that("special characters in text are XML-escaped", {
  node <- xml_node("", "Value", text = "a&b<c>d")
  xml <- .node_to_string(node)
  testthat::expect_equal(xml, "<Value>a&amp;b&lt;c&gt;d</Value>")
})

testthat::test_that("full discover envelope with BasicAuth matches expected format", {
  auth <- BasicAuth$new("sss", "")
  conn <- Connection$new("http://localhost/xmla", auth)
  
  combine <- conn$.__enclos_env__$private$combine_header_nodes
  header_nodes <- combine()
  
  env <- soap_envelope(header = header_nodes, body = list(discover_request(
    "DBSCHEMA_CATALOGS", properties = list(Format = "Tabular")
  )))
  
  xml <- to_xml_string(env)
  
  testthat::expect_match(xml, "<SOAP-ENV:Envelope")
  testthat::expect_match(xml, "<SOAP-ENV:Header>")
  testthat::expect_match(xml,
                         '<Discover xmlns="urn:schemas-microsoft-com:xml-analysis">')
  testthat::expect_match(xml, "<RequestType>DBSCHEMA_CATALOGS</RequestType>")
  testthat::expect_false(grepl("_username", xml))
  testthat::expect_false(grepl("_password", xml))
  testthat::expect_false(grepl("SOAPAction", xml))
  testthat::expect_false(grepl("xmla:", xml))
})
