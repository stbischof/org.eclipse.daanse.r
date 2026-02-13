.mock_discover_response <- function(rows_xml = "") {
  paste0(
    '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">',
    '<SOAP-ENV:Header/><SOAP-ENV:Body>',
    '<DiscoverResponse xmlns="urn:schemas-microsoft-com:xml-analysis">',
    '<return>',
    '<root xmlns="urn:schemas-microsoft-com:xml-analysis:rowset">',
    rows_xml,
    '</root></return></DiscoverResponse>',
    '</SOAP-ENV:Body></SOAP-ENV:Envelope>'
  )
}

testthat::test_that("xmla_discover returns data.frame", {
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      xml2::read_xml(
        .mock_discover_response(
          '<row xmlns="urn:schemas-microsoft-com:xml-analysis:rowset"><CATALOG_NAME>Sales</CATALOG_NAME></row>'
        )
      )
    }
  )
  
  conn <- Connection$new("http://localhost/xmla")
  df <- xmla_discover(conn, "DBSCHEMA_CATALOGS")
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_equal(nrow(df), 1)
  testthat::expect_equal(df$CATALOG_NAME, "Sales")
})

testthat::test_that("xmla_discover auto-sets Format=Tabular", {
  captured_xml <- NULL
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      captured_xml <<- soap_xml
      xml2::read_xml(.mock_discover_response())
    }
  )
  
  conn <- Connection$new("http://localhost/xmla")
  xmla_discover(conn, "DBSCHEMA_CATALOGS")
  testthat::expect_match(captured_xml, "<Format>Tabular</Format>")
})

testthat::test_that("xmla_discover preserves user-supplied Format", {
  captured_xml <- NULL
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      captured_xml <<- soap_xml
      xml2::read_xml(.mock_discover_response())
    }
  )
  
  conn <- Connection$new("http://localhost/xmla")
  xmla_discover(conn, "DBSCHEMA_CATALOGS", properties = list(Format = "Native"))
  testthat::expect_match(captured_xml, "<Format>Native</Format>")
  testthat::expect_false(grepl("Tabular", captured_xml))
})

testthat::test_that("discover_catalogs calls correct request_type", {
  captured_xml <- NULL
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      captured_xml <<- soap_xml
      xml2::read_xml(
        .mock_discover_response(
          '<row xmlns="urn:schemas-microsoft-com:xml-analysis:rowset"><CATALOG_NAME>Sales</CATALOG_NAME></row>'
        )
      )
    }
  )
  
  conn <- Connection$new("http://localhost/xmla")
  result <- discover_catalogs(conn)
  testthat::expect_match(captured_xml,
                         "<RequestType>DBSCHEMA_CATALOGS</RequestType>")
  testthat::expect_length(result, 1)
  testthat::expect_s3_class(result[[1]], "Catalog")
  testthat::expect_equal(result[[1]]$catalog_name, "Sales")
})

testthat::test_that("discover_cubes includes CATALOG_NAME restriction", {
  captured_xml <- NULL
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      captured_xml <<- soap_xml
      xml2::read_xml(
        .mock_discover_response(
          '<row xmlns="urn:schemas-microsoft-com:xml-analysis:rowset"><CATALOG_NAME>Sales</CATALOG_NAME><CUBE_NAME>MyCube</CUBE_NAME><CUBE_TYPE>CUBE</CUBE_TYPE></row>'
        )
      )
    }
  )
  
  conn <- Connection$new("http://localhost/xmla")
  result <- discover_cubes(conn, "Sales")
  testthat::expect_match(captured_xml, "<RequestType>MDSCHEMA_CUBES</RequestType>")
  testthat::expect_match(captured_xml, "<CATALOG_NAME>Sales</CATALOG_NAME>")
  testthat::expect_length(result, 1)
  testthat::expect_s3_class(result[[1]], "Cube")
})

testthat::test_that("discover_hierarchies includes optional dimension restriction",
                    {
                      captured_xml <- NULL
                      local_mocked_bindings(
                        send_soap_request = function(url, soap_xml, auth) {
                          captured_xml <<- soap_xml
                          xml2::read_xml(.mock_discover_response())
                        }
                      )
                      
                      conn <- Connection$new("http://localhost/xmla")
                      discover_hierarchies(conn, "Sales", "MyCube", dimension = "[Time]")
                      testthat::expect_match(captured_xml,
                                             "<DIMENSION_UNIQUE_NAME>\\[Time\\]</DIMENSION_UNIQUE_NAME>")
                    })

testthat::test_that("discover_datasources calls DISCOVER_DATASOURCES", {
  captured_xml <- NULL
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      captured_xml <<- soap_xml
      xml2::read_xml(
        .mock_discover_response(
          '<row xmlns="urn:schemas-microsoft-com:xml-analysis:rowset"><DataSourceName>DS1</DataSourceName></row>'
        )
      )
    }
  )
  
  conn <- Connection$new("http://localhost/xmla")
  result <- discover_datasources(conn)
  testthat::expect_match(captured_xml,
                         "<RequestType>DISCOVER_DATASOURCES</RequestType>")
  testthat::expect_length(result, 1)
  testthat::expect_s3_class(result[[1]], "DataSource")
})
