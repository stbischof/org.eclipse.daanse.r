testthat::test_that("Connection initializes with defaults", {
  conn <- Connection$new("http://localhost:8080/xmla")
  testthat::expect_equal(conn$url, "http://localhost:8080/xmla")
  testthat::expect_true(inherits(conn$auth, "NoAuth"))
  testthat::expect_null(conn$session_id)
})

testthat::test_that("Connection initializes with BasicAuth", {
  auth <- BasicAuth$new("user", "pass")
  conn <- Connection$new("http://localhost:8080/xmla", auth)
  testthat::expect_equal(conn$url, "http://localhost:8080/xmla")
  testthat::expect_true(inherits(conn$auth, "BasicAuth"))
  testthat::expect_null(conn$session_id)
})

testthat::test_that("Connection print output", {
  conn <- Connection$new("http://localhost:8080/xmla")
  out <- capture.output(conn$print())
  testthat::expect_true(any(grepl("Connection", out)))
  testthat::expect_true(any(grepl("localhost", out)))
  testthat::expect_true(any(grepl("NoAuth", out)))
  testthat::expect_true(any(grepl("\\(none\\)", out)))
})

.mock_soap_response <- function(body_xml = "<ok/>",
                                session_id = NULL) {
  header_xml <- ""
  if (!is.null(session_id)) {
    header_xml <- paste0(
      '<Session xmlns="urn:schemas-microsoft-com:xml-analysis" SessionId="',
      session_id,
      '"/>'
    )
  }
  resp_xml <- paste0(
    '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">',
    '<SOAP-ENV:Header>',
    header_xml,
    '</SOAP-ENV:Header>',
    '<SOAP-ENV:Body>',
    body_xml,
    '</SOAP-ENV:Body></SOAP-ENV:Envelope>'
  )
  xml2::read_xml(resp_xml)
}

testthat::test_that("discover sends credentials in SOAP header", {
  captured_xml <- NULL
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      captured_xml <<- soap_xml
      .mock_soap_response()
    }
  )
  
  auth <- BasicAuth$new("myuser", "mypass")
  conn <- Connection$new("http://localhost/xmla", auth)
  conn$discover("DBSCHEMA_CATALOGS", properties = list(Format = "Tabular"))
  
  testthat::expect_match(captured_xml, "<SOAP-ENV:Header>")
  testthat::expect_match(captured_xml,
                         '<Discover xmlns="urn:schemas-microsoft-com:xml-analysis">')
})

testthat::test_that("discover with NoAuth sends no credentials in header", {
  captured_xml <- NULL
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      captured_xml <<- soap_xml
      .mock_soap_response()
    }
  )
  
  conn <- Connection$new("http://localhost/xmla")
  conn$discover("DBSCHEMA_CATALOGS")
  
  testthat::expect_false(grepl("_username", captured_xml))
  testthat::expect_false(grepl("_password", captured_xml))
})

testthat::test_that("start_session includes BeginSession with credentials", {
  captured_xml <- NULL
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      captured_xml <<- soap_xml
      .mock_soap_response(session_id = "abc123")
    }
  )
  
  auth <- BasicAuth$new("u", "p")
  conn <- Connection$new("http://localhost/xmla", auth)
  conn$start_session()
  
  testthat::expect_match(captured_xml, "BeginSession")
  testthat::expect_match(captured_xml, 'soap:mustUnderstand="1"')
  testthat::expect_equal(conn$session_id, "abc123")
})

testthat::test_that("end_session includes EndSession and clears session_id", {
  call_count <- 0
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        testthat::expect_match(soap_xml, "EndSession")
        testthat::expect_match(soap_xml, 'SessionId="sess42"')
      }
      .mock_soap_response()
    }
  )
  
  conn <- Connection$new("http://localhost/xmla", BasicAuth$new("u", "p"))
  conn$session_id <- "sess42"
  conn$end_session()
  
  testthat::expect_null(conn$session_id)
  testthat::expect_equal(call_count, 1)
})

testthat::test_that("end_session is no-op without active session", {
  call_count <- 0
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      call_count <<- call_count + 1
      .mock_soap_response()
    }
  )
  
  conn <- Connection$new("http://localhost/xmla")
  conn$end_session()
  
  testthat::expect_equal(call_count, 0)
})
