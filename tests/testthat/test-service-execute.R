.mock_tabular_execute_response <- function(rows_xml = "") {
  paste0(
    '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">',
    '<SOAP-ENV:Header/><SOAP-ENV:Body>',
    '<ExecuteResponse xmlns="urn:schemas-microsoft-com:xml-analysis">',
    '<return>',
    '<root xmlns="urn:schemas-microsoft-com:xml-analysis:rowset">',
    rows_xml,
    '</root></return></ExecuteResponse>',
    '</SOAP-ENV:Body></SOAP-ENV:Envelope>'
  )
}

.mock_md_execute_response <- function(axes_xml = "",
                                      celldata_xml = "") {
  paste0(
    '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">',
    '<SOAP-ENV:Header/><SOAP-ENV:Body>',
    '<ExecuteResponse xmlns="urn:schemas-microsoft-com:xml-analysis">',
    '<return>',
    '<root xmlns="urn:schemas-microsoft-com:xml-analysis:mddataset">',
    '<Axes>',
    axes_xml,
    '</Axes>',
    '<CellData>',
    celldata_xml,
    '</CellData>',
    '</root></return></ExecuteResponse>',
    '</SOAP-ENV:Body></SOAP-ENV:Envelope>'
  )
}

testthat::test_that("parse_tabular_response with empty rows", {
  xml <- xml2::read_xml(.mock_tabular_execute_response())
  df <- parse_tabular_response(xml)
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_equal(nrow(df), 0)
})

testthat::test_that("parse_tabular_response with populated rows", {
  xml <- xml2::read_xml(
    .mock_tabular_execute_response(
      '<row xmlns="urn:schemas-microsoft-com:xml-analysis:rowset"><COL1>a</COL1><COL2>b</COL2></row><row xmlns="urn:schemas-microsoft-com:xml-analysis:rowset"><COL1>c</COL1><COL2>d</COL2></row>'
    )
  )
  df <- parse_tabular_response(xml)
  testthat::expect_equal(nrow(df), 2)
  testthat::expect_equal(df$COL1, c("a", "c"))
  testthat::expect_equal(df$COL2, c("b", "d"))
})

testthat::test_that("parse_multidimensional_response with axes and cells", {
  axes_xml <- paste0(
    '<Axis name="Axis0">',
    '<Tuples>',
    '<Tuple><Member><UName>[Measures].[Revenue]</UName><Caption>Revenue</Caption></Member></Tuple>',
    '</Tuples>',
    '</Axis>'
  )
  celldata_xml <- paste0(
    '<Cell CellOrdinal="0"><Value>100</Value><FmtValue>$100</FmtValue></Cell>',
    '<Cell CellOrdinal="1"><Value>200</Value><FmtValue>$200</FmtValue></Cell>'
  )
  xml <- xml2::read_xml(.mock_md_execute_response(axes_xml, celldata_xml))
  result <- parse_multidimensional_response(xml)
  
  testthat::expect_s3_class(result, "ExecuteResult")
  testthat::expect_length(result$axes, 1)
  testthat::expect_equal(result$axes[[1]]$name, "Axis0")
  testthat::expect_length(result$axes[[1]]$tuples, 1)
  testthat::expect_length(result$cell_data$cells, 2)
  testthat::expect_equal(result$cell_data$cells[[1]][["Value"]], "100")
  testthat::expect_equal(result$cell_data$cells[[1]][["CellOrdinal"]], "0")
})

testthat::test_that("parse_multidimensional_response empty response", {
  xml <- xml2::read_xml(
    paste0(
      '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/">',
      '<SOAP-ENV:Header/><SOAP-ENV:Body>',
      '<ExecuteResponse xmlns="urn:schemas-microsoft-com:xml-analysis">',
      '<return><root xmlns="urn:schemas-microsoft-com:xml-analysis:mddataset">',
      '</root></return></ExecuteResponse>',
      '</SOAP-ENV:Body></SOAP-ENV:Envelope>'
    )
  )
  result <- parse_multidimensional_response(xml)
  testthat::expect_s3_class(result, "ExecuteResult")
  testthat::expect_length(result$axes, 0)
  testthat::expect_length(result$cell_data$cells, 0)
})

testthat::test_that("ExecuteResult print method", {
  axis <- Axis$new(name = "Axis0", tuples = list(list()))
  cells <- CellData$new(cells = list(list(Value = "1")))
  er <- ExecuteResult$new(axes = list(axis), cell_data = cells)
  out <- capture.output(er$print())
  testthat::expect_true(any(grepl("ExecuteResult", out)))
  testthat::expect_true(any(grepl("Axis0", out)))
  testthat::expect_true(any(grepl("Cells: 1", out)))
})

testthat::test_that("xmla_execute tabular format returns data.frame", {
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      xml2::read_xml(
        .mock_tabular_execute_response(
          '<row xmlns="urn:schemas-microsoft-com:xml-analysis:rowset"><A>1</A></row>'
        )
      )
    }
  )
  
  conn <- Connection$new("http://localhost/xmla")
  df <- xmla_execute(conn, "SELECT 1", format = "Tabular")
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_equal(nrow(df), 1)
})

testthat::test_that("xmla_execute multidimensional format returns ExecuteResult", {
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      xml2::read_xml(
        .mock_md_execute_response(
          '<Axis name="Axis0"><Tuples><Tuple><Member><UName>M1</UName></Member></Tuple></Tuples></Axis>',
          '<Cell CellOrdinal="0"><Value>42</Value></Cell>'
        )
      )
    }
  )
  
  conn <- Connection$new("http://localhost/xmla")
  result <- xmla_execute(conn, "SELECT FROM [Cube]")
  testthat::expect_s3_class(result, "ExecuteResult")
  testthat::expect_length(result$axes, 1)
})

testthat::test_that("xmla_execute sets catalog property", {
  captured_xml <- NULL
  local_mocked_bindings(
    send_soap_request = function(url, soap_xml, auth) {
      captured_xml <<- soap_xml
      xml2::read_xml(.mock_tabular_execute_response())
    }
  )
  
  conn <- Connection$new("http://localhost/xmla")
  xmla_execute(conn, "SELECT 1", catalog = "Sales", format = "Tabular")
  testthat::expect_match(captured_xml, "<Catalog>Sales</Catalog>")
})
