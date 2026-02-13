testthat::test_that("xml_node_to_list extracts children", {
  doc <- xml2::read_xml("<row><A>1</A><B>hello</B></row>")
  result <- xml_node_to_list(doc)
  testthat::expect_equal(result$A, "1")
  testthat::expect_equal(result$B, "hello")
})

testthat::test_that("xml_nodes_to_df produces correct data.frame", {
  xml <- xml2::read_xml("<root><row><X>1</X><Y>a</Y></row><row><X>2</X><Y>b</Y></row></root>")
  nodes <- xml2::xml_children(xml)
  df <- xml_nodes_to_df(nodes)
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_equal(nrow(df), 2)
  testthat::expect_equal(df$X, c("1", "2"))
  testthat::expect_equal(df$Y, c("a", "b"))
})

testthat::test_that("xml_nodes_to_df handles empty input", {
  df <- xml_nodes_to_df(list())
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_equal(nrow(df), 0)
})

testthat::test_that("xml_nodes_to_df handles missing columns", {
  xml <- xml2::read_xml("<root><row><X>1</X><Y>a</Y></row><row><X>2</X></row></root>")
  nodes <- xml2::xml_children(xml)
  df <- xml_nodes_to_df(nodes)
  testthat::expect_equal(nrow(df), 2)
  testthat::expect_true(is.na(df$Y[2]))
})

testthat::test_that("xml_child_text extracts text", {
  doc <- xml2::read_xml("<row><NAME>hello</NAME></row>")
  testthat::expect_equal(xml_child_text(doc, "NAME"), "hello")
})

testthat::test_that("xml_child_text returns NA for missing element", {
  doc <- xml2::read_xml("<row><NAME>hello</NAME></row>")
  testthat::expect_true(is.na(xml_child_text(doc, "MISSING")))
})

testthat::test_that("xml_child_integer extracts integer", {
  doc <- xml2::read_xml("<row><NUM>42</NUM></row>")
  testthat::expect_equal(xml_child_integer(doc, "NUM"), 42L)
})

testthat::test_that("xml_child_integer returns NA for missing element", {
  doc <- xml2::read_xml("<row><NUM>42</NUM></row>")
  testthat::expect_true(is.na(xml_child_integer(doc, "MISSING")))
})

testthat::test_that("xml_child_logical extracts logical", {
  doc <- xml2::read_xml("<row><FLAG>true</FLAG></row>")
  testthat::expect_true(xml_child_logical(doc, "FLAG"))
  
  doc2 <- xml2::read_xml("<row><FLAG>false</FLAG></row>")
  testthat::expect_false(xml_child_logical(doc2, "FLAG"))
  
  doc3 <- xml2::read_xml("<row><FLAG>1</FLAG></row>")
  testthat::expect_true(xml_child_logical(doc3, "FLAG"))
  
  doc4 <- xml2::read_xml("<row><FLAG>yes</FLAG></row>")
  testthat::expect_true(xml_child_logical(doc4, "FLAG"))
})

testthat::test_that("xml_child_logical returns NA for missing element", {
  doc <- xml2::read_xml("<row><FLAG>true</FLAG></row>")
  testthat::expect_true(is.na(xml_child_logical(doc, "MISSING")))
})

testthat::test_that("xmla_decode_names decodes hex-encoded names", {
  testthat::expect_equal(xmla_decode_names("_x0020_"), " ")
  testthat::expect_equal(xmla_decode_names("Column_x0020_Name"), "Column Name")
  testthat::expect_equal(xmla_decode_names("plain"), "plain")
  testthat::expect_equal(xmla_decode_names("_x005B_Measures_x005D_"), "[Measures]")
})
