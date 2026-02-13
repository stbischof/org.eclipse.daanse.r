testthat::test_that("Catalog_from_node creates Catalog", {
  xml <- xml2::read_xml(
    '<row><CATALOG_NAME>Sales</CATALOG_NAME><DESCRIPTION>Sales DB</DESCRIPTION><ROLES>admin</ROLES></row>'
  )
  obj <- Catalog_from_node(xml)
  testthat::expect_s3_class(obj, "Catalog")
  testthat::expect_equal(obj$catalog_name, "Sales")
  testthat::expect_equal(obj$description, "Sales DB")
  testthat::expect_equal(obj$roles, "admin")
})

testthat::test_that("Cube_from_node creates Cube", {
  xml <- xml2::read_xml(
    '<row><CATALOG_NAME>Sales</CATALOG_NAME><CUBE_NAME>SalesCube</CUBE_NAME><CUBE_TYPE>CUBE</CUBE_TYPE></row>'
  )
  obj <- Cube_from_node(xml)
  testthat::expect_s3_class(obj, "Cube")
  testthat::expect_equal(obj$cube_name, "SalesCube")
  testthat::expect_equal(obj$cube_type, "CUBE")
})

testthat::test_that("Dimension_from_node creates Dimension", {
  xml <- xml2::read_xml(
    '<row><CATALOG_NAME>Sales</CATALOG_NAME><CUBE_NAME>C</CUBE_NAME><DIMENSION_NAME>Time</DIMENSION_NAME><DIMENSION_UNIQUE_NAME>[Time]</DIMENSION_UNIQUE_NAME></row>'
  )
  obj <- Dimension_from_node(xml)
  testthat::expect_s3_class(obj, "Dimension")
  testthat::expect_equal(obj$dimension_name, "Time")
  testthat::expect_equal(obj$dimension_unique_name, "[Time]")
})

testthat::test_that("Hierarchy_from_node creates Hierarchy", {
  xml <- xml2::read_xml(
    '<row><CATALOG_NAME>Sales</CATALOG_NAME><CUBE_NAME>C</CUBE_NAME><DIMENSION_UNIQUE_NAME>[Time]</DIMENSION_UNIQUE_NAME><HIERARCHY_NAME>Year</HIERARCHY_NAME><HIERARCHY_UNIQUE_NAME>[Time].[Year]</HIERARCHY_UNIQUE_NAME></row>'
  )
  obj <- Hierarchy_from_node(xml)
  testthat::expect_s3_class(obj, "Hierarchy")
  testthat::expect_equal(obj$hierarchy_name, "Year")
  testthat::expect_equal(obj$hierarchy_unique_name, "[Time].[Year]")
})

testthat::test_that("Level_from_node creates Level", {
  xml <- xml2::read_xml(
    '<row><CATALOG_NAME>Sales</CATALOG_NAME><CUBE_NAME>C</CUBE_NAME><DIMENSION_UNIQUE_NAME>[Time]</DIMENSION_UNIQUE_NAME><HIERARCHY_UNIQUE_NAME>[Time].[Year]</HIERARCHY_UNIQUE_NAME><LEVEL_NAME>Year</LEVEL_NAME><LEVEL_UNIQUE_NAME>[Time].[Year].[Year]</LEVEL_UNIQUE_NAME><LEVEL_NUMBER>1</LEVEL_NUMBER></row>'
  )
  obj <- Level_from_node(xml)
  testthat::expect_s3_class(obj, "Level")
  testthat::expect_equal(obj$level_name, "Year")
  testthat::expect_equal(obj$level_number, 1L)
})

testthat::test_that("Measure_from_node creates Measure", {
  xml <- xml2::read_xml(
    '<row><CATALOG_NAME>Sales</CATALOG_NAME><CUBE_NAME>C</CUBE_NAME><MEASURE_NAME>Revenue</MEASURE_NAME><MEASURE_UNIQUE_NAME>[Measures].[Revenue]</MEASURE_UNIQUE_NAME><DATA_TYPE>5</DATA_TYPE></row>'
  )
  obj <- Measure_from_node(xml)
  testthat::expect_s3_class(obj, "Measure")
  testthat::expect_equal(obj$measure_name, "Revenue")
  testthat::expect_equal(obj$measure_unique_name, "[Measures].[Revenue]")
})

testthat::test_that("Member_from_node creates Member", {
  xml <- xml2::read_xml(
    '<row><CATALOG_NAME>Sales</CATALOG_NAME><CUBE_NAME>C</CUBE_NAME><DIMENSION_UNIQUE_NAME>[Time]</DIMENSION_UNIQUE_NAME><HIERARCHY_UNIQUE_NAME>[Time].[Year]</HIERARCHY_UNIQUE_NAME><LEVEL_UNIQUE_NAME>[Time].[Year].[Year]</LEVEL_UNIQUE_NAME><MEMBER_NAME>2024</MEMBER_NAME><MEMBER_UNIQUE_NAME>[Time].[Year].&amp;[2024]</MEMBER_UNIQUE_NAME><MEMBER_TYPE>1</MEMBER_TYPE></row>'
  )
  obj <- Member_from_node(xml)
  testthat::expect_s3_class(obj, "Member")
  testthat::expect_equal(obj$member_name, "2024")
  testthat::expect_equal(obj$member_type, "1")
})

testthat::test_that("MdSet_from_node creates MdSet", {
  xml <- xml2::read_xml(
    '<row><CATALOG_NAME>Sales</CATALOG_NAME><CUBE_NAME>C</CUBE_NAME><SET_NAME>Top10</SET_NAME><SET_CAPTION>Top 10</SET_CAPTION><SCOPE>1</SCOPE></row>'
  )
  obj <- MdSet_from_node(xml)
  testthat::expect_s3_class(obj, "MdSet")
  testthat::expect_equal(obj$set_name, "Top10")
  testthat::expect_equal(obj$set_caption, "Top 10")
})

testthat::test_that("DataSource_from_node creates DataSource", {
  xml <- xml2::read_xml(
    '<row><DataSourceName>MyDS</DataSourceName><URL>http://localhost</URL><ProviderName>Mondrian</ProviderName><AuthenticationMode>Unauthenticated</AuthenticationMode></row>'
  )
  obj <- DataSource_from_node(xml)
  testthat::expect_s3_class(obj, "DataSource")
  testthat::expect_equal(obj$data_source_name, "MyDS")
  testthat::expect_equal(obj$url, "http://localhost")
  testthat::expect_equal(obj$provider_name, "Mondrian")
})

testthat::test_that("Property_from_node creates Property", {
  xml <- xml2::read_xml(
    '<row><CATALOG_NAME>Sales</CATALOG_NAME><PropertyName>Catalog</PropertyName><PropertyDescription>Catalog name</PropertyDescription><PropertyType>string</PropertyType><PropertyAccessType>ReadWrite</PropertyAccessType><IsRequired>true</IsRequired><Value>Sales</Value></row>'
  )
  obj <- Property_from_node(xml)
  testthat::expect_s3_class(obj, "Property")
  testthat::expect_equal(obj$property_name, "Catalog")
  testthat::expect_true(obj$is_required)
  testthat::expect_equal(obj$value, "Sales")
})
