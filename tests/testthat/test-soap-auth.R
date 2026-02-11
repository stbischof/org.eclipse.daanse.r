get_req_headers <- function(req) {
  # httr2 can be in $headers or $options$headers
  if (!is.null(req$headers))
    return(req$headers)
  if (!is.null(req$options$headers))
    return(req$options$headers)
  list()
}


testthat::test_that("NoAuth - no changed", {
  auth <- NoAuth$new()
  req  <- httr2::request("https://example.org")
  
  reqWithAuth <- auth$apply_auth(req)
  
  testthat::expect_equal(get_req_headers(reqWithAuth), get_req_headers(req))
})



testthat::test_that("BasicAuth adds Authorization header", {
  auth <- BasicAuth$new("user", "pass")
  req  <- httr2::request("https://example.org")
  
  reqWithAuth <- auth$apply_auth(req)
  
  expect_true(!is.null(reqWithAuth$headers))
  
  
})
