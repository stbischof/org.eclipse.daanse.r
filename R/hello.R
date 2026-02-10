#' Say hello
#'
#' Returns a greeting string.
#'
#' @param name Character scalar. Name to greet.
#' @return A character scalar greeting.
#' @export
hello <- function(name = "Eclipse") {
  paste0("Hello, ", name, "!")
}
