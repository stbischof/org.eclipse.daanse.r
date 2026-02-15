#' Return NULL if value is empty or NA
#' @param x A value to check.
#' @return The value or NULL.
#' @export
null_if_empty <- function(x) {
  if (is.null(x) || length(x) == 0)
    return(NULL)
  if (is.character(x) && all(is.na(x) | x == ""))
    return(NULL)
  if (all(is.na(x)))
    return(NULL)
  x
}

#' Return default if value is NULL or NA
#' @param x A value to check.
#' @param default The default value.
#' @return The value or the default.
#' @export
optional_value <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)))
    return(default)
  x
}

#' Ensure value is a list
#' @param x A value to convert.
#' @return A list.
#' @export
ensure_list <- function(x) {
  if (is.null(x))
    return(list())
  if (!is.list(x))
    return(list(x))
  x
}
