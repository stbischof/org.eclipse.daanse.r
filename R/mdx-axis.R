#' @title MDX Axis Construction
#' @description Functions for building MDX axis set expressions.

#' Get all members of a hierarchy as a set (with braces)
#' @param hierarchy Hierarchy unique name string.
#' @return MDX set expression string.
#' @export
mdx_hierarchy_members <- function(hierarchy) {
  paste0("{", hierarchy, ".Members}")
}

#' CrossJoin two or more sets
#' @param ... Set expression strings.
#' @return MDX CrossJoin expression string.
#' @export
mdx_crossjoin <- function(...) {
  sets <- list(...)
  if (length(sets) < 2)
    stop("mdx_crossjoin requires at least 2 sets")
  result <- sets[[1]]
  for (i in 2:length(sets)) {
    result <- paste0("CrossJoin(", result, ", ", sets[[i]], ")")
  }
  result
}

#' Create an aggregate filter expression for a dimension
#' @param dimension Dimension unique name.
#' @param members Character vector of member unique names.
#' @return MDX Aggregate expression for use in WHERE clause.
#' @export
mdx_aggregate_filter <- function(dimension, members) {
  member_set <- paste0("{", paste(members, collapse = ", "), "}")
  paste0("Aggregate(", member_set, ")")
}
