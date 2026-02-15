#' @title MDX Composite Expression Helpers
#' @description Hand-written helper functions that compose multiple MDX
#' operations into common analytical patterns. These complement the
#' auto-generated function wrappers.

# --- Axis Set Construction Helpers ---

#' Create a set of explicit members wrapped in braces
#'
#' Wraps the given member unique names in \code{\{...\}} to form an MDX set
#' expression suitable for axis definitions.
#'
#' @param ... Member unique name strings.
#' @return MDX set expression string.
#' @export
mdx_member_set <- function(...) {
  members <- c(...)
  paste0("{", paste(members, collapse = ", "), "}")
}

#' Get all members of a level as a set (with braces)
#'
#' Returns \code{\{level.Members\}} — the level's members wrapped in set
#' braces, ready for use in axis expressions.
#'
#' @param level Level unique name string.
#' @return MDX set expression string.
#' @export
mdx_level_member_set <- function(level) {
  paste0("{", level, ".Members}")
}

# --- Calculated Member Expression Helpers ---

#' Build expression: ratio of two measures
#' @param numerator Measure unique name for numerator.
#' @param denominator Measure unique name for denominator.
#' @return MDX expression string.
#' @export
mdx_ratio <- function(numerator, denominator) {
  paste0(numerator, " / ", denominator)
}

#' Build expression: difference of two measures
#' @param measure1 First measure unique name.
#' @param measure2 Second measure unique name.
#' @return MDX expression string.
#' @export
mdx_diff <- function(measure1, measure2) {
  paste0(measure1, " - ", measure2)
}

#' Build expression: sum of measures
#' @param ... Measure unique names.
#' @return MDX expression string.
#' @export
mdx_sum_expr <- function(...) {
  measures <- c(...)
  paste(measures, collapse = " + ")
}

#' Build expression: product of measures
#' @param ... Measure unique names.
#' @return MDX expression string.
#' @export
mdx_product <- function(...) {
  measures <- c(...)
  paste(measures, collapse = " * ")
}

#' Build expression: percentage of total
#' @param measure Measure unique name.
#' @param total_member Member expression for total (e.g., "[Product].[All Products]").
#' @return MDX expression string.
#' @export
mdx_pct_of_total <- function(measure, total_member) {
  paste0(measure, " / (", total_member, ", ", measure, ") * 100")
}

#' Build expression: year-over-year growth
#' @param measure Measure unique name.
#' @param time_hierarchy Time hierarchy unique name.
#' @return MDX expression string for (current - prior) / prior.
#' @export
mdx_yoy_growth <- function(measure, time_hierarchy) {
  prior <- paste0("(",
                  time_hierarchy,
                  ".CurrentMember.PrevMember, ",
                  measure,
                  ")")
  current <- measure
  paste0("IIF(",
         prior,
         " = 0, NULL, (",
         current,
         " - ",
         prior,
         ") / ",
         prior,
         ")")
}

#' Build expression: running sum / cumulative total
#' @param measure Measure unique name.
#' @param hierarchy Hierarchy unique name for the running dimension.
#' @return MDX expression string.
#' @export
mdx_running_sum <- function(measure, hierarchy) {
  paste0(
    "Sum(",
    hierarchy,
    ".CurrentMember.Level.Members.Item(0) : ",
    hierarchy,
    ".CurrentMember, ",
    measure,
    ")"
  )
}

#' Build expression: rank within a set
#' @param set Set expression.
#' @param measure Measure to rank by.
#' @return MDX expression string.
#' @export
mdx_rank_expr <- function(set, measure) {
  paste0("Rank(", set, ".Current, ", set, ", ", measure, ")")
}

# --- Missing MDX function wrappers ---

#' MDX CoalesceEmpty function
#'
#' Returns the first non-empty expression from the arguments.
#'
#' @param ... Two or more expressions to coalesce.
#' @return MDX CoalesceEmpty expression string.
#' @export
mdx_coalesce_empty <- function(...) {
  args <- c(...)
  paste0("CoalesceEmpty(", paste(args, collapse = ", "), ")")
}

#' MDX Dimension Name function
#'
#' Returns the name of a dimension.
#'
#' @param dimension Dimension expression.
#' @return MDX expression string returning the dimension name.
#' @export
mdx_dimension_name <- function(dimension) {
  paste0(dimension, ".Name")
}

#' MDX Intersect function
#'
#' Returns the intersection of two sets.
#'
#' @param set1 First set expression.
#' @param set2 Second set expression.
#' @param all Optional logical; if TRUE, uses the ALL flag to retain duplicates.
#' @return MDX Intersect expression string.
#' @export
mdx_intersect <- function(set1, set2, all = FALSE) {
  if (all) {
    paste0("Intersect(", set1, ", ", set2, ", ALL)")
  } else {
    paste0("Intersect(", set1, ", ", set2, ")")
  }
}

#' MDX Order function
#'
#' Orders members of a set according to an expression.
#'
#' @param set Set expression to order.
#' @param expression Numeric or string expression to sort by.
#' @param direction Sort direction: "ASC", "DESC", "BASC", or "BDESC".
#' @return MDX Order expression string.
#' @export
mdx_order <- function(set, expression, direction = "ASC") {
  paste0("Order(", set, ", ", expression, ", ", direction, ")")
}

#' MDX TopCount function (alias)
#'
#' Returns a specified number of items from the top of a set,
#' optionally ordered by an expression.
#'
#' @param set Set expression.
#' @param count Number of items to return.
#' @param expression Optional numeric expression to order by.
#' @return MDX TopCount expression string.
#' @export
mdx_topcount <- function(set, count, expression = NULL) {
  if (is.null(expression)) {
    paste0("TopCount(", set, ", ", count, ")")
  } else {
    paste0("TopCount(", set, ", ", count, ", ", expression, ")")
  }
}

#' MDX VisualTotals function
#'
#' Returns a set generated by dynamically totaling child members
#' in a specified set.
#'
#' @param set Set expression.
#' @param pattern Optional pattern string for the visual totals caption.
#' @return MDX VisualTotals expression string.
#' @export
mdx_visual_totals <- function(set, pattern = NULL) {
  if (is.null(pattern)) {
    paste0("VisualTotals(", set, ")")
  } else {
    paste0("VisualTotals(", set, ", ", pattern, ")")
  }
}
