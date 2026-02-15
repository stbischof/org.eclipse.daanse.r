# --- MDX Axis Helpers ---

testthat::test_that("mdx_member_set creates member set", {
  testthat::expect_equal(
    mdx_member_set("[Measures].[Sales]", "[Measures].[Cost]"),
    "{[Measures].[Sales], [Measures].[Cost]}"
  )
})

testthat::test_that("mdx_hierarchy_members creates hierarchy set", {
  testthat::expect_equal(
    mdx_hierarchy_members("[Product].[Category]"),
    "{[Product].[Category].Members}"
  )
})

testthat::test_that("mdx_level_member_set creates level set", {
  testthat::expect_equal(
    mdx_level_member_set("[Time].[Year].[Year]"),
    "{[Time].[Year].[Year].Members}"
  )
})

testthat::test_that("mdx_crossjoin nests for 2 sets", {
  testthat::expect_equal(
    mdx_crossjoin("{A}", "{B}"),
    "CrossJoin({A}, {B})"
  )
})

testthat::test_that("mdx_crossjoin nests for 3 sets", {
  testthat::expect_equal(
    mdx_crossjoin("{A}", "{B}", "{C}"),
    "CrossJoin(CrossJoin({A}, {B}), {C})"
  )
})

testthat::test_that("mdx_crossjoin errors with 1 set", {
  testthat::expect_error(mdx_crossjoin("{A}"), "at least 2")
})

testthat::test_that("mdx_hierarchize without POST", {
  testthat::expect_equal(mdx_hierarchize("{S}"), "Hierarchize({S})")
})

testthat::test_that("mdx_hierarchize with POST", {
  testthat::expect_equal(mdx_hierarchize("{S}", "POST"), "Hierarchize({S}, POST)")
})

testthat::test_that("mdx_drilldown_level without level", {
  testthat::expect_equal(mdx_drilldown_level("{S}"), "DrilldownLevel({S})")
})

testthat::test_that("mdx_drilldown_level with level", {
  testthat::expect_equal(
    mdx_drilldown_level("{S}", "[Time].[Year]"),
    "DrilldownLevel({S}, [Time].[Year])"
  )
})

testthat::test_that("mdx_aggregate_filter creates aggregate", {
  testthat::expect_equal(
    mdx_aggregate_filter("[Product]", c("[Product].[A]", "[Product].[B]")),
    "Aggregate({[Product].[A], [Product].[B]})"
  )
})

# --- MDX Functions ---

testthat::test_that("mdx_descendants with and without level", {
  testthat::expect_equal(mdx_descendants("[Time].[All]"), "Descendants([Time].[All])")
  testthat::expect_equal(
    mdx_descendants("[Time].[All]", "[Time].[Month]"),
    "Descendants([Time].[All], [Time].[Month])"
  )
})

testthat::test_that("mdx_children and mdx_parent", {
  testthat::expect_equal(mdx_children("[Time].[2024]"), "[Time].[2024].Children")
  testthat::expect_equal(mdx_parent("[Time].[Q1]"), "[Time].[Q1].Parent")
})

testthat::test_that("mdx_union, intersect, except", {
  testthat::expect_equal(mdx_union("{A}", "{B}"), "Union({A}, {B})")
  testthat::expect_equal(mdx_intersect("{A}", "{B}"), "Intersect({A}, {B})")
  testthat::expect_equal(mdx_except("{A}", "{B}"), "Except({A}, {B})")
})

testthat::test_that("mdx_filter and mdx_order", {
  testthat::expect_equal(
    mdx_filter("{S}", "[Measures].[Sales] > 100"),
    "Filter({S}, [Measures].[Sales] > 100)"
  )
  testthat::expect_equal(
    mdx_order("{S}", "[Measures].[Sales]", "DESC"),
    "Order({S}, [Measures].[Sales], DESC)"
  )
})

testthat::test_that("mdx_topcount, head, tail, subset", {
  testthat::expect_equal(
    mdx_topcount("{S}", 10, "[Measures].[Sales]"),
    "TopCount({S}, 10, [Measures].[Sales])"
  )
  testthat::expect_equal(mdx_head("{S}", 5), "Head({S}, 5)")
  testthat::expect_equal(mdx_tail("{S}", 5), "Tail({S}, 5)")
  testthat::expect_equal(mdx_subset("{S}", 0, 10), "Subset({S}, 0, 10)")
})

testthat::test_that("mdx_distinct and mdx_non_empty", {
  testthat::expect_equal(mdx_distinct("{S}"), "Distinct({S})")
  testthat::expect_equal(mdx_non_empty("{S}"), "NonEmpty({S})")
  testthat::expect_equal(
    mdx_non_empty("{S}", "[Measures].[Sales]"),
    "NonEmpty({S}, [Measures].[Sales])"
  )
})

testthat::test_that("mdx_ratio, diff, sum, product", {
  testthat::expect_equal(mdx_ratio("A", "B"), "A / B")
  testthat::expect_equal(mdx_diff("A", "B"), "A - B")
  testthat::expect_equal(mdx_sum_expr("A", "B", "C"), "A + B + C")
  testthat::expect_equal(mdx_product("A", "B"), "A * B")
})

testthat::test_that("mdx_iif, coalesce_empty, format", {
  testthat::expect_equal(mdx_iif("X > 0", "X", "0"), "IIf(X > 0, X, 0)")
  testthat::expect_equal(mdx_coalesce_empty("A", "B"), "CoalesceEmpty(A, B)")
  testthat::expect_equal(mdx_format("X", "#,##0"), "Format(X, #,##0)")
})

testthat::test_that("mdx_yoy_growth produces correct expression", {
  result <- mdx_yoy_growth("[Measures].[Sales]", "[Time].[Year]")
  testthat::expect_match(result, "PrevMember")
  testthat::expect_match(result, "IIF")
})

testthat::test_that("mdx_running_sum produces correct expression", {
  result <- mdx_running_sum("[Measures].[Sales]", "[Time].[Year]")
  testthat::expect_match(result, "Sum")
  testthat::expect_match(result, "CurrentMember")
})

testthat::test_that("mdx_rank produces correct expression", {
  result <- mdx_rank("{S}", "[Measures].[Sales]")
  testthat::expect_equal(result, "Rank({S}, [Measures].[Sales])")
})

testthat::test_that("mdx_rank_expr produces rank with Current", {
  result <- mdx_rank_expr("{S}", "[Measures].[Sales]")
  testthat::expect_equal(result, "Rank({S}.Current, {S}, [Measures].[Sales])")
})

# --- MdxBuilder ---

testthat::test_that("MdxBuilder basic query with columns", {
  mdx <- MdxBuilder$new("[Sales]")$
    on_columns(mdx_member_set("[Measures].[Sales]"))$
    to_mdx()
  testthat::expect_match(mdx, "SELECT")
  testthat::expect_match(mdx, "ON COLUMNS")
  testthat::expect_match(mdx, "FROM \\[Sales\\]")
})

testthat::test_that("MdxBuilder with columns and rows", {
  mdx <- MdxBuilder$new("[Sales]")$
    on_columns(mdx_member_set("[Measures].[Sales]"))$
    on_rows(mdx_hierarchy_members("[Product].[Category]"))$
    to_mdx()
  testthat::expect_match(mdx, "ON COLUMNS")
  testthat::expect_match(mdx, "ON ROWS")
})

testthat::test_that("MdxBuilder NON EMPTY", {
  mdx <- MdxBuilder$new("[Sales]")$
    on_columns(mdx_member_set("[Measures].[Sales]"))$
    non_empty()$
    to_mdx()
  testthat::expect_match(mdx, "NON EMPTY")
})

testthat::test_that("MdxBuilder WHERE clause", {
  mdx <- MdxBuilder$new("[Sales]")$
    on_columns(mdx_member_set("[Measures].[Sales]"))$
    where("[Time].[2024]")$
    to_mdx()
  testthat::expect_match(mdx, "WHERE \\[Time\\]\\.\\[2024\\]")
})

testthat::test_that("MdxBuilder WITH MEMBER", {
  mdx <- MdxBuilder$new("[Sales]")$
    with_member("[Measures].[Calc]", "[Measures].[A] + [Measures].[B]")$
    on_columns(mdx_member_set("[Measures].[Calc]"))$
    to_mdx()
  testthat::expect_match(mdx, "WITH MEMBER \\[Measures\\]\\.\\[Calc\\] AS")
})

testthat::test_that("MdxBuilder with_calculated_measure", {
  mdx <- MdxBuilder$new("[Sales]")$
    with_calculated_measure("Profit", "[Measures].[Sales] - [Measures].[Cost]")$
    on_columns(mdx_member_set("[Measures].[Profit]"))$
    to_mdx()
  testthat::expect_match(mdx, "MEMBER \\[Measures\\]\\.\\[Profit\\] AS")
})

testthat::test_that("MdxBuilder with_member format_string", {
  mdx <- MdxBuilder$new("[Sales]")$
    with_member("[Measures].[Pct]", "1.0", format_string = "0.00%")$
    on_columns(mdx_member_set("[Measures].[Pct]"))$
    to_mdx()
  testthat::expect_match(mdx, 'FORMAT_STRING = "0.00%"')
})

testthat::test_that("MdxBuilder WITH SET", {
  mdx <- MdxBuilder$new("[Sales]")$
    with_set("[TopProducts]", mdx_topcount("{[Product].Members}", 10, "[Measures].[Sales]"))$
    on_rows("[TopProducts]")$
    on_columns(mdx_member_set("[Measures].[Sales]"))$
    to_mdx()
  testthat::expect_match(mdx, "SET \\[TopProducts\\] AS")
})

testthat::test_that("MdxBuilder CELL PROPERTIES", {
  mdx <- MdxBuilder$new("[Sales]")$
    on_columns(mdx_member_set("[Measures].[Sales]"))$
    cell_properties("VALUE", "FORMATTED_VALUE")$
    to_mdx()
  testthat::expect_match(mdx, "CELL PROPERTIES VALUE, FORMATTED_VALUE")
})

testthat::test_that("MdxBuilder DIMENSION PROPERTIES", {
  mdx <- MdxBuilder$new("[Sales]")$
    on_columns(mdx_member_set("[Measures].[Sales]"))$
    on_rows(mdx_hierarchy_members("[Product].[Category]"))$
    dimension_properties("MEMBER_CAPTION", "PARENT_UNIQUE_NAME")$
    to_mdx()
  testthat::expect_match(mdx, "DIMENSION PROPERTIES MEMBER_CAPTION, PARENT_UNIQUE_NAME")
})

testthat::test_that("MdxBuilder print method", {
  b <- MdxBuilder$new("[Sales]")$
    on_columns(mdx_member_set("[Measures].[Sales]"))
  out <- capture.output(b$print())
  testthat::expect_true(any(grepl("MdxBuilder", out)))
  testthat::expect_true(any(grepl("Sales", out)))
})

# --- Utility Helpers ---

testthat::test_that("null_if_empty returns NULL for empty", {
  testthat::expect_null(null_if_empty(NULL))
  testthat::expect_null(null_if_empty(character(0)))
  testthat::expect_null(null_if_empty(c(NA_character_)))
  testthat::expect_null(null_if_empty(""))
  testthat::expect_equal(null_if_empty("a"), "a")
  testthat::expect_equal(null_if_empty(42), 42)
})

testthat::test_that("optional_value returns default for NULL/NA", {
  testthat::expect_equal(optional_value(NULL, "default"), "default")
  testthat::expect_equal(optional_value(NA, "default"), "default")
  testthat::expect_equal(optional_value("val", "default"), "val")
})

testthat::test_that("ensure_list converts properly", {
  testthat::expect_equal(ensure_list(NULL), list())
  testthat::expect_equal(ensure_list("a"), list("a"))
  testthat::expect_equal(ensure_list(list("a")), list("a"))
})
