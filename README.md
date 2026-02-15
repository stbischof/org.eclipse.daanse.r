# eclipse.daanse.xmla

An R client for [XMLA (XML for Analysis)](https://learn.microsoft.com/en-us/analysis-services/xmla/xml-for-analysis-xmla-reference) servers like daanse cube server. Connect to any XMLA-compliant OLAP server, discover metadata (catalogs, cubes, dimensions, measures, ...), and execute MDX queries -- all from R.

## Installation

Install directly from GitHub using the `remotes` package:

```r
# install.packages("remotes")
remotes::install_github("eclipse-daanse/org.eclipse.daanse.r.xmla")
```

## Quick Start

```r
install.packages(".", repos = NULL, type = "source")
library(eclipse.daanse.xmla)

# Connect
conn <- Connection$new("http://localhost:8080/xmla",
                        auth = BasicAuth$new("user", "pass"))

# Discover what's available
catalogs <- discover_catalogs(conn)
cubes    <- discover_cubes(conn, catalogs[[1]]$catalog_name)

# Build and run an MDX query
result <- MdxBuilder$new("[Sales]")$
  on_columns(mdx_member_set("[Measures].[Revenue]", "[Measures].[Cost]"))$
  on_rows(mdx_hierarchy_members("[Product].[Category]"))$
  non_empty()$
  execute(conn, catalog = "Sales")

# Or get a flat data.frame directly
df <- MdxBuilder$new("[Sales]")$
  on_columns(mdx_member_set("[Measures].[Revenue]"))$
  on_rows(mdx_hierarchy_members("[Product].[Category]"))$
  non_empty()$
  execute(conn, catalog = "Sales", format = "Tabular")
```

A full working example that exercises every exported function lives in `inst/examples/discover-and-query.R`:

```r
source(system.file("examples", "discover-and-query.R", package = "eclipse.daanse.xmla"))
```

## Connection and Authentication

Every operation starts with a `Connection` object pointing at the XMLA endpoint.

```r
# Basic auth (username + password)
conn <- Connection$new("http://server:8080/xmla",
                        auth = BasicAuth$new("user", "pass"))

# Anonymous / no auth
conn <- Connection$new("http://server:8080/xmla",
                        auth = NoAuth$new())
```

## Metadata Discovery

The `discover_*` functions let you explore the server's metadata. Each returns a list of R6 objects with typed fields.

```r
datasources <- discover_datasources(conn)
catalogs    <- discover_catalogs(conn)
cubes       <- discover_cubes(conn, "MyCatalog")
dimensions  <- discover_dimensions(conn, "MyCatalog", "MyCube")
hierarchies <- discover_hierarchies(conn, "MyCatalog", "MyCube")
levels      <- discover_levels(conn, "MyCatalog", "MyCube")
measures    <- discover_measures(conn, "MyCatalog", "MyCube")
members     <- discover_members(conn, "MyCatalog", "MyCube",
                                 hierarchy = "[Product].[Category]")
sets        <- discover_sets(conn, "MyCatalog", "MyCube")
properties  <- discover_properties(conn, "MyCatalog", "MyCube")
```

You can also filter hierarchies by dimension and levels by hierarchy:

```r
hierarchies <- discover_hierarchies(conn, "MyCatalog", "MyCube",
                                     dimension = "[Product]")
levels      <- discover_levels(conn, "MyCatalog", "MyCube",
                                hierarchy = "[Product].[Category]")
```

For raw access, `xmla_discover()` returns a plain `data.frame`:

```r
df <- xmla_discover(conn, "DBSCHEMA_CATALOGS")
```

## Executing MDX Queries

Use `xmla_execute()` to run a raw MDX string, or `MdxBuilder` (see below) for a fluent API.

```r
# Multidimensional result (axes + cells)
result <- xmla_execute(conn,
  "SELECT {[Measures].[Revenue]} ON COLUMNS,
          {[Product].[Category].Members} ON ROWS
   FROM [Sales]",
  catalog = "MyCatalog")

result$axes       # list of Axis objects
result$cell_data  # CellData object with $cells

# Tabular result (flat data.frame)
df <- xmla_execute(conn,
  "SELECT {[Measures].[Revenue]} ON COLUMNS FROM [Sales]",
  catalog = "MyCatalog",
  format = "Tabular")
```

## MDX Builder

`MdxBuilder` provides a fluent, chainable API for constructing MDX queries without writing raw strings.

### Basic query

```r
mdx <- MdxBuilder$new("[Sales]")$
  on_columns(mdx_member_set("[Measures].[Revenue]"))$
  on_rows(mdx_hierarchy_members("[Product].[Category]"))$
  non_empty()$
  to_mdx()
```

Produces:

```
SELECT
NON EMPTY {[Measures].[Revenue]} ON COLUMNS,
NON EMPTY {[Product].[Category].Members} ON ROWS
FROM [Sales]
CELL PROPERTIES VALUE, FORMAT_STRING, FORMATTED_VALUE
```

### Columns and rows

`on_columns()` sets axis 0, `on_rows()` sets axis 1. Pass any MDX set expression:

```r
MdxBuilder$new("[Sales]")$
  on_columns(mdx_member_set("[Measures].[Revenue]", "[Measures].[Cost]"))$
  on_rows(mdx_level_member_set("[Time].[Year].[Year]"))$
  non_empty()
```

### WHERE clause (slicer)

```r
MdxBuilder$new("[Sales]")$
  on_columns(mdx_member_set("[Measures].[Revenue]"))$
  where("[Time].[2024]")
```

### Calculated members

```r
MdxBuilder$new("[Sales]")$
  with_calculated_measure(
    "Profit",
    "[Measures].[Revenue] - [Measures].[Cost]",
    format_string = "#,##0.00")$
  on_columns(mdx_member_set("[Measures].[Revenue]",
                             "[Measures].[Cost]",
                             "[Measures].[Profit]"))$
  on_rows(mdx_hierarchy_members("[Product].[Category]"))$
  non_empty()
```

Or use the expression helpers for composability:

```r
MdxBuilder$new("[Sales]")$
  with_calculated_measure("Profit",
    mdx_diff("[Measures].[Revenue]", "[Measures].[Cost]"))$
  with_calculated_measure("Margin",
    mdx_ratio("[Measures].[Profit]", "[Measures].[Revenue]"),
    format_string = "0.00%")
```

### Named sets

```r
MdxBuilder$new("[Sales]")$
  with_set("[Top10]",
    mdx_topcount(mdx_hierarchy_members("[Product].[Category]"), 10,
                 "[Measures].[Revenue]"))$
  on_columns(mdx_member_set("[Measures].[Revenue]"))$
  on_rows("[Top10]")$
  non_empty()
```

### Properties

```r
MdxBuilder$new("[Sales]")$
  on_columns(mdx_member_set("[Measures].[Revenue]"))$
  on_rows(mdx_hierarchy_members("[Product].[Category]"))$
  cell_properties("VALUE", "FORMATTED_VALUE")$
  dimension_properties("MEMBER_CAPTION", "PARENT_UNIQUE_NAME")
```

### Execute directly

```r
# Multidimensional
result <- builder$execute(conn, catalog = "MyCatalog")

# Tabular (returns data.frame)
df <- builder$execute(conn, catalog = "MyCatalog", format = "Tabular")
```

## MDX Concepts

MDX (Multidimensional Expressions) is the query language for OLAP cubes. Here are the key concepts you need to build queries with this package.

### Cubes, Dimensions, Hierarchies, Levels, Members

An OLAP **cube** organizes data along multiple **dimensions** (e.g., Time, Product, Geography). Each dimension has one or more **hierarchies** (e.g., `[Time].[Calendar]`), which contain **levels** (Year > Quarter > Month) and **members** (the actual values like `[Time].[2024]`).

**Measures** (e.g., Revenue, Cost) are the numeric facts stored in the cube. They live in a special `[Measures]` dimension.

### Axes and sets

An MDX query places **sets** of members on **axes**:

- **COLUMNS** (axis 0) -- typically measures
- **ROWS** (axis 1) -- typically dimension members

A **set** is a collection of members enclosed in braces: `{[Measures].[Revenue], [Measures].[Cost]}`.

### Key MDX set functions

This package wraps all standard MDX functions as `mdx_*()` helpers. The most common ones:

| Function | R helper | Example output |
|----------|----------|----------------|
| Explicit member set | `mdx_member_set(m1, m2)` | `{m1, m2}` |
| Hierarchy members | `mdx_hierarchy_members(h)` | `{h.Members}` |
| Level members | `mdx_level_member_set(lv)` | `{lv.Members}` |
| Children | `mdx_children(member)` | `member.Children` |
| Descendants | `mdx_descendants(member, level)` | `Descendants(member, level)` |
| Filter | `mdx_filter(set, condition)` | `Filter(set, condition)` |
| Order | `mdx_order(set, expr, "DESC")` | `Order(set, expr, DESC)` |
| TopCount | `mdx_topcount(set, n, expr)` | `TopCount(set, n, expr)` |
| Head / Tail | `mdx_head(set, n)` | `Head(set, n)` |
| CrossJoin | `mdx_crossjoin(set1, set2)` | `CrossJoin(set1, set2)` |
| Union | `mdx_union(set1, set2)` | `Union(set1, set2)` |
| Except | `mdx_except(set1, set2)` | `Except(set1, set2)` |
| Intersect | `mdx_intersect(set1, set2)` | `Intersect(set1, set2)` |
| Distinct | `mdx_distinct(set)` | `Distinct(set)` |
| NonEmpty | `mdx_non_empty(set)` | `NonEmpty(set)` |
| Hierarchize | `mdx_hierarchize(set)` | `Hierarchize(set)` |
| DrilldownLevel | `mdx_drilldown_level(set)` | `DrilldownLevel(set)` |

### Hierarchy members vs. level members

When querying a cube you need to decide **what** you want on an axis: the full hierarchy (with subtotals) or only the leaf-level data.

**Hierarchy members** -- `{[Hierarchy].Members}` returns **all** members of a hierarchy: the `All` root, every intermediate parent, and the leaf members. Each parent member carries the aggregated subtotal of its children. This is useful for hierarchical drill-down displays where you want to see totals, subtotals, and detail values together.

```r
# All members including All-member and subtotals:
mdx_hierarchy_members("[Product].[Category]")
# → {[Product].[Category].Members}
#   returns: All Products, Fruit, Apple, Banana, Dairy, Milk, Cheese, ...
```

**Level members** -- `{[Hierarchy].[Level].Members}` returns **only** the members at one specific level. No parent aggregates, no subtotals -- just the actual data points. This is the right choice for charts, flat tables, and statistical analysis where including subtotals would distort sums, averages, and visual proportions.

```r
# Only leaf-level members (no subtotals):
mdx_level_member_set("[Product].[Category].[Product]")
# → {[Product].[Category].[Product].Members}
#   returns: Apple, Banana, Milk, Cheese, ...
```

**Rule of thumb:** Use `mdx_hierarchy_members()` when you want a hierarchical, drill-down view with subtotals. Use `mdx_level_member_set()` when you want clean data for charts or calculations.

### Calculated member expressions

Expression helpers compose MDX arithmetic for `with_calculated_measure()`:

| Helper | Produces |
|--------|----------|
| `mdx_ratio(a, b)` | `a / b` |
| `mdx_diff(a, b)` | `a - b` |
| `mdx_sum_expr(a, b, c)` | `a + b + c` |
| `mdx_product(a, b)` | `a * b` |
| `mdx_pct_of_total(m, total)` | `m / (total, m) * 100` |
| `mdx_yoy_growth(m, time_h)` | Year-over-year growth formula |
| `mdx_running_sum(m, h)` | Cumulative sum along hierarchy |
| `mdx_coalesce_empty(a, b)` | `CoalesceEmpty(a, b)` |
| `mdx_iif(cond, t, f)` | `IIf(cond, t, f)` |

### Putting it all together

```r
library(eclipse.daanse.xmla)

conn <- Connection$new("http://server:8080/xmla",
                        auth = BasicAuth$new("user", "pass"))
catalog <- "Sales"

# Discover measures and hierarchies
measures    <- discover_measures(conn, catalog, "[Sales]")
hierarchies <- discover_hierarchies(conn, catalog, "[Sales]")

m1 <- measures[[1]]$measure_unique_name
h1 <- hierarchies[[1]]$hierarchy_unique_name

# Top 5 products by revenue, with a profit margin column
df <- MdxBuilder$new("[Sales]")$
  with_calculated_measure("Margin",
    mdx_ratio("[Measures].[Revenue]", "[Measures].[Cost]"),
    format_string = "0.00%")$
  with_set("[Top5]",
    mdx_topcount(mdx_hierarchy_members(h1), 5, m1))$
  on_columns(mdx_member_set(m1, "[Measures].[Margin]"))$
  on_rows("[Top5]")$
  non_empty()$
  execute(conn, catalog = catalog, format = "Tabular")

print(df)
```

## How XMLA and SOAP Work Under the Hood

XMLA (XML for Analysis) is a SOAP-based web service protocol for OLAP databases. This package handles all the XML plumbing so you don't have to, but here is what happens when you call `discover_*` or `xmla_execute`:

1. **SOAP envelope** -- Every request is wrapped in a SOAP XML envelope (`<SOAP-ENV:Envelope>`) containing a `<SOAP-ENV:Body>`.

2. **Two operations** -- XMLA defines exactly two operations:
   - **Discover** -- Retrieves metadata (catalogs, cubes, dimensions, measures, etc.). The request specifies a `RequestType` (e.g., `MDSCHEMA_CUBES`), optional `Restrictions`, and `Properties`.
   - **Execute** -- Runs an MDX (or DAX/SQL) statement. The request contains a `Command` with the statement and `Properties` controlling the response format.

3. **HTTP POST** -- The SOAP envelope is sent as an HTTP POST to the XMLA endpoint URL. Authentication (Basic, etc.) is handled at the HTTP level.

4. **Response parsing** -- The server returns a SOAP envelope containing either tabular row data (for Discover and Tabular Execute) or a multidimensional dataset with axes and cells (for Multidimensional Execute). This package parses the XML into R6 objects or data.frames.

The low-level building blocks are also exported if you need custom requests:

```r
# Build XML nodes
node <- xml_node("xmla", "Discover",
           xmla("RequestType", text = "DBSCHEMA_CATALOGS"))

# Create a SOAP envelope
envelope <- soap_envelope(body = list(node))

# Convert to XML string
xml_str <- to_xml_string(envelope)

# Send raw SOAP request
response <- send_soap_request("http://server:8080/xmla", xml_str, NoAuth$new())
```

The namespace constants are available via `XmlNamespaces$soap` and `XmlNamespaces$xmla`.

## Development

```r
# Load for development (no install needed)
pkgload::load_all()

# Regenerate NAMESPACE and docs
roxygen2::roxygenise()

# Run tests
testthat::test_dir("tests/testthat")

# Dependency management
renv::status()
renv::snapshot()
```

## Related Projects

This R client follows the same API patterns as the other Daanse XMLA clients:

- **Java** -- [org.eclipse.daanse.xmla.client](https://github.com/eclipse-daanse/org.eclipse.daanse.xmla) -- the reference implementation with the full XMLA API surface
- **TypeScript** -- [org.eclipse.daanse.board.app](https://github.com/eclipse-daanse/org.eclipse.daanse.board.app) -- browser and Node.js client with equivalent discover/execute functions

The `discover_*`, `xmla_execute`, and `mdx_*` functions in this package mirror their counterparts in the Java and TypeScript clients.

The `R/mdx-functions-*.R` files were programmatically generated from the MDX function metadata in [org.eclipse.daanse.olap](https://github.com/eclipse-daanse/org.eclipse.daanse.olap).

---

For this README -- in particular spelling, grammar, and formatting -- as well as the code documentation and the examples in the tests, LLMs were used as an aid during development.

---

```
library(pkgload)
pkgload::load_all()
setwd("~/git/daanse/org.eclipse.daanse.r.xmla")
roxygen2::roxygenise()
```