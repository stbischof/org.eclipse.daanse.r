#' Discover data sources
#'
#' @param conn A \code{Connection} object.
#' @return A list of \code{DataSource} objects.
#' @export
discover_datasources <- function(conn) {
  rows <- xmla_discover_raw(conn, "DISCOVER_DATASOURCES")
  lapply(rows, DataSource_from_node)
}

#' Discover catalogs
#'
#' @param conn A \code{Connection} object.
#' @return A list of \code{Catalog} objects.
#' @export
discover_catalogs <- function(conn) {
  rows <- xmla_discover_raw(conn, "DBSCHEMA_CATALOGS")
  lapply(rows, Catalog_from_node)
}

#' Discover cubes
#'
#' @param conn A \code{Connection} object.
#' @param catalog Catalog name.
#' @return A list of \code{Cube} objects.
#' @export
discover_cubes <- function(conn, catalog) {
  restrictions <- list(CATALOG_NAME = catalog)
  properties <- list(Catalog = catalog)
  rows <- xmla_discover_raw(conn, "MDSCHEMA_CUBES", restrictions, properties)
  lapply(rows, Cube_from_node)
}

#' Discover dimensions
#'
#' @param conn A \code{Connection} object.
#' @param catalog Catalog name.
#' @param cube Cube name.
#' @return A list of \code{Dimension} objects.
#' @export
discover_dimensions <- function(conn, catalog, cube) {
  restrictions <- list(CATALOG_NAME = catalog, CUBE_NAME = cube)
  properties <- list(Catalog = catalog)
  rows <- xmla_discover_raw(conn, "MDSCHEMA_DIMENSIONS", restrictions, properties)
  lapply(rows, Dimension_from_node)
}

#' Discover hierarchies
#'
#' @param conn A \code{Connection} object.
#' @param catalog Catalog name.
#' @param cube Cube name.
#' @param dimension Optional dimension unique name filter.
#' @return A list of \code{Hierarchy} objects.
#' @export
discover_hierarchies <- function(conn, catalog, cube, dimension = NULL) {
  restrictions <- list(CATALOG_NAME = catalog, CUBE_NAME = cube)
  if (!is.null(dimension)) {
    restrictions[["DIMENSION_UNIQUE_NAME"]] <- dimension
  }
  properties <- list(Catalog = catalog)
  rows <- xmla_discover_raw(conn, "MDSCHEMA_HIERARCHIES", restrictions, properties)
  lapply(rows, Hierarchy_from_node)
}

#' Discover levels
#'
#' @param conn A \code{Connection} object.
#' @param catalog Catalog name.
#' @param cube Cube name.
#' @param hierarchy Optional hierarchy unique name filter.
#' @return A list of \code{Level} objects.
#' @export
discover_levels <- function(conn, catalog, cube, hierarchy = NULL) {
  restrictions <- list(CATALOG_NAME = catalog, CUBE_NAME = cube)
  if (!is.null(hierarchy)) {
    restrictions[["HIERARCHY_UNIQUE_NAME"]] <- hierarchy
  }
  properties <- list(Catalog = catalog)
  rows <- xmla_discover_raw(conn, "MDSCHEMA_LEVELS", restrictions, properties)
  lapply(rows, Level_from_node)
}

#' Discover measures
#'
#' @param conn A \code{Connection} object.
#' @param catalog Catalog name.
#' @param cube Cube name.
#' @return A list of \code{Measure} objects.
#' @export
discover_measures <- function(conn, catalog, cube) {
  restrictions <- list(CATALOG_NAME = catalog, CUBE_NAME = cube)
  properties <- list(Catalog = catalog)
  rows <- xmla_discover_raw(conn, "MDSCHEMA_MEASURES", restrictions, properties)
  lapply(rows, Measure_from_node)
}

#' Discover measure groups
#'
#' @param conn A \code{Connection} object.
#' @param catalog Catalog name.
#' @param cube Cube name.
#' @return A data.frame with measure group information.
#' @export
discover_measure_groups <- function(conn, catalog, cube) {
  restrictions <- list(CATALOG_NAME = catalog, CUBE_NAME = cube)
  properties <- list(Catalog = catalog)
  xmla_discover(conn, "MDSCHEMA_MEASUREGROUPS", restrictions, properties)
}

#' Discover members
#'
#' @param conn A \code{Connection} object.
#' @param catalog Catalog name.
#' @param cube Cube name.
#' @param dimension Optional dimension unique name filter.
#' @param hierarchy Optional hierarchy unique name filter.
#' @param level Optional level unique name filter.
#' @return A list of \code{Member} objects.
#' @export
discover_members <- function(conn,
                             catalog,
                             cube,
                             dimension = NULL,
                             hierarchy = NULL,
                             level = NULL) {
  restrictions <- list(CATALOG_NAME = catalog, CUBE_NAME = cube)
  if (!is.null(dimension)) {
    restrictions[["DIMENSION_UNIQUE_NAME"]] <- dimension
  }
  if (!is.null(hierarchy)) {
    restrictions[["HIERARCHY_UNIQUE_NAME"]] <- hierarchy
  }
  if (!is.null(level)) {
    restrictions[["LEVEL_UNIQUE_NAME"]] <- level
  }
  properties <- list(Catalog = catalog)
  rows <- xmla_discover_raw(conn, "MDSCHEMA_MEMBERS", restrictions, properties)
  lapply(rows, Member_from_node)
}

#' Discover named sets
#'
#' @param conn A \code{Connection} object.
#' @param catalog Catalog name.
#' @param cube Cube name.
#' @return A list of \code{MdSet} objects.
#' @export
discover_sets <- function(conn, catalog, cube) {
  restrictions <- list(CATALOG_NAME = catalog, CUBE_NAME = cube)
  properties <- list(Catalog = catalog)
  rows <- xmla_discover_raw(conn, "MDSCHEMA_SETS", restrictions, properties)
  lapply(rows, MdSet_from_node)
}

#' Discover properties
#'
#' @param conn A \code{Connection} object.
#' @param catalog Catalog name.
#' @param cube Cube name.
#' @return A list of \code{Property} objects.
#' @export
discover_properties <- function(conn, catalog, cube) {
  restrictions <- list(CATALOG_NAME = catalog, CUBE_NAME = cube)
  properties <- list(Catalog = catalog)
  rows <- xmla_discover_raw(conn, "MDSCHEMA_PROPERTIES", restrictions, properties)
  lapply(rows, Property_from_node)
}
