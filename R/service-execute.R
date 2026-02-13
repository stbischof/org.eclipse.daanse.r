#' Execute an MDX/DAX/SQL statement
#'
#' Sends an Execute SOAP request and parses the response.
#'
#' @param conn A \code{Connection} object.
#' @param statement The MDX, DAX, or SQL statement.
#' @param catalog Optional catalog name (added to properties).
#' @param format Response format: \code{"Multidimensional"} (default) or
#'   \code{"Tabular"}.
#' @return For \code{"Multidimensional"}: an \code{ExecuteResult} object.
#'   For \code{"Tabular"}: a \code{data.frame}.
#' @export
xmla_execute <- function(conn,
                         statement,
                         catalog = NULL,
                         format = "Multidimensional") {
  props <- list(Format = format)
  if (!is.null(catalog)) {
    props[["Catalog"]] <- catalog
  }
  xml <- conn$execute(statement, properties = props)
  
  if (format == "Tabular") {
    parse_tabular_response(xml)
  } else {
    parse_multidimensional_response(xml)
  }
}

#' Parse a tabular Execute response into a data.frame
#' @param xml An xml2 document.
#' @return A data.frame.
#' @keywords internal
parse_tabular_response <- function(xml) {
  rows <- xml_get_discover_rows(xml)
  df <- xml_nodes_to_df(rows)
  names(df) <- xmla_decode_names(names(df))
  df
}

#' Parse a multidimensional Execute response
#' @param xml An xml2 document.
#' @return An ExecuteResult object.
#' @keywords internal
parse_multidimensional_response <- function(xml) {
  ns <- xml_ns_detect(xml)
  
  axes_nodes <- xml2::xml_find_all(xml, ".//mddataset:Axes/mddataset:Axis", ns)
  if (length(axes_nodes) == 0) {
    axes_nodes <- xml2::xml_find_all(xml, ".//*[local-name()='Axes']/*[local-name()='Axis']")
  }
  
  axes <- lapply(axes_nodes, parse_axis)
  
  celldata_node <- xml2::xml_find_first(xml, ".//mddataset:CellData", ns)
  if (is.na(celldata_node)) {
    celldata_node <- xml2::xml_find_first(xml, ".//*[local-name()='CellData']")
  }
  
  cell_data <- if (!is.na(celldata_node)) {
    parse_cell_data(celldata_node)
  } else {
    CellData$new()
  }
  
  ExecuteResult$new(axes = axes, cell_data = cell_data)
}

#' Parse a single Axis node
#' @param axis_node An xml2 node for an Axis element.
#' @return An Axis object.
#' @keywords internal
parse_axis <- function(axis_node) {
  axis_name <- xml2::xml_attr(axis_node, "name")
  
  tuple_nodes <- xml2::xml_find_all(axis_node, ".//*[local-name()='Tuple']")
  tuples <- lapply(tuple_nodes, function(tnode) {
    member_nodes <- xml2::xml_find_all(tnode, ".//*[local-name()='Member']")
    lapply(member_nodes, function(mnode) {
      xml_node_to_list(mnode)
    })
  })
  
  Axis$new(name = axis_name, tuples = tuples)
}

#' Parse CellData node
#' @param celldata_node An xml2 node for the CellData element.
#' @return A CellData object.
#' @keywords internal
parse_cell_data <- function(celldata_node) {
  cell_nodes <- xml2::xml_find_all(celldata_node, ".//*[local-name()='Cell']")
  cells <- lapply(cell_nodes, function(cnode) {
    ordinal <- xml2::xml_attr(cnode, "CellOrdinal")
    info <- xml_node_to_list(cnode)
    info[["CellOrdinal"]] <- ordinal
    info
  })
  CellData$new(cells = cells)
}
