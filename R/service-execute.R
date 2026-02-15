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
  if (format == "Multidimensional") {
    props[["AxisFormat"]] <- "TupleFormat"
  }
  xml <- conn$execute(statement, properties = props)
  
  if (is.null(xml)) {
    if (format == "Tabular")
      return(data.frame())
    return(ExecuteResult$new())
  }
  
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
  xml_nodes_to_df(rows)
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
  
  tuples_node <- NULL
  for (ch in xml2::xml_children(axis_node)) {
    if (xml2::xml_name(ch) == "Tuples") {
      tuples_node <- ch
      break
    }
  }
  
  tuples <- list()
  if (!is.null(tuples_node)) {
    tuple_nodes <- xml2::xml_children(tuples_node)
    tuple_nodes <- tuple_nodes[xml2::xml_name(tuple_nodes) == "Tuple"]
    tuples <- lapply(tuple_nodes, function(tuple_node) {
      member_nodes <- xml2::xml_children(tuple_node)
      member_nodes <- member_nodes[xml2::xml_name(member_nodes) == "Member"]
      lapply(member_nodes, function(member_node) {
        hierarchy <- xml2::xml_attr(member_node, "Hierarchy")
        children <- xml2::xml_children(member_node)
        member_info <- list()
        for (child in children) {
          nm <- xml2::xml_name(child)
          member_info[[nm]] <- xml2::xml_text(child)
        }
        member_info[["Hierarchy"]] <- hierarchy
        member_info
      })
    })
  }
  
  Axis$new(name = axis_name, tuples = tuples)
}

#' Parse CellData node
#' @param celldata_node An xml2 node for the CellData element.
#' @return A CellData object.
#' @keywords internal
parse_cell_data <- function(celldata_node) {
  cell_nodes <- xml2::xml_children(celldata_node)
  cell_nodes <- cell_nodes[xml2::xml_name(cell_nodes) == "Cell"]
  
  cells <- lapply(cell_nodes, function(cell_node) {
    ordinal <- as.integer(xml2::xml_attr(cell_node, "CellOrdinal"))
    children <- xml2::xml_children(cell_node)
    cell_info <- list(ordinal = ordinal)
    for (child in children) {
      nm <- xml2::xml_name(child)
      val <- xml2::xml_text(child)
      if (nm == "Value") {
        xsi_type <- xml2::xml_attr(child, "type")
        if (!is.na(xsi_type) &&
            grepl("int|long|double|float|decimal",
                  xsi_type,
                  ignore.case = TRUE)) {
          cell_info[["value"]] <- as.numeric(val)
        } else {
          cell_info[["value"]] <- val
        }
      } else if (nm == "FmtValue") {
        cell_info[["formattedValue"]] <- val
      } else if (nm == "FormatString") {
        cell_info[["formatString"]] <- val
      } else {
        cell_info[[nm]] <- val
      }
    }
    cell_info
  })
  
  CellData$new(cells = cells)
}
