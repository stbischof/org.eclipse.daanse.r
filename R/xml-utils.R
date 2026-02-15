# Internal XML parsing helpers
# @keywords internal

#' Extract text content of a child element by local name
#' @param node An xml2 node.
#' @param name Local element name to find.
#' @return Character scalar or NA_character_.
#' @keywords internal
xml_child_text <- function(node, name) {
  children <- xml2::xml_children(node)
  for (ch in children) {
    local <- xml2::xml_name(ch)
    if (local == name) {
      txt <- xml2::xml_text(ch)
      if (is.na(txt) || txt == "")
        return(NA_character_)
      return(txt)
    }
  }
  NA_character_
}

#' Extract integer value of a child element by local name
#' @param node An xml2 node.
#' @param name Local element name to find.
#' @return Integer scalar or NA_integer_.
#' @keywords internal
xml_child_integer <- function(node, name) {
  txt <- xml_child_text(node, name)
  if (is.na(txt))
    return(NA_integer_)
  suppressWarnings(as.integer(txt))
}

#' Extract logical value of a child element by local name
#' @param node An xml2 node.
#' @param name Local element name to find.
#' @return Logical scalar or NA.
#' @keywords internal
xml_child_logical <- function(node, name) {
  txt <- xml_child_text(node, name)
  if (is.na(txt))
    return(NA)
  tolower(txt) %in% c("true", "1", "yes")
}

#' Convert all children of a node to a named list
#' @param node An xml2 node.
#' @return A named list of character values.
#' @keywords internal
xml_node_to_list <- function(node) {
  children <- xml2::xml_children(node)
  if (length(children) == 0)
    return(list())
  nms <- xmla_decode_names(xml2::xml_name(children))
  vals <- xml2::xml_text(children)
  stats::setNames(as.list(vals), nms)
}

#' Convert a list of XML nodes to a data.frame
#' @param nodes A list of xml2 nodes (each representing a row).
#' @return A data.frame with one row per node.
#' @keywords internal
xml_nodes_to_df <- function(nodes) {
  if (length(nodes) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  rows <- lapply(nodes, xml_node_to_list)
  all_names <- unique(unlist(lapply(rows, names)))
  cols <- lapply(all_names, function(nm) {
    vapply(rows, function(r) {
      v <- r[[nm]]
      if (is.null(v))
        NA_character_
      else
        v
    }, character(1))
  })
  df <- stats::setNames(data.frame(cols, stringsAsFactors = FALSE), all_names)
  df
}

#' Extract row nodes from a DiscoverResponse
#' @param xml An xml2 document from a Discover response.
#' @return A nodeset of row elements.
#' @keywords internal
xml_get_discover_rows <- function(xml) {
  ns <- xml_ns_detect(xml)
  rows <- xml2::xml_find_all(xml, ".//rowset:row", ns)
  if (length(rows) == 0) {
    rows <- xml2::xml_find_all(xml, ".//*[local-name()='row']")
  }
  rows
}

#' Decode XMLA _xHHHH_ encoded names
#' @param x Character vector to decode.
#' @return Decoded character vector.
#' @keywords internal
xmla_decode_names <- function(x) {
  vapply(x, function(s) {
    m <- gregexpr("_x[0-9A-Fa-f]{4}_", s, perl = TRUE)
    if (m[[1]][1] == -1L)
      return(s)
    matches <- regmatches(s, m)[[1]]
    for (tok in matches) {
      hex <- substring(tok, 3, 6)
      s <- sub(tok, intToUtf8(strtoi(hex, 16L)), s, fixed = TRUE)
    }
    s
  }, character(1), USE.NAMES = FALSE)
}
