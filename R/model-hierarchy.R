#' @title Hierarchy
#' @description R6 class representing an XMLA hierarchy.
#' @export
Hierarchy <- R6::R6Class(
  "Hierarchy",
  public = list(
    #' @field catalog_name Catalog name.
    catalog_name = NULL,
    
    #' @field cube_name Cube name.
    cube_name = NULL,
    
    #' @field dimension_unique_name Dimension unique name.
    dimension_unique_name = NULL,
    
    #' @field hierarchy_name Hierarchy name.
    hierarchy_name = NULL,
    
    #' @field hierarchy_unique_name Hierarchy unique name.
    hierarchy_unique_name = NULL,
    
    #' @field hierarchy_caption Hierarchy caption.
    hierarchy_caption = NULL,
    
    #' @field description Description.
    description = NULL,
    
    #' @description Create a Hierarchy.
    #' @param catalog_name Catalog.
    #' @param cube_name Cube.
    #' @param dimension_unique_name Dimension unique name.
    #' @param hierarchy_name Name.
    #' @param hierarchy_unique_name Unique name.
    #' @param hierarchy_caption Caption.
    #' @param description Description.
    initialize = function(catalog_name = NA_character_,
                          cube_name = NA_character_,
                          dimension_unique_name = NA_character_,
                          hierarchy_name = NA_character_,
                          hierarchy_unique_name = NA_character_,
                          hierarchy_caption = NA_character_,
                          description = NA_character_) {
      self$catalog_name <- catalog_name
      self$cube_name <- cube_name
      self$dimension_unique_name <- dimension_unique_name
      self$hierarchy_name <- hierarchy_name
      self$hierarchy_unique_name <- hierarchy_unique_name
      self$hierarchy_caption <- hierarchy_caption
      self$description <- description
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<Hierarchy>", self$hierarchy_name, "\n")
      invisible(self)
    }
  )
)

#' Create a Hierarchy from an XML row node
#' @param node An xml2 node.
#' @return A Hierarchy object.
#' @keywords internal
Hierarchy_from_node <- function(node) {
  Hierarchy$new(
    catalog_name          = xml_child_text(node, "CATALOG_NAME"),
    cube_name             = xml_child_text(node, "CUBE_NAME"),
    dimension_unique_name = xml_child_text(node, "DIMENSION_UNIQUE_NAME"),
    hierarchy_name        = xml_child_text(node, "HIERARCHY_NAME"),
    hierarchy_unique_name = xml_child_text(node, "HIERARCHY_UNIQUE_NAME"),
    hierarchy_caption     = xml_child_text(node, "HIERARCHY_CAPTION"),
    description           = xml_child_text(node, "DESCRIPTION")
  )
}
