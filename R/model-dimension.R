#' @title Dimension
#' @description R6 class representing an XMLA dimension.
#' @export
Dimension <- R6::R6Class(
  "Dimension",
  public = list(
    #' @field catalog_name Catalog name.
    catalog_name = NULL,
    
    #' @field cube_name Cube name.
    cube_name = NULL,
    
    #' @field dimension_name Dimension name.
    dimension_name = NULL,
    
    #' @field dimension_unique_name Dimension unique name.
    dimension_unique_name = NULL,
    
    #' @field dimension_caption Dimension caption.
    dimension_caption = NULL,
    
    #' @field dimension_type Dimension type.
    dimension_type = NULL,
    
    #' @field description Description.
    description = NULL,
    
    #' @description Create a Dimension.
    #' @param catalog_name Catalog.
    #' @param cube_name Cube.
    #' @param dimension_name Name.
    #' @param dimension_unique_name Unique name.
    #' @param dimension_caption Caption.
    #' @param dimension_type Type.
    #' @param description Description.
    initialize = function(catalog_name = NA_character_,
                          cube_name = NA_character_,
                          dimension_name = NA_character_,
                          dimension_unique_name = NA_character_,
                          dimension_caption = NA_character_,
                          dimension_type = NA_character_,
                          description = NA_character_) {
      self$catalog_name <- catalog_name
      self$cube_name <- cube_name
      self$dimension_name <- dimension_name
      self$dimension_unique_name <- dimension_unique_name
      self$dimension_caption <- dimension_caption
      self$dimension_type <- dimension_type
      self$description <- description
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<Dimension>", self$dimension_name, "\n")
      invisible(self)
    }
  )
)

#' Create a Dimension from an XML row node
#' @param node An xml2 node.
#' @return A Dimension object.
#' @keywords internal
Dimension_from_node <- function(node) {
  Dimension$new(
    catalog_name          = xml_child_text(node, "CATALOG_NAME"),
    cube_name             = xml_child_text(node, "CUBE_NAME"),
    dimension_name        = xml_child_text(node, "DIMENSION_NAME"),
    dimension_unique_name = xml_child_text(node, "DIMENSION_UNIQUE_NAME"),
    dimension_caption     = xml_child_text(node, "DIMENSION_CAPTION"),
    dimension_type        = xml_child_text(node, "DIMENSION_TYPE"),
    description           = xml_child_text(node, "DESCRIPTION")
  )
}
