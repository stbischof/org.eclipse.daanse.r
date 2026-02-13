#' @title Cube
#' @description R6 class representing an XMLA cube.
#' @export
Cube <- R6::R6Class(
  "Cube",
  public = list(
    #' @field catalog_name Catalog name.
    catalog_name = NULL,
    
    #' @field cube_name Cube name.
    cube_name = NULL,
    
    #' @field cube_type Cube type.
    cube_type = NULL,
    
    #' @field description Description.
    description = NULL,
    
    #' @field last_schema_update Last schema update timestamp.
    last_schema_update = NULL,
    
    #' @description Create a Cube.
    #' @param catalog_name Catalog.
    #' @param cube_name Name.
    #' @param cube_type Type.
    #' @param description Description.
    #' @param last_schema_update Last update.
    initialize = function(catalog_name = NA_character_,
                          cube_name = NA_character_,
                          cube_type = NA_character_,
                          description = NA_character_,
                          last_schema_update = NA_character_) {
      self$catalog_name <- catalog_name
      self$cube_name <- cube_name
      self$cube_type <- cube_type
      self$description <- description
      self$last_schema_update <- last_schema_update
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<Cube>", self$cube_name, "\n")
      invisible(self)
    }
  )
)

#' Create a Cube from an XML row node
#' @param node An xml2 node.
#' @return A Cube object.
#' @keywords internal
Cube_from_node <- function(node) {
  Cube$new(
    catalog_name       = xml_child_text(node, "CATALOG_NAME"),
    cube_name          = xml_child_text(node, "CUBE_NAME"),
    cube_type          = xml_child_text(node, "CUBE_TYPE"),
    description        = xml_child_text(node, "DESCRIPTION"),
    last_schema_update = xml_child_text(node, "LAST_SCHEMA_UPDATE")
  )
}
