#' @title Level
#' @description R6 class representing an XMLA level.
#' @export
Level <- R6::R6Class(
  "Level",
  public = list(
    #' @field catalog_name Catalog name.
    catalog_name = NULL,
    
    #' @field cube_name Cube name.
    cube_name = NULL,
    
    #' @field dimension_unique_name Dimension unique name.
    dimension_unique_name = NULL,
    
    #' @field hierarchy_unique_name Hierarchy unique name.
    hierarchy_unique_name = NULL,
    
    #' @field level_name Level name.
    level_name = NULL,
    
    #' @field level_unique_name Level unique name.
    level_unique_name = NULL,
    
    #' @field level_caption Level caption.
    level_caption = NULL,
    
    #' @field level_number Level number.
    level_number = NULL,
    
    #' @field description Description.
    description = NULL,
    
    #' @description Create a Level.
    #' @param catalog_name Catalog.
    #' @param cube_name Cube.
    #' @param dimension_unique_name Dimension unique name.
    #' @param hierarchy_unique_name Hierarchy unique name.
    #' @param level_name Name.
    #' @param level_unique_name Unique name.
    #' @param level_caption Caption.
    #' @param level_number Number.
    #' @param description Description.
    initialize = function(catalog_name = NA_character_,
                          cube_name = NA_character_,
                          dimension_unique_name = NA_character_,
                          hierarchy_unique_name = NA_character_,
                          level_name = NA_character_,
                          level_unique_name = NA_character_,
                          level_caption = NA_character_,
                          level_number = NA_integer_,
                          description = NA_character_) {
      self$catalog_name <- catalog_name
      self$cube_name <- cube_name
      self$dimension_unique_name <- dimension_unique_name
      self$hierarchy_unique_name <- hierarchy_unique_name
      self$level_name <- level_name
      self$level_unique_name <- level_unique_name
      self$level_caption <- level_caption
      self$level_number <- level_number
      self$description <- description
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<Level>", self$level_name, "\n")
      invisible(self)
    }
  )
)

#' Create a Level from an XML row node
#' @param node An xml2 node.
#' @return A Level object.
#' @keywords internal
Level_from_node <- function(node) {
  Level$new(
    catalog_name          = xml_child_text(node, "CATALOG_NAME"),
    cube_name             = xml_child_text(node, "CUBE_NAME"),
    dimension_unique_name = xml_child_text(node, "DIMENSION_UNIQUE_NAME"),
    hierarchy_unique_name = xml_child_text(node, "HIERARCHY_UNIQUE_NAME"),
    level_name            = xml_child_text(node, "LEVEL_NAME"),
    level_unique_name     = xml_child_text(node, "LEVEL_UNIQUE_NAME"),
    level_caption         = xml_child_text(node, "LEVEL_CAPTION"),
    level_number          = xml_child_integer(node, "LEVEL_NUMBER"),
    description           = xml_child_text(node, "DESCRIPTION")
  )
}
