#' @title Member
#' @description R6 class representing an XMLA member.
#' @export
Member <- R6::R6Class(
  "Member",
  public = list(
    #' @field catalog_name Catalog name.
    catalog_name = NULL,
    
    #' @field cube_name Cube name.
    cube_name = NULL,
    
    #' @field dimension_unique_name Dimension unique name.
    dimension_unique_name = NULL,
    
    #' @field hierarchy_unique_name Hierarchy unique name.
    hierarchy_unique_name = NULL,
    
    #' @field level_unique_name Level unique name.
    level_unique_name = NULL,
    
    #' @field member_name Member name.
    member_name = NULL,
    
    #' @field member_unique_name Member unique name.
    member_unique_name = NULL,
    
    #' @field member_caption Member caption.
    member_caption = NULL,
    
    #' @field member_type Member type.
    member_type = NULL,
    
    #' @field description Description.
    description = NULL,
    
    #' @description Create a Member.
    #' @param catalog_name Catalog.
    #' @param cube_name Cube.
    #' @param dimension_unique_name Dimension unique name.
    #' @param hierarchy_unique_name Hierarchy unique name.
    #' @param level_unique_name Level unique name.
    #' @param member_name Name.
    #' @param member_unique_name Unique name.
    #' @param member_caption Caption.
    #' @param member_type Type.
    #' @param description Description.
    initialize = function(catalog_name = NA_character_,
                          cube_name = NA_character_,
                          dimension_unique_name = NA_character_,
                          hierarchy_unique_name = NA_character_,
                          level_unique_name = NA_character_,
                          member_name = NA_character_,
                          member_unique_name = NA_character_,
                          member_caption = NA_character_,
                          member_type = NA_character_,
                          description = NA_character_) {
      self$catalog_name <- catalog_name
      self$cube_name <- cube_name
      self$dimension_unique_name <- dimension_unique_name
      self$hierarchy_unique_name <- hierarchy_unique_name
      self$level_unique_name <- level_unique_name
      self$member_name <- member_name
      self$member_unique_name <- member_unique_name
      self$member_caption <- member_caption
      self$member_type <- member_type
      self$description <- description
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<Member>", self$member_name, "\n")
      invisible(self)
    }
  )
)

#' Create a Member from an XML row node
#' @param node An xml2 node.
#' @return A Member object.
#' @keywords internal
Member_from_node <- function(node) {
  Member$new(
    catalog_name          = xml_child_text(node, "CATALOG_NAME"),
    cube_name             = xml_child_text(node, "CUBE_NAME"),
    dimension_unique_name = xml_child_text(node, "DIMENSION_UNIQUE_NAME"),
    hierarchy_unique_name = xml_child_text(node, "HIERARCHY_UNIQUE_NAME"),
    level_unique_name     = xml_child_text(node, "LEVEL_UNIQUE_NAME"),
    member_name           = xml_child_text(node, "MEMBER_NAME"),
    member_unique_name    = xml_child_text(node, "MEMBER_UNIQUE_NAME"),
    member_caption        = xml_child_text(node, "MEMBER_CAPTION"),
    member_type           = xml_child_text(node, "MEMBER_TYPE"),
    description           = xml_child_text(node, "DESCRIPTION")
  )
}
