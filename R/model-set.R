#' @title MdSet
#' @description R6 class representing an XMLA named set.
#' @export
MdSet <- R6::R6Class(
  "MdSet",
  public = list(
    #' @field catalog_name Catalog name.
    catalog_name = NULL,

    #' @field cube_name Cube name.
    cube_name = NULL,

    #' @field set_name Set name.
    set_name = NULL,

    #' @field set_caption Set caption.
    set_caption = NULL,

    #' @field scope Scope.
    scope = NULL,

    #' @field description Description.
    description = NULL,

    #' @description Create an MdSet.
    #' @param catalog_name Catalog.
    #' @param cube_name Cube.
    #' @param set_name Name.
    #' @param set_caption Caption.
    #' @param scope Scope.
    #' @param description Description.
    initialize = function(catalog_name = NA_character_,
                          cube_name = NA_character_,
                          set_name = NA_character_,
                          set_caption = NA_character_,
                          scope = NA_character_,
                          description = NA_character_) {
      self$catalog_name <- catalog_name
      self$cube_name <- cube_name
      self$set_name <- set_name
      self$set_caption <- set_caption
      self$scope <- scope
      self$description <- description
    },

    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<MdSet>", self$set_name, "\n")
      invisible(self)
    }
  )
)

#' Create an MdSet from an XML row node
#' @param node An xml2 node.
#' @return An MdSet object.
#' @keywords internal
MdSet_from_node <- function(node) {
  MdSet$new(
    catalog_name = xml_child_text(node, "CATALOG_NAME"),
    cube_name    = xml_child_text(node, "CUBE_NAME"),
    set_name     = xml_child_text(node, "SET_NAME"),
    set_caption  = xml_child_text(node, "SET_CAPTION"),
    scope        = xml_child_text(node, "SCOPE"),
    description  = xml_child_text(node, "DESCRIPTION")
  )
}
