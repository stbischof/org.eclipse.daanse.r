#' @title Catalog
#' @description R6 class representing an XMLA catalog.
#' @export
Catalog <- R6::R6Class(
  "Catalog",
  public = list(
    #' @field catalog_name Name of the catalog.
    catalog_name = NULL,
    
    #' @field description Description.
    description = NULL,
    
    #' @field roles Roles.
    roles = NULL,
    
    #' @description Create a Catalog.
    #' @param catalog_name Name.
    #' @param description Description.
    #' @param roles Roles.
    initialize = function(catalog_name = NA_character_,
                          description = NA_character_,
                          roles = NA_character_) {
      self$catalog_name <- catalog_name
      self$description <- description
      self$roles <- roles
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<Catalog>", self$catalog_name, "\n")
      invisible(self)
    }
  )
)

#' Create a Catalog from an XML row node
#' @param node An xml2 node.
#' @return A Catalog object.
#' @keywords internal
Catalog_from_node <- function(node) {
  Catalog$new(
    catalog_name = xml_child_text(node, "CATALOG_NAME"),
    description  = xml_child_text(node, "DESCRIPTION"),
    roles        = xml_child_text(node, "ROLES")
  )
}
