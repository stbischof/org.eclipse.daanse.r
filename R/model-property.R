#' @title Property
#' @description R6 class representing an XMLA property.
#' @export
Property <- R6::R6Class(
  "Property",
  public = list(
    #' @field catalog_name Catalog name.
    catalog_name = NULL,
    
    #' @field property_name Property name.
    property_name = NULL,
    
    #' @field property_description Property description.
    property_description = NULL,
    
    #' @field property_type Property type.
    property_type = NULL,
    
    #' @field property_access_type Access type.
    property_access_type = NULL,
    
    #' @field is_required Whether property is required.
    is_required = NULL,
    
    #' @field value Current value.
    value = NULL,
    
    #' @description Create a Property.
    #' @param catalog_name Catalog.
    #' @param property_name Name.
    #' @param property_description Description.
    #' @param property_type Type.
    #' @param property_access_type Access type.
    #' @param is_required Required flag.
    #' @param value Value.
    initialize = function(catalog_name = NA_character_,
                          property_name = NA_character_,
                          property_description = NA_character_,
                          property_type = NA_character_,
                          property_access_type = NA_character_,
                          is_required = NA,
                          value = NA_character_) {
      self$catalog_name <- catalog_name
      self$property_name <- property_name
      self$property_description <- property_description
      self$property_type <- property_type
      self$property_access_type <- property_access_type
      self$is_required <- is_required
      self$value <- value
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<Property>", self$property_name, "\n")
      invisible(self)
    }
  )
)

#' Create a Property from an XML row node
#' @param node An xml2 node.
#' @return A Property object.
#' @keywords internal
Property_from_node <- function(node) {
  Property$new(
    catalog_name         = xml_child_text(node, "CATALOG_NAME"),
    property_name        = xml_child_text(node, "PropertyName"),
    property_description = xml_child_text(node, "PropertyDescription"),
    property_type        = xml_child_text(node, "PropertyType"),
    property_access_type = xml_child_text(node, "PropertyAccessType"),
    is_required          = xml_child_logical(node, "IsRequired"),
    value                = xml_child_text(node, "Value")
  )
}
