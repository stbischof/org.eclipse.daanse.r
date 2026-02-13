#' @title Measure
#' @description R6 class representing an XMLA measure.
#' @export
Measure <- R6::R6Class(
  "Measure",
  public = list(
    #' @field catalog_name Catalog name.
    catalog_name = NULL,
    
    #' @field cube_name Cube name.
    cube_name = NULL,
    
    #' @field measure_name Measure name.
    measure_name = NULL,
    
    #' @field measure_unique_name Measure unique name.
    measure_unique_name = NULL,
    
    #' @field measure_caption Measure caption.
    measure_caption = NULL,
    
    #' @field measure_aggregator Aggregator type.
    measure_aggregator = NULL,
    
    #' @field data_type Data type.
    data_type = NULL,
    
    #' @field description Description.
    description = NULL,
    
    #' @description Create a Measure.
    #' @param catalog_name Catalog.
    #' @param cube_name Cube.
    #' @param measure_name Name.
    #' @param measure_unique_name Unique name.
    #' @param measure_caption Caption.
    #' @param measure_aggregator Aggregator.
    #' @param data_type Data type.
    #' @param description Description.
    initialize = function(catalog_name = NA_character_,
                          cube_name = NA_character_,
                          measure_name = NA_character_,
                          measure_unique_name = NA_character_,
                          measure_caption = NA_character_,
                          measure_aggregator = NA_character_,
                          data_type = NA_character_,
                          description = NA_character_) {
      self$catalog_name <- catalog_name
      self$cube_name <- cube_name
      self$measure_name <- measure_name
      self$measure_unique_name <- measure_unique_name
      self$measure_caption <- measure_caption
      self$measure_aggregator <- measure_aggregator
      self$data_type <- data_type
      self$description <- description
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<Measure>", self$measure_name, "\n")
      invisible(self)
    }
  )
)

#' Create a Measure from an XML row node
#' @param node An xml2 node.
#' @return A Measure object.
#' @keywords internal
Measure_from_node <- function(node) {
  Measure$new(
    catalog_name        = xml_child_text(node, "CATALOG_NAME"),
    cube_name           = xml_child_text(node, "CUBE_NAME"),
    measure_name        = xml_child_text(node, "MEASURE_NAME"),
    measure_unique_name = xml_child_text(node, "MEASURE_UNIQUE_NAME"),
    measure_caption     = xml_child_text(node, "MEASURE_CAPTION"),
    measure_aggregator  = xml_child_text(node, "MEASURE_AGGREGATOR"),
    data_type           = xml_child_text(node, "DATA_TYPE"),
    description         = xml_child_text(node, "DESCRIPTION")
  )
}
