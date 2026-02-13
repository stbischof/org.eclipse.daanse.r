#' @title DataSource
#' @description R6 class representing an XMLA data source.
#' @export
DataSource <- R6::R6Class(
  "DataSource",
  public = list(
    #' @field data_source_name Name of the data source.
    data_source_name = NULL,
    
    #' @field data_source_description Description.
    data_source_description = NULL,
    
    #' @field url URL of the data source.
    url = NULL,
    
    #' @field provider_name Provider name.
    provider_name = NULL,
    
    #' @field provider_type Provider type.
    provider_type = NULL,
    
    #' @field authentication_mode Authentication mode.
    authentication_mode = NULL,
    
    #' @description Create a DataSource.
    #' @param data_source_name Name.
    #' @param data_source_description Description.
    #' @param url URL.
    #' @param provider_name Provider.
    #' @param provider_type Provider type.
    #' @param authentication_mode Auth mode.
    initialize = function(data_source_name = NA_character_,
                          data_source_description = NA_character_,
                          url = NA_character_,
                          provider_name = NA_character_,
                          provider_type = NA_character_,
                          authentication_mode = NA_character_) {
      self$data_source_name <- data_source_name
      self$data_source_description <- data_source_description
      self$url <- url
      self$provider_name <- provider_name
      self$provider_type <- provider_type
      self$authentication_mode <- authentication_mode
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<DataSource>", self$data_source_name, "\n")
      invisible(self)
    }
  )
)

#' Create a DataSource from an XML row node
#' @param node An xml2 node.
#' @return A DataSource object.
#' @keywords internal
DataSource_from_node <- function(node) {
  DataSource$new(
    data_source_name       = xml_child_text(node, "DataSourceName"),
    data_source_description = xml_child_text(node, "DataSourceDescription"),
    url                    = xml_child_text(node, "URL"),
    provider_name          = xml_child_text(node, "ProviderName"),
    provider_type          = xml_child_text(node, "ProviderType"),
    authentication_mode    = xml_child_text(node, "AuthenticationMode")
  )
}
