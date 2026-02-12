#' @title Authentication Classes
#' @description Classes for XMLA connection authentication.


#' No authentication (no-op)
#'
#' No-op authentication
#' @export
NoAuth <- R6::R6Class(
  "NoAuth",
  public = list(
    #' @description Apply authentication headers to request (no-op).
    #' @param req An httr2 request object.
    #' @return The unmodified request.
    apply_auth = function(req) {
      req
    },
    
    #' @description Return SOAP header nodes for authentication.
    #' @return NULL (no credentials needed).
    soap_header_nodes = function() {
      NULL
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<NoAuth>\n")
      invisible(self)
    }
  )
)


#' BasicAuth authentication
#'
#' HTTP Basic authentication.
#' @export
BasicAuth <- R6::R6Class(
  "BasicAuth",
  public = list(
    #' @description Create a new BasicAuth object.
    #' @param username The username.
    #' @param password The password.
    initialize = function(username, password) {
      private$username <- username
      private$password <- password
    },
    
    #' @description Apply Basic Auth header to request.
    #' @param req An httr2 request object.
    #' @return The request with Authorization header.
    apply_auth = function(req) {
      credentials <- paste0(private$username, ":", private$password)
      encoded <- base64enc::base64encode(charToRaw(credentials))
      httr2::req_headers(req, Authorization = paste("Basic", encoded))
    },
    
    #' @description Return SOAP header nodes for authentication.
    #' @return NULL (credentials sent via HTTP only).
    soap_header_nodes = function() {
      NULL
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<BasicAuth>\n")
      cat("  Username:", private$username, "\n")
      invisible(self)
    }
  ),
  private = list(username = NULL, password = NULL)
)
