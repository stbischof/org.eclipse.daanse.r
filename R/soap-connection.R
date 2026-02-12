#' @title Connection
#' @description XMLA SOAP connection with optional session support.
#' @export
Connection <- R6::R6Class(
  "Connection",
  public = list(
    #' @field url The XMLA endpoint URL.
    url = NULL,
    
    #' @field auth The auth object.
    auth = NULL,
    
    #' @field session_id The current session ID, or NULL if stateless.
    session_id = NULL,
    
    #' @description Create a XMLA connection.
    #' @param url The endpoint URL.
    #' @param auth An auth object (NoAuth or BasicAuth). Defaults to NoAuth.
    initialize = function(url, auth = NoAuth$new()) {
      self$url <- url
      self$auth <- auth
    },
    
    #' @description Start a new XMLA session.
    #' @return Invisible self.
    start_session = function() {
      header_nodes <- private$combine_header_nodes(list(xml_node(
        "",
        "BeginSession",
        attrs = list(
          xmlns = XmlNamespaces$xmla,
          "xmlns:soap" = XmlNamespaces$soap,
          "soap:mustUnderstand" = "1"
        )
      )))
      env <- soap_envelope(header = header_nodes, body = list(execute_request("", properties = list())))
      soap_xml <- to_xml_string(env)
      doc <- send_soap_request(self$url, soap_xml, self$auth)
      
      ns <- xml_ns_detect(doc)
      session_node <- xml2::xml_find_first(doc, ".//soap:Header/xmla:Session", ns)
      if (is.na(session_node)) {
        header_node <- xml2::xml_find_first(doc, ".//soap:Header", ns)
        if (!is.na(header_node)) {
          for (ch in xml2::xml_children(header_node)) {
            if (xml2::xml_name(ch) == "Session") {
              session_node <- ch
              break
            }
          }
        }
      }
      if (!is.na(session_node)) {
        self$session_id <- xml2::xml_attr(session_node, "SessionId")
      }
      invisible(self)
    },
    
    #' @description End the current XMLA session.
    #' @return Invisible self.
    end_session = function() {
      if (is.null(self$session_id))
        return(invisible(self))
      header_nodes <- private$combine_header_nodes(list(xml_node(
        "",
        "EndSession",
        attrs = list(
          xmlns = XmlNamespaces$xmla,
          "xmlns:soap" = XmlNamespaces$soap,
          "soap:mustUnderstand" = "1",
          "SessionId" = self$session_id
        )
      )))
      env <- soap_envelope(header = header_nodes, body = list(execute_request("", properties = list())))
      soap_xml <- to_xml_string(env)
      tryCatch(
        send_soap_request(self$url, soap_xml, self$auth),
        error = function(e)
          warning("Failed to end session: ", e$message)
      )
      self$session_id <- NULL
      invisible(self)
    },
    
    #' @description Send a Discover request.
    #' @param request_type The discover request type.
    #' @param restrictions Named list of restrictions.
    #' @param properties Named list of properties.
    #' @return An xml2 document of the response.
    discover = function(request_type,
                        restrictions = list(),
                        properties = list()) {
      extra <- list()
      sh <- private$session_header()
      if (!is.null(sh))
        extra <- list(sh)
      
      env <- soap_envelope(
        header = private$combine_header_nodes(extra),
        body = list(discover_request(request_type, restrictions, properties))
      )
      soap_xml <- to_xml_string(env)
      send_soap_request(self$url, soap_xml, self$auth)
    },
    
    #' @description Send an Execute request.
    #' @param statement The MDX or DAX or SQL ... statement.
    #' @param properties Named list of properties.
    #' @return An xml2 document of the response.
    execute = function(statement, properties = list()) {
      extra <- list()
      sh <- private$session_header()
      if (!is.null(sh))
        extra <- list(sh)
      
      env <- soap_envelope(
        header = private$combine_header_nodes(extra),
        body = list(execute_request(statement, properties))
      )
      soap_xml <- to_xml_string(env)
      send_soap_request(self$url, soap_xml, self$auth)
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<Connection>\n")
      cat("  URL:", self$url, "\n")
      cat("  Auth:", class(self$auth)[1], "\n")
      cat("  Session:", if (is.null(self$session_id))
        "(none)"
        else
          self$session_id, "\n")
      invisible(self)
    }
  ),
  
  private = list(
    session_header = function() {
      if (is.null(self$session_id))
        return(NULL)
      xml_node(
        "",
        "Session",
        attrs = list(
          xmlns = XmlNamespaces$xmla,
          "xmlns:soap" = XmlNamespaces$soap,
          "soap:mustUnderstand" = "1",
          "SessionId" = self$session_id
        )
      )
    },
    
    combine_header_nodes = function(extra = list()) {
      nodes <- list()
      auth_nodes <- self$auth$soap_header_nodes()
      if (!is.null(auth_nodes)) {
        nodes <- c(nodes, auth_nodes)
      }
      if (length(extra) > 0) {
        nodes <- c(nodes, extra)
      }
      nodes
    }
  )
)
