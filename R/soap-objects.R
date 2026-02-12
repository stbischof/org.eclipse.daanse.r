#' @title SOAP/XMLA Namespaces
#' @description Namespace registry used by all SOAP/XMLA objects.
#' @export
XmlNamespaces <- list(soap = "http://schemas.xmlsoap.org/soap/envelope/", xmla = "urn:schemas-microsoft-com:xml-analysis")



.xml_escape <- function(s) {
  s <- as.character(s)
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s <- gsub('"', "&quot;", s, fixed = TRUE)
  s <- gsub("'", "&apos;", s, fixed = TRUE)
  s
}



#' Create an XML node descriptor
#'
#' Returns a plain list representing an XML element with prefix, tag,
#' attributes, text content, and children.
#'
#' @param prefix XML namespace prefix (e.g. "soap", "xmla").
#' @param tag Local tag name (without prefix).
#' @param ... Child nodes (further xml_node lists).
#' @param attrs Named list of XML attributes.
#' @param text Optional text content.
#' @return A list with components prefix, tag, attrs, text, children.
#' @export
xml_node <- function(prefix,
                     tag,
                     ...,
                     attrs = list(),
                     text = NULL) {
  list(
    prefix = prefix,
    tag = tag,
    attrs = attrs,
    text = text,
    children = list(...)
  )
}

#' Create a SOAP-namespaced XML node
#'
#' Shorthand for \code{xml_node("soap", tag, ...)}.
#'
#' @inheritParams xml_node
#' @export
soap <- function(tag,
                 ...,
                 attrs = list(),
                 text = NULL) {
  xml_node("SOAP-ENV", tag, ..., attrs = attrs, text = text)
}

#' Create an XMLA-namespaced XML node
#'
#' Shorthand for \code{xml_node("xmla", tag, ...)}.
#'
#' @inheritParams xml_node
#' @export
xmla <- function(tag,
                 ...,
                 attrs = list(),
                 text = NULL) {
  xml_node("xmla", tag, ..., attrs = attrs, text = text)
}

#' Recursively build xml2 nodes from a node descriptor
#'
#' Adds an xml2 child element to \code{parent} based on \code{node},
#' then recurses into children.
#'
#' @param node A list as returned by \code{xml_node}.
#' @param parent An xml2 node to add children to.
#' @return The newly created xml2 node (invisibly).
#' @export
build_xml <- function(node, parent) {
  stopifnot(inherits(parent, "xml_node"))
  
  name <- if (nzchar(node$prefix))
    paste0(node$prefix, ":", node$tag)
  else
    node$tag
  el <- xml2::xml_add_child(parent, name)
  
  if (length(node$attrs)) {
    for (nm in names(node$attrs)) {
      xml2::xml_set_attr(el, nm, as.character(node$attrs[[nm]]))
    }
  }
  
  if (!is.null(node$text)) {
    xml2::xml_set_text(el, .xml_escape(node$text))
  }
  
  for (ch in node$children) {
    build_xml(ch, el)
  }
  
  invisible(el)
}

#' Build a SOAP Envelope node descriptor
#'
#' Creates the top-level SOAP Envelope structure with Header and Body.
#'
#' @param header List of child node descriptors for the SOAP Header.
#' @param body List of child node descriptors for the SOAP Body.
#' @return A node descriptor list (the Envelope).
#' @export
soap_envelope <- function(header = list(), body = list()) {
  xml_node(
    "SOAP-ENV",
    "Envelope",
    do.call(xml_node, c(
      list(prefix = "SOAP-ENV", tag = "Header"), header
    )),
    do.call(xml_node, c(
      list(prefix = "SOAP-ENV", tag = "Body"), body
    )),
    attrs = list(
      "xmlns:SOAP-ENV" = XmlNamespaces$soap,
      "xmlns:xmla" = XmlNamespaces$xmla
    )
  )
}

#' Serialize a SOAP Envelope to an XML string
#'
#' @param envelope A node descriptor as returned by \code{soap_envelope}.
#' @return A single XML string.
#' @export
to_xml_string <- function(envelope) {
  paste0('<?xml version="1.0" encoding="UTF-8"?>',
         .node_to_string(envelope))
}

.node_to_string <- function(node) {
  name <- if (nzchar(node$prefix))
    paste0(node$prefix, ":", node$tag)
  else
    node$tag
  
  attr_str <- ""
  if (length(node$attrs) > 0) {
    parts <- vapply(names(node$attrs), function(n) {
      paste0(" ", n, '="', node$attrs[[n]], '"')
    }, character(1))
    attr_str <- paste(parts, collapse = "")
  }
  
  inner <- ""
  if (!is.null(node$text)) {
    inner <- .xml_escape(node$text)
  }
  for (ch in node$children) {
    inner <- paste0(inner, .node_to_string(ch))
  }
  
  paste0("<", name, attr_str, ">", inner, "</", name, ">")
}



#' Build an XMLA Discover request body node
#'
#' @param request_type The XMLA request type string (e.g. "DBSCHEMA_CATALOGS").
#' @param restrictions Named list of restriction key/value pairs.
#' @param properties Named list of property key/value pairs.
#' @return A node descriptor for the Discover element.
#' @export
discover_request <- function(request_type,
                             restrictions = list(),
                             properties = list()) {
  restr_children <- lapply(names(restrictions), function(nm) {
    xml_node("", nm, text = restrictions[[nm]])
  })
  prop_children <- lapply(names(properties), function(nm) {
    xml_node("", nm, text = properties[[nm]])
  })
  
  restriction_node <- if (length(restr_children) > 0) {
    do.call(xml_node, c(list(prefix = "", tag = "Restrictions"), list(do.call(
      xml_node, c(list(prefix = "", tag = "RestrictionList"), restr_children)
    ))))
  } else {
    xml_node("", "Restrictions")
  }
  
  property_node <- if (length(prop_children) > 0) {
    do.call(xml_node, c(list(prefix = "", tag = "Properties"), list(do.call(
      xml_node, c(list(prefix = "", tag = "PropertyList"), prop_children)
    ))))
  } else {
    xml_node("", "Properties")
  }
  
  xml_node(
    "",
    "Discover",
    xml_node("", "RequestType", text = request_type),
    restriction_node,
    property_node,
    attrs = list(xmlns = XmlNamespaces$xmla)
  )
}

#' Build an XMLA Execute request body node
#'
#' @param statement The MDX, DAX, or SQL statement to execute.
#' @param properties Named list of property key/value pairs.
#' @return A node descriptor for the Execute element.
#' @export
execute_request <- function(statement, properties = list()) {
  prop_children <- lapply(names(properties), function(nm) {
    xml_node("", nm, text = properties[[nm]])
  })
  
  property_node <- if (length(prop_children) > 0) {
    do.call(xml_node, c(list(prefix = "", tag = "Properties"), list(do.call(
      xml_node, c(list(prefix = "", tag = "PropertyList"), prop_children)
    ))))
  } else {
    xml_node("", "Properties")
  }
  
  xml_node(
    "",
    "Execute",
    xml_node("", "Command", xml_node("", "Statement", text = statement)),
    property_node,
    attrs = list(xmlns = XmlNamespaces$xmla)
  )
}

#' Send a SOAP request to an XMLA server
#'
#' Sends an XML string as a SOAP request, checks for HTTP errors and
#' SOAP Faults, and returns the parsed xml2 document.
#'
#' @param url The XMLA endpoint URL.
#' @param soap_xml A SOAP envelope XML string.
#' @param auth An auth object with an \code{apply_auth(req)} method.
#' @return An xml2 document of the response.
#' @export
send_soap_request <- function(url, soap_xml, auth) {
  req <- httr2::request(url)
  req <- httr2::req_headers(req, "Content-Type" = "text/xml; charset=UTF-8")
  req <- auth$apply_auth(req)
  req <- httr2::req_body_raw(req, soap_xml, type = "text/xml; charset=UTF-8")
  req <- httr2::req_error(
    req,
    is_error = function(resp)
      FALSE
  )
  
  resp <- httr2::req_perform(req)
  
  status <- httr2::resp_status(resp)
  
  if (status >= 400) {
    body <- tryCatch(
      httr2::resp_body_string(resp),
      error = function(e)
        ""
    )
    stop("XMLA SOAP request failed with HTTP status ",
         status,
         ": ",
         body,
         call. = FALSE)
  }
  
  resp_raw <- tryCatch(
    httr2::resp_body_raw(resp),
    error = function(e)
      raw(0)
  )
  if (length(resp_raw) == 0) {
    stop("XMLA server returned empty response (HTTP ",
         status,
         ")",
         call. = FALSE)
  }
  
  body <- rawToChar(resp_raw)
  doc <- xml2::read_xml(body)
  
  # Check for SOAP Fault - detect namespace prefix dynamically
  doc_ns <- xml2::xml_ns(doc)
  soap_prefix <- names(doc_ns)[doc_ns == "http://schemas.xmlsoap.org/soap/envelope/"]
  if (length(soap_prefix) > 0) {
    soap_prefix <- soap_prefix[1]
    fault_xpath <- paste0(".//", soap_prefix, ":Body/", soap_prefix, ":Fault")
    fault <- xml2::xml_find_first(doc, fault_xpath, doc_ns)
    if (!is.na(fault)) {
      faultstring <- xml2::xml_text(xml2::xml_find_first(fault, ".//faultstring"))
      if (is.na(faultstring))
        faultstring <- "Unknown SOAP Fault"
      stop("XMLA SOAP Fault: ", faultstring, call. = FALSE)
    }
  }
  
  doc
}

#' Detect XML namespace prefixes in an XMLA response
#'
#' Maps document namespace URIs to canonical names used for XPath queries.
#'
#' @param doc An xml2 document.
#' @return A named character vector mapping canonical names to namespace URIs.
#' @export
xml_ns_detect <- function(doc) {
  doc_ns <- xml2::xml_ns(doc)
  ns_vec <- as.character(doc_ns)
  names(ns_vec) <- names(doc_ns)
  
  result <- c()
  uris <- list(
    soap      = "http://schemas.xmlsoap.org/soap/envelope/",
    xmla      = "urn:schemas-microsoft-com:xml-analysis",
    rowset    = "urn:schemas-microsoft-com:xml-analysis:rowset",
    mddataset = "urn:schemas-microsoft-com:xml-analysis:mddataset"
  )
  
  for (canonical in names(uris)) {
    uri <- uris[[canonical]]
    match <- names(ns_vec)[ns_vec == uri]
    if (length(match) > 0) {
      result[canonical] <- uri
    }
  }
  
  for (canonical in names(uris)) {
    if (!canonical %in% names(result)) {
      result[canonical] <- uris[[canonical]]
    }
  }
  
  result
}
