#' @title MDX Query Builder
#' @description Fluent API for building MDX queries.
#' @export
MdxBuilder <- R6::R6Class(
  "MdxBuilder",
  public = list(
    #' @description Create a new MdxBuilder for a cube.
    #' @param cube The cube name (e.g., "[Sales]").
    initialize = function(cube) {
      private$.cube <- cube
      private$.columns <- NULL
      private$.rows <- NULL
      private$.where <- NULL
      private$.non_empty <- FALSE
      private$.with_clauses <- list()
      private$.cell_props <- c("VALUE", "FORMAT_STRING", "FORMATTED_VALUE")
      private$.dim_props <- character(0)
    },
    
    #' @description Set the columns axis (axis 0).
    #' @param ... Set expressions for the columns axis.
    #' @return Self (for chaining).
    on_columns = function(...) {
      sets <- c(...)
      private$.columns <- paste(sets, collapse = ", ")
      invisible(self)
    },
    
    #' @description Set the rows axis (axis 1).
    #' @param ... Set expressions for the rows axis.
    #' @return Self (for chaining).
    on_rows = function(...) {
      sets <- c(...)
      private$.rows <- paste(sets, collapse = ", ")
      invisible(self)
    },
    
    #' @description Set the WHERE (slicer) clause.
    #' @param ... Member expressions for the slicer.
    #' @return Self (for chaining).
    where = function(...) {
      members <- c(...)
      if (length(members) == 1) {
        private$.where <- members
      } else {
        private$.where <- paste0("(", paste(members, collapse = ", "), ")")
      }
      invisible(self)
    },
    
    #' @description Enable or disable NON EMPTY on all axes.
    #' @param value Logical.
    #' @return Self (for chaining).
    non_empty = function(value = TRUE) {
      private$.non_empty <- value
      invisible(self)
    },
    
    #' @description Add a WITH MEMBER clause.
    #' @param name The calculated member name (e.g., "[Measures].[Calc]").
    #' @param expression The MDX expression.
    #' @param format_string Optional FORMAT_STRING for the calculated member.
    #' @return Self (for chaining).
    with_member = function(name, expression, format_string = NULL) {
      clause <- paste0("MEMBER ", name, " AS ", expression)
      if (!is.null(format_string)) {
        clause <- paste0(clause, ', FORMAT_STRING = "', format_string, '"')
      }
      private$.with_clauses <- c(private$.with_clauses, clause)
      invisible(self)
    },
    
    #' @description Convenience: add a calculated measure.
    #' @param measure_name The measure name (without dimension prefix).
    #' @param expression The MDX expression.
    #' @param format_string Optional FORMAT_STRING.
    #' @return Self (for chaining).
    with_calculated_measure = function(measure_name, expression, format_string = NULL) {
      full_name <- paste0("[Measures].[", measure_name, "]")
      self$with_member(full_name, expression, format_string)
    },
    
    #' @description Add a WITH SET clause.
    #' @param name The named set name.
    #' @param expression The MDX set expression.
    #' @return Self (for chaining).
    with_set = function(name, expression) {
      clause <- paste0("SET ", name, " AS ", expression)
      private$.with_clauses <- c(private$.with_clauses, clause)
      invisible(self)
    },
    
    #' @description Set cell properties to return.
    #' @param ... Property names (e.g., "VALUE", "FORMAT_STRING").
    #' @return Self (for chaining).
    cell_properties = function(...) {
      private$.cell_props <- c(...)
      invisible(self)
    },
    
    #' @description Set dimension properties to return.
    #' @param ... Property names (e.g., "MEMBER_CAPTION", "PARENT_UNIQUE_NAME").
    #' @return Self (for chaining).
    dimension_properties = function(...) {
      private$.dim_props <- c(...)
      invisible(self)
    },
    
    #' @description Generate the MDX query string.
    #' @return Character string with the MDX query.
    to_mdx = function() {
      parts <- character(0)
      
      if (length(private$.with_clauses) > 0) {
        with_str <- paste("WITH", paste(private$.with_clauses, collapse = "\n"))
        parts <- c(parts, with_str)
      }
      
      parts <- c(parts, "SELECT")
      
      ne <- if (private$.non_empty)
        "NON EMPTY "
      else
        ""
      
      axes <- character(0)
      if (!is.null(private$.columns)) {
        dim_prop_str <- ""
        if (length(private$.dim_props) > 0) {
          dim_prop_str <- paste0(" DIMENSION PROPERTIES ",
                                 paste(private$.dim_props, collapse = ", "))
        }
        axes <- c(axes,
                  paste0(ne, private$.columns, dim_prop_str, " ON COLUMNS"))
      }
      
      if (!is.null(private$.rows)) {
        dim_prop_str <- ""
        if (length(private$.dim_props) > 0) {
          dim_prop_str <- paste0(" DIMENSION PROPERTIES ",
                                 paste(private$.dim_props, collapse = ", "))
        }
        axes <- c(axes, paste0(ne, private$.rows, dim_prop_str, " ON ROWS"))
      }
      
      parts <- c(parts, paste(axes, collapse = ",\n"))
      
      parts <- c(parts, paste0("FROM ", private$.cube))
      
      if (!is.null(private$.where)) {
        parts <- c(parts, paste0("WHERE ", private$.where))
      }
      
      if (length(private$.cell_props) > 0) {
        parts <- c(parts, paste0(
          "CELL PROPERTIES ",
          paste(private$.cell_props, collapse = ", ")
        ))
      }
      
      paste(parts, collapse = "\n")
    },
    
    #' @description Execute the MDX query.
    #' @param conn A Connection object.
    #' @param catalog Optional catalog name.
    #' @param format Response format: "Multidimensional" or "Tabular".
    #' @return For "Tabular": a data.frame. For "Multidimensional": an ExecuteResult.
    execute = function(conn,
                       catalog = NULL,
                       format = "Multidimensional") {
      mdx <- self$to_mdx()
      xmla_execute(conn, mdx, catalog = catalog, format = format)
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<MdxBuilder> cube:", private$.cube, "\n")
      cat(self$to_mdx(), "\n")
      invisible(self)
    }
  ),
  
  private = list(
    .cube = NULL,
    .columns = NULL,
    .rows = NULL,
    .where = NULL,
    .non_empty = FALSE,
    .with_clauses = list(),
    .cell_props = character(0),
    .dim_props = character(0)
  )
)
