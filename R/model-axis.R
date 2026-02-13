#' @title Axis
#' @description R6 class representing an axis in a multidimensional execute response.
#' @export
Axis <- R6::R6Class("Axis",
                    public = list(
                      #' @field name Axis name (e.g. "Axis0", "SlicerAxis").
                      name = NULL,
                      
                      #' @field tuples List of tuples. Each tuple is a list of named member info lists.
                      tuples = NULL,
                      
                      #' @description Create an Axis.
                      #' @param name Axis name.
                      #' @param tuples List of tuples.
                      initialize = function(name = NA_character_, tuples = list()) {
                        self$name <- name
                        self$tuples <- tuples
                      },
                      
                      #' @description Print method.
                      #' @param ... Ignored.
                      print = function(...) {
                        cat("<Axis>", self$name, "tuples:", length(self$tuples), "\n")
                        invisible(self)
                      }
                    ))

#' @title CellData
#' @description R6 class representing cell data in a multidimensional execute response.
#' @export
CellData <- R6::R6Class("CellData",
                        public = list(
                          #' @field cells List of cell lists, each with ordinal, value, formatted_value.
                          cells = NULL,
                          
                          #' @description Create CellData.
                          #' @param cells List of cell lists.
                          initialize = function(cells = list()) {
                            self$cells <- cells
                          },
                          
                          #' @description Print method.
                          #' @param ... Ignored.
                          print = function(...) {
                            cat("<CellData> cells:", length(self$cells), "\n")
                            invisible(self)
                          }
                        ))

#' @title ExecuteResult
#' @description R6 class holding the parsed result of a multidimensional Execute response.
#' @export
ExecuteResult <- R6::R6Class(
  "ExecuteResult",
  public = list(
    #' @field axes List of Axis objects.
    axes = NULL,
    
    #' @field cell_data A CellData object.
    cell_data = NULL,
    
    #' @description Create an ExecuteResult.
    #' @param axes List of Axis objects.
    #' @param cell_data CellData object.
    initialize = function(axes = list(), cell_data = NULL) {
      self$axes <- axes
      self$cell_data <- cell_data
    },
    
    #' @description Print method.
    #' @param ... Ignored.
    print = function(...) {
      cat("<ExecuteResult>\n")
      cat("  Axes:", length(self$axes), "\n")
      for (ax in self$axes) {
        cat("    -", ax$name, "(", length(ax$tuples), "tuples )\n")
      }
      n_cells <- if (!is.null(self$cell_data))
        length(self$cell_data$cells)
      else
        0L
      cat("  Cells:", n_cells, "\n")
      invisible(self)
    }
  )
)
