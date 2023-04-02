#' Component Data
#'
#' @description
#'
#' Data is used for a component's internal state and
#' can also be used to pass information from a parent component to its children.
#' Both component templates and methods have access to component data.
#'
#' @details
#'
#' ## Creating data
#'
#' For instance, the following is a valid data function:
#'
#' ```r
#' function() {
#'  list(
#'    rctv = shiny::reactiveVal(),
#'    df   = mtcars
#'  )
#' }
#' ```
#'
#' ## Accessing data
#'
#' Data can be accessed in both the methods and template parts of the component via the `self` keyword.
#' For example, the following component defines the data `foo`, and accesses it in the template via `self$foo`.
#'
#' ```{r}
#' x <- component(
#'     data = function() {
#'         list(
#'             foo = 1L
#'         )
#'     },
#'     template = function(ns) {
#'         self$foo
#'     }
#' )
#'
#' print(x())
#' ```
#'
#' ## Passing data
#'
#' ```{r}
#' x <- component(
#'     data = function() {
#'         list(
#'             foo = 1L
#'         )
#'     },
#'     template = function(ns) {
#'         self$foo
#'     }
#' )
#'
#' print(x(data = list(foo = 2L)))
#' ```
#'
#' @name component-data
#' @family components
#' @rdname component-data
NULL

new_data_property <- function(dat) {
    if (!is.null(dat)) {
        dat
    } else {
        no_op_data
    }
}

validate_data_property <- function(data) {
    if (!is.null(data)) {
        if (!is.function(data)) {
            return("- $data must be a function")
        }

        if (!assert_no_argument(data)) {
            return("- $data function cannot take arguments")
        }
    }
}