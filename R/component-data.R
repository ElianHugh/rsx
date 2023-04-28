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
#' To create component data, define a function that returns a list of named
#' objects. These objects can be Shiny reactive objects, data frames, lists,
#' or any other R object.
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
#'     name = "data_access",
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
#' To pass data to a component, use the `data` argument when calling the
#' component function. The `data` argument should be a list of named objects
#' that match the names of the objects defined in the component's data
#' function. For example:
#'
#' ```{r}
#' x <- component(
#'     name = "data_access",
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
    dat %||% no_op_data
}

validate_data_property <- function(data) {
    if (!is.null(data)) {
        if (!is.function(data)) {
            return("$data must be a function")
        }

        if (!assert_no_argument(data)) {
            return("$data function cannot take arguments")
        }
    }
}