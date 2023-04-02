#' Component Methods
#'
#' @description
#'
#' Methods are a list of functions that are contained within a given component,
#' and typically will do work with component data.
#'
#' Methods can be accessed in both other methods and the component template
#' via the `self` keyword.
#'
#' @details
#'
#' The following is an excerpt from a simple counter component:
#'
#' ```r
#' methods = list(
#'     setup = function(input, output, session) {
#'         output$txt <- renderText({
#'             paste0("Count is: ", self$count())
#'         })
#'         observeEvent(input$button, {
#'             self$count(self$count() + 1L)
#'         })
#'     },
#'     increment = function() {
#'         self$count(self$count() + 1L)
#'     }
#' )
#' ```
#'
#' ## Hooks
#'
#' ### Setup
#'
#' The `setup` hook is defined by passing a function named "setup" to the
#' methods list. Setup is used as the module server for the component.
#'
#' ```r
#' setup = function(input, output, session) {
#'
#' }
#' ````
#'
#' ### Render
#'
#' The `render` hook is defined by passing a function named "render" to
#' the methods list. The render hook allows for the modification of template code
#' prior to its rendering.
#'
#' ```r
#' render = function(element) {
#'
#' }
#' ````
#'
#' @name component-methods
#' @family components
#' @rdname component-methods
NULL

new_methods_property <- function(methods) {
    if (!is.null(methods)) {
        methods
    } else {
        NULL
    }
}

validate_methods_property <- function(methods) {
    if (!is.null(methods)) {
        if (!is.list(methods)) {
            return("- $methods must be a list")
        }

        for (i in seq_along(methods)) {
            fn <- methods[[i]]
            fn_name <- names(methods)[i]
            if (!is.function(fn)) {
                return(
                    "- Each method member must be a function"
                )
            } else if (assert_self_argument(fn)) {
                return(
                    "- `self` is a reserved word in {rsx} methods."
                )
            }
        }
    }
}
