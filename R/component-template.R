#' Component Template
#'
#' @description
#'
#' The template refers to the UI-generator of a given component.
#' This is analagous to the UI function of a given {shiny} module.
#'
#'
#' @details
#'
#' The component template function must be of the following structure (note
#' the ns argument):
#'
#' ```r
#' function(ns) {
#'  # must either return an object that can
#'  # be coerced to a `shiny::tags` object or a `shiny::tagList`
#' }
#' ```
#'
#' The following is an example of a valid template:
#'
#' ```r
#' function(ns) {
#'  shiny::div("Hello world!")
#' }
#' ```
#'
#' # Namespacing
#'
#' The template function requires one argument: `ns`.
#' `ns` is used identically to shiny modules, and
#' helps distinguish between component instances.
#'
#' # Slots
#'
#' Component templates can be nested through the use of slots.
#' Slots are placeholder elements that tell {rsx} where tags should be placed.
#'
#' ```{r}
#' x <- component(
#'     name = "slots",
#'     template = function(ns) {
#'         shiny::tagList(
#'             shiny::tags$slot(),
#'             shiny::p("bar")
#'         )
#'     }
#' )
#'
#' print(x(shiny::p("foo")))
#' ```
#'
#' You can also specify if you'd like content to be used in the case that a
#' slot isn't filled -- this is typically called "fallback" content.
#'
#' ```{r}
#' x <- component(
#'     name = "fallback",
#'     template = function(ns) {
#'         shiny::tags$slot(
#'             shiny::p("Hello!")
#'         )
#'     }
#' )
#'
#' print(x())
#' ```
#'
#' ## Named Slots
#'
#' Slots can be further distinguished by name attributes, which
#' can be used to target specific areas of the template.
#'
#' ```{r}
#' x <- component(
#'     name = "named_slots",
#'     template = function(ns) {
#'         shiny::tagList(
#'             shiny::tags$slot(name = "a"),
#'             shiny::tags$slot(name = "b")
#'         )
#'     }
#' )
#' print(x(shiny::p("bar", slot = "b"), shiny::p("foo", slot = "a")))
#' ```
#'
#' @name component-template
#' @family components
#' @rdname component-template
NULL

new_template_property <- function(template) {
    template %||% no_op_template
}

validate_template_property <- function(template) {
    if (!is.null(template)) {
        if (!is.function(template)) {
            return("$template must be a function")
        }

        if (!identical(names(formals(template)), "ns")) {
            return(
                "$template function must have one argument: `ns`"
            )
        }
    }
}
