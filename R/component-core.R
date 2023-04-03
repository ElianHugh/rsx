#' Create a component
#'
#' @description
#'
#' Components represent the encapsulation of a shiny module's
#' logic and UI into a singular object, and can be used like
#' any other shiny tag.
#'
#' @param name component name
#' @param data  function that returns a named list
#' @param methods named list of functions
#' @param template function that returns a taglist
#' @param styles character vector of length 1
#'
#' @examples
#' library(shiny)
#' counter <- component(
#'     name = "counter",
#'     data = function() {
#'         list(
#'             label = "Counter",
#'             count = reactiveVal(0L)
#'         )
#'     },
#'     template = function(ns) {
#'         tagList(
#'             actionButton(ns("button"), label = self$label),
#'             verbatimTextOutput(ns("out"))
#'         )
#'     },
#'     methods = list(
#'         setup = function(input, output, session) {
#'             observeEvent(input$button, {
#'                 self$count(
#'                     self$count() + 1L
#'                 )
#'             })
#'             output$out <- renderText(
#'                 self$count()
#'             )
#'         }
#'     )
#' )
#' tagList(
#'  tags$h1("Counter"),
#'  counter()
#' )
#'
#' @family components
#' @include
#' component-data.R,
#' component-methods.R,
#' component-styles.R,
#' component-template.R
#' @export
component <- function(name = sprintf("unnamed_component-%s", random_id()),
                      data = NULL,
                      methods = NULL,
                      template = NULL,
                      styles = NULL) {
    this <- new_component(
        name = name,
        data,
        methods,
        template,
        styles,
        namespace = parent.frame()
    )
    msg <- validate_component(this)
    if (!is.null(msg)) {
        error_component_validation(msg)
    }
    this
}

new_component <- function(name, data, methods, template, styles, namespace) {
    component_id <- sprintf("%s%s", name, random_id())
    structure(
        function(...) {
            instantiate(sys.function())(...)
        },
        class        = "component",
        component_id = component_id,
        name         = name,
        data         = new_data_property(data),
        methods      = new_methods_property(methods),
        template     = new_template_property(template),
        styles       = new_styles_property(styles, component_id),
        .namespace   = namespace
    )
}

validate_component <- function(self) { #nolint
    invalidations <- c(
        validate_data_property(self$data),
        validate_methods_property(self$methods),
        validate_template_property(self$template),
        validate_styles_property(self$styles)
    )
    for (invalidation in invalidations) {
        if (!is.null(invalidation) && nzchar(invalidation)) {
            return(invalidation)
        }
    }
    NULL
}
