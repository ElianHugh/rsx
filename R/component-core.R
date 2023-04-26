#' Create a component
#'
#' @description
#'
#' Components represent the encapsulation of a shiny module's
#' logic and UI into a singular object, and can be used like
#' any other shiny tag.
#'
#' For more information on the data, methods, template, and styles arguments, see related documentation.
#'
#' @param name component name
#' @param data a function that returns a named list of values, which are used to store the component's state
#' @param methods named list of functions, which define the behavior of the component
#' @param template function that returns a taglist
#' @param styles function that returns a character vector or list of CSS styles that are scoped to the component
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
component <- function(name = NULL,
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
    name <- new_name_property(name)
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
        quote(validate_data_property(self$data)),
        quote(validate_methods_property(self$methods)),
        quote(validate_template_property(self$template)),
        quote(validate_styles_property(self$styles))
    )
    for (invalidation in invalidations) {
        invalidation <- eval(invalidation)
        if (!is.null(invalidation) && nzchar(invalidation)) {
            return(invalidation)
        }
    }
    NULL
}


new_name_property <- function(name) {
    if (is.null(name)) {
        sprintf("component-%s", random_id())
    } else {
        name
    }
}