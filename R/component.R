#' rsx Component
#'
#' @param name TODO
#' @param data  function that returns a named list
#' @param methods named list of functions
#' @param template function that returns a taglist
#' @param styles named list of "global" and "scoped" styles
#'
#' @details
#' ## Data
#'
#' Data is used for a component's internal state and
#' can also be used to pass information from a parent component to its children.
#' Both component templates and methods have access to component data.
#'
#' The component data function must be of the following structure:
#'
#' ```r
#' function() {
#'  # must either return a named list or NULL
#'  list()
#' }
#' ```
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
#' @details
#' ## Methods
#'
#' Methods are a list of functions that are contained within a given component, and
#' typically will do work with component data.
#'
#' The `setup` method is special and is treated as the equivalent of a
#' module server for the component.
#'
#' The `render` method is special and is called when the UI of a component is rendered.
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
#' @details
#' ## Template
#'
#' The template refers to the UI-generator of a given component. This is analagous
#' to the UI function of a given {shiny} module.
#'
#' Template requires one argument: `ns`. `ns` is used the same as in shiny modules, and is used to distinguish between
#' component instances. The user does not need to supply their own `shiny::NS`
#' call -- this is handled by {rsx}.
#'
#' The component template function must be of the following structure:
#'
#' ```r
#' function(ns) {
#'  # must either return an object that can be coerced to a `shiny::tags` object or a `shiny::tagList`
#' }
#' ```
#'
#' The following is a valid template:
#'
#' ```r
#' function(ns) {
#'  shiny::div("Hello world!")
#' }
#' ```
#'
#' @details
#' ## Styles
#'
#' The styles argument is a named list of scoped and global CSS styles.
#'
#' ```r
#' list(
#'  scoped = "",
#'  global = ""
#' )
#' ````
#'
#'
#' @export
component <- function(name = paste0("unnamed_component-", random_id()),
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
        parent = parent.frame()
    )
    msg <- validate_component(this)
    if (!is.null(msg)) {
        error_component_validation(msg)
    }
    this
}

new_component <- function(name, data, methods, template, styles, parent) {
    component_id <- paste0(name, random_id())
    new <- structure(
        function(...) {
            instantiate(sys.function())(...)
        },
        class        = "component",
        component_id = component_id,
        name         = name,
        data         = create_data_property(data),
        methods      = methods,
        template     = create_template_property(template),
        styles       = create_styles_property(styles, component_id),
        .parent      = parent
    )

    register_component(name, new)
}

validate_component <- function(self) { #nolint
    # typecheck
    if (!is.null(self$data)) {
        if (!is.function(self$data)) {
            return("- $data must be a function")
        }

        if (!check_no_argument(self$data)) {
            return("- $data function cannot take arguments")
        }
    }

    if (!is.null(self$methods)) {
        if (!is.list(self$methods)) {
            return("- $methods must be a list")
        }

        for (i in seq_along(self$methods)) {
            fn <- self$methods[[i]]
            fn_name <- names(self$methods)[i]
            if (!is.function(fn)) {
                return(
                    "- Each method member must be a function"
                )
            }  else if (check_self_argument(fn)) {
                return(
                    "- `self` is a reserved word in {rsx} methods."
                )
            }
        }
    }

    if (!is.null(self$template)) {
        if (!is.function(self$template)) {
            return("- $template must be a function")
        }

        if (!identical(names(formals(self$template)), "ns")) {
            return(
                    "- $template function must have one argument: `ns`"
                )
        }
    }

    if (!is.null(self$styles)) {
        if (!is.list(self$styles)) {
            return("- $styles must be a list")
        }
        if (length(names(self$styles)) > 2L || any(names(self$styles) %nin% c("scoped", "global"))) {
            return("- $styles only supports the names `scoped` and `global`")
        }
    }

    NULL
}


create_data_property <- function(dat) {
    if (!is.null(dat)) {
        dat
    } else {
        no_op_data
    }
}

create_template_property <- function(template) {
    if (!is.null(template)) {
        template
    } else {
        no_op_template
    }
}

create_styles_property <- function(styles, hash) {
    if (!is.null(styles)) {
        if (is.list(styles)) {
            nms <- names(styles)
            if ((length(nms) > 2L && all(nms %nin% c("scoped", "global")))) {
                error_component_invalid_styles()
            }
        }

        # TODO
        # this should occur AFTER preprocessing
        if (!is.null(styles$scoped)) {
            styles$scoped <- postfix_css(
                styles$scoped,
                hash
            )
        }
        styles
    } else {
        NULL
    }
}
