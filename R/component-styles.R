#' Component Styles
#'
#' @description
#'
#' The styles argument is function that returns a character vector or list that defines the styles for the component.
#' Styles are scoped to the component.
#'
#' @details
#'
#' The styles argument takes a character vector of length 1 that defines the styles for the component.
#' For example:
#'
#' ```r
#' styles = function() {
#' "a { color: red; }"
#' }
#' ````
#'
#' This would define the CSS styles for the anchor
#' elements (`<a>`) in the component, setting their color to red.
#'
#' # Scoped Styles
#'
#' Styles defined in a component are scoped to the component,  meaning they will only apply to
#' elements within that component.
#'
#' To style the top-level node of the component, we
#' can apply the styles without specifying a tag:
#'
#' ```r
#' x <- component(
#'  name = "scoped_styles",
#'  template = function(ns) {
#'      shiny::div(
#'       "Hello world!"
#'      )
#'  },
#'  styles = function() {
#'     "color: red"
#'   }
#' )
#' ```
#'
#' @name component-styles
#' @family components
#' @rdname component-styles
NULL


new_styles_property <- function(style_fun, hash, name) {
    msg <- validate_styles_precompile(style_fun)
    if (!is.null(msg) && nzchar(msg)) {
        error_component_validation(msg, name)
    }
    if (!is.null(style_fun) && is.function(style_fun)) {
        generate_scoped_sass(style_fun, hash)
    } else {
        NULL
    }
}

validate_styles_precompile <- function(styles) {
    if (!is.null(styles) && !is.function(styles)) {
        return("$styles must be a function that returns a character vector or list")
    }
}

validate_styles_property <- function(styles) {
    if (!is.null(styles)) {
        if (!is.character(styles)) {
            return("$styles must be a function that returns a character vector or list")
        }
    }
}

generate_scoped_sass <- function(style_fn, hash) {
    styles <- style_fn()
    if (is.list(styles)) {
        styles <- paste(styles, collapse = "\n", sep = "\n")
    }
    sprintf(".%s { %s }", hash, styles) |>
        sass::sass()
}
