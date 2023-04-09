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
#' This would define the CSS styles for the anchor elements (`<a>`) in the component, setting their color to red.
#' Styles defined in this way are scoped to the component, meaning they will only apply to
#' elements within that component.
#'
#'
#' @name component-styles
#' @family components
#' @rdname component-styles
NULL


new_styles_property <- function(style_fun, hash) {
    if (!is.null(style_fun)) {
        generate_scoped_sass(style_fun, hash)
    } else {
        NULL
    }
}

validate_styles_property <- function(styles) {
    if (!is.null(styles)) {
        if (!is.character(styles)) {
            return("- $styles must be a function that returns a character vector or list")
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
