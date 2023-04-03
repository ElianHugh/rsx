#' Component Styles
#'
#' @description
#'
#' The styles argument is a character vector of length 1 that defines the styles for the component.
#' Styles are scoped to the component.
#'
#' @details
#'
#' The styles argument takes a character vector of length 1 that defines the styles for the component.
#' For example:
#'
#' ```r
#' styles = "a { color: red; }"
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


new_styles_property <- function(styles, hash) {
    if (!is.null(styles)) {
        if (!is.character(styles)) {
            error_component_invalid_styles()
        }
        postfix_css(
            styles,
            hash
        )
    } else {
        NULL
    }
}

validate_styles_property <- function(styles) {
    if (!is.null(styles)) {
        if (!is.character(styles)) {
            return("- $styles must be a character vector of length 1")
        }
    }
}