#' Component Styles
#'
#' @description
#'
#' The styles argument is a character vector of length 1
#'
#' @details
#'
#' ```r
#' "a { color: red; }"
#' ````
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