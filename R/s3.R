#' @export
`[.component` <- function(x, arg) {
    error_illegal_subset()
}

#' @export
`[[.component` <- function(x, arg) {
    nms <- c(names(x), ".namespace", "component_id")
    if (arg %in% nms) {
        attributes(x)[nms][[arg]]
    } else {
        error_unknown_subset(arg)
    }
}

#' @export
`$.component` <- `[[.component`

#' @export
`[<-.component` <- function(x, arg, value) {
    error_illegal_subset()
}

#' @export
`[[<-.component` <- function(x, arg, value) {
    if (arg %in% names(x)) {
        attr(x, arg) <- value
        msg <- validate_component(x)
        if (!is.null(msg)) {
            error_component_validation(msg, x$name)
        }
        x
    } else {
        error_unknown_subset(arg)
    }

}

#' @export
`$<-.component` <- `[[<-.component`

#' @export
names.component <- function(x) {
    c(
        "name",
        "data",
        "methods",
        "template",
        "styles"
    )
}

#' Is a component
#'
#' @param x object
#' @export
is.component <- function(x) {
    inherits(x, "component")
}

#' Is an instance tag
#'
#' @param x object
#' @export
is.instance_tag <- function(x) {
    inherits(x, "instance_tag")
}
