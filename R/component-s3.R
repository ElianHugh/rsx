#' @export
`[.component` <- function(x, arg) {
    attributes(x)[names(x)][arg]
}

#' @export
`[[.component` <- function(x, arg) {
    attributes(x)[names(x)][[arg]]
}

#' @export
`$.component` <- `[[.component`

#' @export
`[<-.component` <- function(x, arg, value) {
    error_illegal_subset()
}


#' @export
`[[<-.component` <- function(x, arg, value) {
    attr(x, arg) <- value
    msg <- validate_component(x)
    if (!is.null(msg)) {
        error_component_validation(msg)
    }
    x
}

#' @export
`$<-.component` <- `[[<-.component`

#' @export
print.component <- function(x, ...) {
    cat(
        format(x, ...),
        sep = "\n"
    )
}

#' @export
print.instance_tag <- function(x, ...) {
    cat(
        format(x, ...),
        sep = "\n"
    )
}

#' @export
format.component <- function(x, ...) {
    header <- sprintf(
        "%s %s",
        cli::col_grey("<rsx::component>") |>
            cli::style_italic(),
        x$name
    )
    namespace <- sprintf("Namespace: %s", format(x$.parent))
    instances <- sprintf("Instances: %s", length(get_component_instances(x$name)))
    subcvals <- c(
        ifelse(
            identical(x[["data"]], no_op_data),
            list(NULL),
            "data"
        ),
        ifelse(
            identical(x[["template"]], no_op_template),
            list(NULL),
            "template"
        ),
        ifelse(is.null(x[["methods"]]), list(NULL), "methods"),
        ifelse(is.null(x[["styles"]]), list(NULL), "styles")
    )
    if (any(vapply(subcvals, Negate(is.null), TRUE))) {
        subcomp_header <- "Subcomponents: "
        subcomps <- vapply(subcvals, function(s) {
            if (!is.null(s)) {
                paste(" - ", s)
            } else {
                ""
            }
        }, "")
    } else {
        subcomp_header <- ""
        subcomps <- ""
    }

    c(
        header,
        namespace,
        instances,
        subcomp_header %||% NULL,
        subcomps %||% NULL
    )
}

#' @export
format.instance_tag <- function(x, ...) {
    output <- htmltools::HTML(as.character(x))
    c(
        cli::col_grey("<rsx::instance>") |>
            cli::style_italic(),
        output
    )
}

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
