#' @export
`[.component` <- function(x, arg) {
    error_illegal_subset()
}

#' @export
`[[.component` <- function(x, arg) {
    if (arg %in% names(x)) {
        attributes(x)[names(x)][[arg]]
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
            error_component_validation(msg)
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
        cli::col_cyan(sprintf("`%s`", x$name))
    )
    namespace <- {
        ns <- x$.namespace
        if (!is.null(ns)) {
            sprintf("Namespace: %s", ns)
        } else {
            NULL
        }
    }
    instances <- sprintf("Instances: %s", length(get_component_instances(x$name)))
    subcvals <- c(
        {
            if (identical(x[["data"]], no_op_data)) {
                list(NULL)
            } else {
                "data"
            }
        },
        {
          if (identical(x[["template"]], no_op_template)) {
                list(NULL)
          } else {
                "template"
          }
        },
        {
            if (is.null(x[["methods"]])) {
                list(NULL)
            } else {
                "methods"
            }
        },
        {
            if (is.null(x[["styles"]])) {
                list(NULL)
            } else {
                "styles"
            }
        }
    )
    if (any(vapply(subcvals, Negate(is.null), TRUE))) {
        subcomp_header <- "Subcomponents: "
        subcomps <- lapply(
            subcvals,
            function(s) {
                if (!is.null(s)) {
                    sprintf(" - %s", s)
                } else {
                    NULL
                }
            }
        ) |>
            unlist()
    } else {
        subcomp_header <- NULL
        subcomps <- NULL
    }

    c(
        header,
        namespace,
        instances,
        subcomp_header,
        subcomps
    )
}

#' @export
format.instance_tag <- function(x, ...) {
    output <- htmltools::HTML(as.character(x))
    header <- cli::col_grey("<rsx::instance>") |>
        cli::style_italic()
    header2 <- sprintf("`%s`", attr(x, "instance")$component$name) |>
        cli::col_cyan()
    c(
        sprintf("%s %s", header, header2),
        output
    )
}