#' @export
print.component <- function(x, ...) {
    cat(
        format(x, ...),
        sep = "\n"
    )
}

#' @export
print.instance_tag <- print.component

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
            sprintf("  Namespace: %s", rlang::env_label(ns))
        } else {
            NULL
        }
    }
    subcomps <- list("data", "template", "methods", "styles") |>
        sapply(function(y) {
            subcomp <- x[[y]]
            if (all(
                !identical(subcomp, no_op_data),
                !identical(subcomp, no_op_template),
                !is.null(subcomp)
            )
            ) {
                sprintf("   %s: %s", y, class(subcomp)[[1L]])
            }
        }) |>
        Filter(is_not_null, x = _) |>
        unlist()

    subcomp_header <- {
        if (any(vapply(subcomps, is_not_null, TRUE))) {
            "  Subcomponents: "
        } else {
            NULL
        }
    }

    c(
        header,
        subcomp_header,
        subcomps,
        namespace
    )
}

#' @export
format.instance_tag <- function(x, ...) {
    output <- pad_lhs(htmltools::HTML(as.character(x))) %||%
        cli::style_italic("  No template")
    header <- cli::col_grey("<rsx::instance>") |>
        cli::style_italic()
    component_name <- sprintf("`%s`", attr(x, "instance")$component$name) |>
        cli::col_cyan()
    c(
        sprintf("%s %s", header, component_name),
        output
    )
}

pad_lhs <- function(x) {
    strsplit(x, "\n") |>
        lapply(function(y) {
            sprintf("  %s", y)
        }) |>
        unlist() |>
        paste0(collapse = "\n")
}
