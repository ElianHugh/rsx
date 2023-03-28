postfix_css <- function(css, hash) {
    tryCatch(
        expr = {
            css <- gsub("\r?\n|\r", "", css)

            rsx_env$js$call("scopeComponentCSS", css, hash)
        },
        warning = function(w) {
            message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))
        },
        error = function(e) {
            message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))
        }
    )
}

transform_css <- function(css) {
    tryCatch(
        expr = {
            css <- gsub("\r?\n|\r", "", css)
            rsx_env$js$call("autoprefixCSS", css)
        }, warning = function(w) {
            message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))
            # code
        }, error = function(e) {
            message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))
            # code
        }
    )
}

minify_css <- function(css) {
    tryCatch(
        expr = {
            css <- gsub("\r?\n|\r", "", css)
            rsx_env$js$call("minifyCSS", css)
            css
        }, warning = function(w) {
            message(sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]]))
            # code
        }, error = function(e) {
            message(sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]]))
            # code
        }
    )
}


compile_styles <- function(static_path = NULL) {
    message("Compiling component CSS...")
    res <- aggregate_styles()
    if (is.null(static_path)) {
        res
    } else {
        path <- file.path(static_path, "rsx.min.css")
        message(paste0("  - CSS compiled to: ", path))
        con <- file(path)
        writeLines(res, con)
        close(con)
        static_path
    }
}


aggregate_styles <- function() {
    css <- list()
    components <- instances_to_component_list()
    for (id in names(components)) {
        comp <- components[[id]]
        if (!is.null(comp$styles)) {
            css <- append(css, comp$styles)
        }
    }
    css <- paste(css, collapse = "\n", sep = "\n")
    transform_css(css) |>
        minify_css()
}
