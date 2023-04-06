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
    for (comp in components) {
        if (!is.null(comp$styles)) {
            css <- append(css, comp$styles)
        }
    }
    paste(css, collapse = "\n", sep = "\n")
}
