compile_styles <- function(static_path = NULL) {
    rlang::inform("Compiling component CSS...")
    res <- aggregate_styles()
    if (nzchar(res)) {
        if (is.null(static_path)) {
            sass::sass(
                res,
                sass::sass_options(
                    output_style = "compressed"
                )
            )
        } else {
            path <- file.path(static_path, "rsx.min.css")
            rlang::inform(paste0("  - CSS compiled to: ", path))
            sass::sass(
                res,
                sass::sass_options(
                    output_style = "compressed"
                ),
                output = path
            )
        }
    }
}

aggregate_styles <- function() {
    css <- list()
    components <- instances_to_component_list()
    styled_components <- Filter(function(comp) !is.null(comp$styles), components)
    styles <- vapply(styled_components, function(comp) {
        comp$styles
    }, character(length(styled_components)))
    paste(styles, collapse = "\n", sep = "\n")
}
