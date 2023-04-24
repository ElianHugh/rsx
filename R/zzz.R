.onLoad <- function(...) {
    ns <- asNamespace("rsx")
    if (is.null(ns[["rsx_env"]])) {
        ns[["rsx_env"]] <- new.env(
            parent = asNamespace("rsx")
        )
    }
    if (is.null(rsx_env[["components"]])) {
        rsx_env[["components"]] <- list()
    }
    if (is.null(rsx_env[["instances"]])) {
        rsx_env[["instances"]] <- list()
    }
}
