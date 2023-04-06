.onLoad <- function(...) {
    ns <- asNamespace("rsx")
    if (is.null(ns[["rsx_env"]])) {
        ns[["rsx_env"]] <- new.env(
            parent = asNamespace("rsx")
        )
    }
    rsx_env[["components"]] <- list()
    rsx_env[["instances"]] <- list()
}
