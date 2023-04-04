.onLoad <- function(...) {
    ns <- asNamespace("rsx")
    if (is.null(ns[["rsx_env"]])) {
        ns[["rsx_env"]] <- new.env(
            parent = asNamespace("rsx")
        )
        rsx_env[["components"]] <- list()
        rsx_env[["instances"]] <- list()
    }

    if (is.null(rsx_env[["js"]])) {
        rsx_env[["js"]] <- V8::v8(
            global = "global",
            console = FALSE
        )
        rsx_env$js$source(system.file("js", "rsx.js", package = "rsx"))
    }
}
