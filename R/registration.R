register_component <- function(name, comp) {
    if (!is.null(get_components()[[name]])) {
        msg <- c(
            sprintf("Overwritting previous definition of `%s` component", name)
        )
        message(paste(msg, collapse = "\n"))
        for (inst in get_instances()) {
            if (identical(inst$component$name, name)) {
                rsx_env[["instances"]][[inst$instance_id]] <- NULL
            }
        }
    }
    rsx_env[["components"]][[name]] <- comp
    invisible(comp)
}

register_component_instance <- function(inst) {
    rsx_env[["instances"]][[inst$instance_id]] <- inst
    invisible(inst)
}
