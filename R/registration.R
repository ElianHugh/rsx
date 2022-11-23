register_component_instance <- function(inst) {
    rsx_env[["instances"]][[inst$instance_id]] <- inst
    invisible(inst)
}

instances_to_component_list <- function() {
    instances <- get_instances()
    comps <- vector(mode = "list")

    for (inst in instances) {
        comp <- inst[["component"]]
        comps[[comp$name]] <- comp
    }
    comps
}
