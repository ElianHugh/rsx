instantiate_data_property <- function(comp, envs) {
    inst_data <- instantiate_data(
        attr(comp, "data"),
        envs$inst_env
    )
    if (!is.null(inst_data)) {
        validate_data(comp, inst_data)
        list2env(inst_data, envs$self_env)
    }
    data_getter <- function() {
        get_data(envs$self_env)
    }
    data_setter <- function(v) {
        error_instance_assignment(envs$inst_env, "data")
    }
    new_active_binding(
        "data",
        getter = data_getter,
        setter = data_setter,
        env = envs$inst_env
    )
}

validate_data <- function(comp, dat) {
    if (length(names(dat)) != length(dat)) {
        error_instance_validation(
            "$data must be a named list.",
            list(component = comp)
        )
    }
}

instantiate_data <- function(dat, inst) {
    set_fn_env(dat, inst$internal)
    invisible(dat())
}
