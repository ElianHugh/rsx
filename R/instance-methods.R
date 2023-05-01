instantiate_methods_property <- function(comp, envs) {
    inst_methods <- instantiate_methods(
        attr(comp, "methods"),
        envs$inst_env
    )
    if (!is.null(inst_methods)) {
        validate_methods(comp, inst_methods, envs)
        list2env(inst_methods, envs$self_env)
    }
    methods_getter <- function() {
        get_functions(envs$self_env)
    }
    methods_setter <- function(v) {
        error_instance_assignment(envs$inst_env, "methods")
    }
    new_active_binding(
        "methods",
        getter = methods_getter,
        setter = methods_setter,
        env = envs$inst_env
    )
}

validate_methods <- function(comp, methods, envs) {
    if (length(names(methods)) != length(methods)) {
        error_instance_validation(
            "$methods must be a named list.",
            list(component = comp)
        )
    }
    inst_data_nms <- names(envs$self_env)
    inst_method_nms <- names(methods)
    if (assert_no_name_duplication(inst_data_nms, inst_method_nms)) {
        nms <- c(inst_data_nms, inst_method_nms)
        error_instance_name_duplication(nms[duplicated(nms)], envs$inst_env)
    }
}

instantiate_methods <- function(methods, inst) {
    lapply(methods, set_fn_env, inst$internal)
}