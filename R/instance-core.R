#' Instance
#'
#' @description
#' An instance represents an instantiation of a component object,
#' and is the mechanism behind reusable components.
#'
#' When a component is called, a unique instance is created that encapsulates
#' component data and methods. This encapsulation is used to distinguish between
#' multiple instantiations of the same component (think namespacing).
#'
#' @param comp component object used to create an instance
#'
#' @examples
#' # a different namespace is created for
#' # each call to the test component
#' test <- component(
#'     template = function(ns) {
#'         shiny::div(id = ns("example"))
#'     }
#' )
#' test()
#' test() # note the different id
#'
#' @noRd
instantiate <- function(comp) {
    this <- new_instance(comp)
    new_tag_function(this, this$template)
}

new_instance <- function(comp) {
    envs <- create_environments(comp)
    this <- create_env_bindings(comp, envs)

    inst_methods <- instantiate_functions(attr(comp, "methods"), this)
    inst_data <- instantiate_data(attr(comp, "data"), this)
    inst_data_nms <- names(inst_data)
    inst_method_nms <- names(inst_methods)

    if (assert_no_name_duplication(inst_data_nms, inst_method_nms)) {
        nms <- c(inst_data_nms, inst_method_nms)
        error_instance_name_duplication(nms[duplicated(nms)], this)
    }

    validation_msg <- validate_params(inst_data, inst_methods)
    if (!is.null(validation_msg)) {
        error_instance_validation(validation_msg, this$component)
    }

    list2env(c(inst_data, inst_methods), this$internal$self)
    lockEnvironment(this$internal)
    lockEnvironment(this$internal[["self"]])

    class(this) <- c("instance", "environment")
    register_component_instance(this)
}

create_environments <- function(comp) {
    inst_env <- new.env(parent = attr(comp, ".namespace"), hash = FALSE)
    internal_env <- new.env(parent = inst_env, hash = FALSE)
    self_env <- new.env(parent = internal_env, hash = FALSE)
    list(
        inst_env = inst_env,
        internal_env = internal_env,
        self_env = self_env
    )
}

create_env_bindings <- function(comp, envs) {


    envs$inst_env$instance_id <- sprintf("instance-%s", random_id())
    envs$inst_env$component <- comp
    envs$inst_env$internal <- envs$internal_env
    envs$inst_env$template <- instantiate_template(comp$template, envs$internal_env)

    envs$internal_env$self <- envs$self_env

    data_getter <- function() {
        get_data(envs$self_env)
    }

    data_setter <- function(v) {
        error_instance_assignment(envs$inst_env, "data")
    }

    methods_getter <- function() {
        get_functions(envs$self_env)
    }

    methods_setter <- function(v) {
        error_instance_assignment(envs$inst_env, "methods")
    }

    new_active_binding("data", getter = data_getter, setter = data_setter, env = envs$inst_env)
    new_active_binding("methods", getter = methods_getter, setter = methods_setter, env = envs$inst_env)

    envs$inst_env
}

validate_params <- function(dat, methods) {
    if (length(names(dat)) != length(dat)) {
        return("$data must be a named list.")
    }
    if (length(names(methods)) != length(methods)) {
        return(
            "$methods must be a named list."
        )
    }
    NULL
}

instantiate_functions <- function(methods, inst) {
    lapply(methods, set_fn_env, inst$internal)
}

instantiate_data <- function(dat, inst) {
    environment(dat) <- inst[["internal"]]
    invisible(dat())
}

instantiate_template <- function(tmp, env) {
    environment(tmp) <- env
    invisible(tmp)
}

register_component_instance <- function(inst) {
    rsx_env[["instances"]][[inst$instance_id]] <- inst
    invisible(inst)
}
