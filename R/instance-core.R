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

    instantiate_data_property(comp, envs)
    instantiate_methods_property(comp, envs)

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
    envs$inst_env
}

instantiate_template <- function(tmp, env) {
    environment(tmp) <- env
    invisible(tmp)
}

register_component_instance <- function(inst) {
    rsx_env[["instances"]][[inst$instance_id]] <- inst
    invisible(inst)
}
