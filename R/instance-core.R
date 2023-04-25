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
    this <- create_env_bindings(comp)

    inst_methods <- instantiate_functions(attr(comp, "methods"), this)
    inst_data <- instantiate_data(attr(comp, "data"), this)
    inst_data_nms <- names(inst_data)
    inst_method_nms <- names(inst_methods)

    if (assert_no_name_duplication(inst_data_nms, inst_method_nms)) {
        nms <- c(
            inst_data_nms,
            inst_method_nms
        )
        error_instance_name_duplication(nms[duplicated(nms)], this)
    }
    msg <- validate_params(inst_data, inst_methods)
    if (!is.null(msg)) {
        error_instance_validation(msg, this[["component"]])
    }

    list2env(c(inst_data, inst_methods), this$internal$self)
    lockEnvironment(this$internal[["self"]])
    for (name in names(inst_methods)) {
        lockBinding(name, this$internal[["self"]])
    }
    lockEnvironment(this$internal)

    class(this) <- c("instance", "environment")
    register_component_instance(this)
}

create_env_bindings <- function(comp) {
    # env hierarchy:
    # this
    # |--> internal
    #      |--> self
    this <- new.env(parent = attr(comp, ".namespace"), hash = FALSE)
    internal <- new.env(parent = this, hash = FALSE)
    internal[["self"]] <- new.env(parent = internal, hash = FALSE)

    this[["instance_id"]] <- sprintf("instance-%s", random_id())
    this[["component"]] <- comp
    this[["internal"]] <- internal
    this[["template"]] <- instantiate_template(
        this$component$template,
        this$internal
    )
    new_active_binding(
        "data",
        getter = function() {
            get_nonfunctions(this$internal$self)
        },
        setter = function(v) {
            error_instance_assignment(this, "data")
        },
        env = this
    )
    new_active_binding(
        "methods",
        getter = function() {
            get_functions(this$internal$self)
        },
        setter = function(v) {
            error_instance_assignment(this, "methods")
        },
        env = this
    )
    this
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
