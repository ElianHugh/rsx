random_id <- function() {
    random1 <- sample(LETTERS, 2L)
    random2 <- as.numeric(format(Sys.Date(), "%y%m%d")) + sample(99999L, size = 1L)
    random3 <- sample(letters, 2L)
    paste0(
        random1,
        random2,
        random3,
        collapse = ""
    )
}

`%||%` <- function(lhs, rhs) {
    if (!shiny::isTruthy(lhs)) {
        rhs
    } else {
        lhs
    }
}

`%nin%` <- Negate(`%in%`)

is_not_null <- Negate(is.null)

get_functions <- function(env) {
    Filter(
        Negate(is.null),
        eapply(
            env,
            function(x) {
                if (!inherits(x, "reactive") && is.function(x)) {
                    x
                }
            }
        )
    )
}

get_data <- function(env) {
    Filter(
        Negate(is.null),
        eapply(
            env,
            function(x) {
                if (inherits(x, "reactive") || !is.function(x)) {
                    x
                }
            }
        )
    )
}

get_components <- function() {
    get0("components", envir = rsx_env)
}

get_instances <- function() {
    get0("instances", envir = rsx_env)
}

reset_rsx_env <- function() {
    rsx_env[["components"]] <- list()
    rsx_env[["instances"]] <- list()
    rsx_env[["ns"]] <- NULL
}

set_fn_env <- function(fn, e) {
    environment(fn) <- e
    fn
}

new_active_binding <- function(sym, getter, setter, env = sys.frame()) {
    fn <- function(v) {
        if (missing(v)) {
            getter()
        } else {
            setter(v)
        }
    }
    makeActiveBinding(sym, fn, env)
}

get_component_instances <- function(name) {
    Filter(function(inst) identical(inst$component$name, name), get_instances())
}

no_op_template <- function(ns) {
    NULL
}

no_op_data <- function() {
    NULL
}