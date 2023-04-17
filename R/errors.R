new_rsx_error <- function(type) {
    sprintf("rsx_%s_error", type)
}

new_rsx_warning <- function(type) {
    sprintf("rsx_%s_warning", type)
}

error_illegal_subset <- function() {
    rlang::abort(
        "{rsx} does not support `[` subsetting components",
        class = new_rsx_error("illegal_subset")
    )
}

error_unknown_subset <- function(x) {
    msg <- sprintf("{rsx} components do not have a `%s` property", x)
    rlang::abort(
        msg,
        class = new_rsx_error("unknown_subset")
    )
}

# ~ Component errors ~~~

error_component_validation <- function(msg) {
    msg <- c(
        "<rsx::component> object is invalid:",
        msg
    )
    rlang::abort(
        msg,
        class = new_rsx_error("component_validation")
    )
}

# ~ Instance errors ~~~

instance_invalid_txt <- function(instance_object) {
    sprintf(
        "<rsx::component> `%s` instance is invalid:",
        instance_object$name
    )
}

error_instance_validation <- function(msg, instance_object) {
    msg <- c(
        instance_invalid_txt(instance_object),
        msg
    )
    rlang::abort(
        msg,
        class = new_rsx_error("instance_validation")
    )
}

error_instance_name_duplication <- function(duplicates, instance_object) { #nolint
    msg <- c(
        instance_invalid_txt(instance_object),
        "data and method names must be unique.",
        sprintf("duplicated names: `%s`", format(duplicates))
    )
    rlang::abort(
        msg,
        class = new_rsx_error("instance_name")
    )
}

error_instance_data <- function(supplied_data, instance_object) {
    msg <- c(
        instance_invalid_txt(instance_object),
        sprintf(
            "Unknown name `%s` supplied to component instance",
            names(supplied_data)
        ),
        ifelse(
            length(instance_object$component$data) > 0L,
            paste0(
                "Allowed data: ",
                names(instance_object$component$data),
                collapse = ", "
            ),
            "Component does not have any data defined"
        )
    )
    rlang::abort(
        msg,
        class = new_rsx_error("instance_data")
    )
}

error_instance_slot <- function(instance_object) {
    msg <- c(
        instance_invalid_txt(instance_object),
        "Cannot supply further content to component template",
        "A <slot> element specified in the component's template is required."
    )
    rlang::abort(
        msg,
        class = new_rsx_error("instance_slot")
    )
}

error_instance_slot_name <- function(instance_object, nm) {
    msg <- c(
        instance_invalid_txt(instance_object),
        "Cannot supply further content to component template",
        sprintf("There is no <slot> element with name `%s` specified in the template.", nm)
    )
    rlang::abort(
        msg,
        class = new_rsx_error("instance_slot")
    )
}

error_instance_assignment <- function(instance_object, assigned_name) {
    msg <- c(
        sprintf(
            "Attempted to assign to %s attribute of `%s` component instance",
            assigned_name,
            instance_object$component$name
        ),
        sprintf("Cannot modify %s outside of an instance context.", assigned_name),
        "Modification can only occur inside component methods"
    )
    rlang::abort(
        msg,
        class = new_rsx_error("instance_assignment")
    )
}

error_component_runtime <- function(instance_object) {
    msg <- c(
        sprintf("rsx component `%s` was called during Shiny runtime", instance_object$component$name),
        "components cannot be rendered outside of `template` functions",
        "was a component called from a setup function?"
    )
    rlang::abort(
        msg,
        class = new_rsx_error("runtime")
    )
}
