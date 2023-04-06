new_rsx_error <- function(type) {
    sprintf("rsx_%s_error", type)
}

new_rsx_warning <- function(type) {
    sprintf("rsx_%s_warning", type)
}

error_illegal_subset <- function() {
    stop(
        "{rsx} does not support `[` subsetting components",
        call. = FALSE
    )
}

# ~ Component errors ~~~

error_component_invalid_styles  <- function() {
    msg <- c(
        "<rsx::component> object is invalid:",
        "- Component styles argument must be a named list of scoped and/or global elements"
    )
    stop(
        paste(msg, collapse = "\n"),
        call. = FALSE
    )
}

error_component_validation <- function(msg) {
    msg <- c(
        "<rsx::component> object is invalid:",
        msg
    )
    stop(
        paste(msg, collapse = "\n"),
        call. = FALSE
    )
}

# ~ Instance errors ~~~

instance_invalid_txt <- function(instance_object) {
    sprintf(
        "<rsx::component> `%s` instance is invalid:",
        instance_object$component$name
    )
}

error_instance_validation <- function(msg, instance_object) {
    msg <- c(
        instance_invalid_txt(instance_object),
        msg
    )
    stop(
        paste(msg, collapse = "\n"),
        call. = FALSE
    )
}

error_instance_name_duplication <- function(duplicates, instance_object) { #nolint
    msg <- c(
        instance_invalid_txt(instance_object),
        "- data and method names must be unique.",
        sprintf("- duplicated names: `%s`", format(duplicates))
    )
    stop(
        paste(msg, collapse = "\n"),
        call. = FALSE
    )
}

error_instance_data <- function(supplied_data, instance_object) {
    msg <- c(
        instance_invalid_txt(instance_object),
        sprintf(
            "- Unknown name `%s` supplied to component instance",
            names(supplied_data)
        ),
        ifelse(
            length(instance_object$component$data) > 0L,
            paste0(
                "- Allowed data: ",
                names(instance_object$component$data),
                collapse = ", "
            ),
            "- Component does not have any data defined"
        )
    )
    stop(
        paste(msg, collapse = "\n"),
        call. = FALSE
    )
}

error_instance_slot <- function(instance_object) {
    msg <- c(
        instance_invalid_txt(instance_object),
        "- Cannot supply further content to component template",
        "- A <slot> element specified in the component's template is required."
    )
    stop(
        paste(msg, collapse = "\n"),
        call. = FALSE
    )
}

error_instance_assignment <- function(instance_object, assigned_name) {
    msg <- c(
        sprintf(
            "Attempted to assign to %s attribute of `%s` component instance",
            assigned_name,
            instance_object$component$name
        ),
        sprintf("- Cannot modify %s outside of an instance context.", assigned_name),
        "- Modification can only occur inside component methods"
    )
    stop(
        paste(msg, collapse = "\n"),
        call. = FALSE
    )
}

error_component_runtime <- function(instance_object) {
    msg <- c(
        sprintf("rsx component `%s` was called during Shiny runtime", instance_object$component$name),
        "- components cannot be rendered outside of `template` functions",
        "- was a component called from a setup function?"
    )
    stop(
        paste(msg, collapse = "\n"),
        call. = FALSE
    )
}