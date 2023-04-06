set_namespace <- function(id) {
    if (!is.null(rsx_env[["ns"]])) {
        out <- sprintf("%s-%s", rsx_env$ns, id)
        shiny::NS(out)
    } else {
        shiny::NS(id)
    }
}

new_tag_function <- function(instance_object, template) {
    function(..., .noWS = NULL) {
        contents <- manage_data(
            ...,
            instance_object = instance_object
        )
        template_tag(instance_object, template, contents, .noWS)
    }
}

template_tag <- function(instance_object, template, contents, .noWS = NULL) {
    if (shiny::isRunning()) {
        error_component_runtime(instance_object)
    }

    tag <- htmltools::tag(
        instance_object$component$name,
        varArgs = contents,
        .noWS = .noWS
    )

    el <- template(
        ns = set_namespace(instance_object$instance_id)
    )

    hooks <- list(
        function(element) {
            element$name <- "component"
            children <- element[["children"]]
            element$children <- el
            element <- manage_slots(element, children, instance_object)
            # return element children as the node is
            # wrapped in a template node
            unwrap_tags(element)
        },
        function(element) {
            element <- manage_scoping(element, instance_object)
            unwrap_tags(element)
        },
        instance_object$methods[["render"]] %||% NULL,
        function(element) {
            element$children
        }
    )

    for (hook in hooks) {
        if (is.function(hook)) {
            tag <- htmltools::tagAddRenderHook(
                tag,
                hook,
                replace = FALSE
            )
        }
    }

    structure(
        tag,
        # used for decomposition
        instance = instance_object,
        class = c("instance_tag", "shiny.tag")
    )
}

manage_data <- function(..., instance_object) {
    contents <- list(...)
    supplied_data <- contents[["data"]]
    if (!is.null(supplied_data)) {
        if (length(instance_object$component$data) > 0L &&
            assert_data_specification(supplied_data, instance_object$internal$self)) {
            contents <- contents[names(contents) != "data"]
            list2env(supplied_data, instance_object$internal$self)
        } else {
            error_instance_data(supplied_data, instance_object)
        }
    }
    contents
}

manage_scoping <- function(element, instance_object) {
    if (!is.null(instance_object$component$styles)) {
        args <- attr(instance_object$component, "component_id")
        htmltools::tagQuery(element)$
            children()$
            each(function(x, i) add_scoping(x, i, args))$
            allTags()
    } else {
        element
    }
}

add_scoping <- function(tag, i, args) {
    do.call(
        htmltools::tagAppendAttributes,
        args = c(
            tag = tag,
            class = args
        )
    )
}

manage_slots <- function(element, children, instance_object) {
    template_query <- htmltools::tagQuery(
        wrap_tags(element)
    )

    child_query <- htmltools::tagQuery(
        wrap_tags(children)
    )

    # unnamed children
    unnamed_query <- child_query$
        children("*")$
        filter(function(x, i) {
        is.null(x[["attribs"]][["slot"]])
    })

    unnamed_tags <- unnamed_query$selectedTags()
    unnamed_query$remove()
    template_query$find("slot")$
        filter(
        function(x, i) {
            is.null(x$attribs[["name"]])
        }
    )$replaceWith(unnamed_tags)$
        resetSelected()

    named_query <- child_query$
        children("*")$
        filter(function(x, i) {
        !is.null(x[["attribs"]][["slot"]])
    })

    named_tags <- named_query$selectedTags()
    named_query$remove()

    # named children
    for (child in named_tags) {
        insert_child <- child
        insert_child$attribs$slot <- NULL
        template_query$find("slot")$
            filter(
            function(x, i) {
                identical(child$attribs$slot, x$attribs$name)
            }
        )$replaceWith(insert_child)$resetSelected()
    }

    template_query$
        find("slot")$
        each(function(x, i) {
        if (length(x$children) > 0L) {
            get_index <- function(parent) {
                vapply(parent$children, function(child) {
                    identical(child$envKey, x$envKey)
                }, logical(1L)) |>
                    which()
            }
            index <- get_index(x$parent)
            x$parent$children <- append(
                x$parent$children,
                x$children,
                index
            )
        }
        x
    })$resetSelected()$
        find("slot")$
        remove()


    unwrap_tags(
        template_query
    )
}

wrap_tags <- function(x) {
    if (!inherits(x, "shiny.tag") || !identical(x$name, "template")) {
        shiny::tags$template(x)
    } else {
        x
    }
}

unwrap_tags <- function(x) {
    if (inherits(x, "shiny.tag.query")) {
        x <- x$allTags()
    }
    if (inherits(x, "shiny.tag") && identical(x$name, "template")) {
        x$children
    } else {
        x
    }
}
