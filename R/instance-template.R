set_namespace <- function(id) {
    if (!is.null(rsx_env[["ns"]])) {
        shiny::NS(sprintf("%s-%s", rsx_env$ns, id))
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
            if (length(element$children) == 1L &&
                inherits(element$children[[1L]], "shiny.tag")) {
                attribs <- manage_attributes(contents)
                element$children <- do.call(
                    htmltools::tagAppendAttributes,
                    args = c(
                        tag = element$children,
                        attribs
                    )
                )
            }
            element
        },
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
    template_query <- wrapped_query(element)
    child_query <- wrapped_query(children)

    compare_slots(template_query, child_query, instance_object)

    # unnamed children
    unnamed_query <- child_query |>
        q_filter_all(
            function(x, i) {
                is.null(x[["attribs"]][["slot"]])
            }
        )

    unnamed_tags <- q_selected(unnamed_query)
    q_drop(unnamed_query)


    # unnamed children replacement
    if (q_length(unnamed_query) > 0L) {
        template_query |>
            q_filter("slot") |>
            q_filter(function(x, i) {
                    is.null(x$attribs[["name"]])
                }
            ) |>
            q_replace(unnamed_tags) |>
            q_reset()
    }

    named_query <- child_query |>
        q_filter_all(function(x, i) {
            !is.null(x[["attribs"]][["slot"]])
        })

    named_tags <- q_selected(named_query)
    q_drop(named_query)

    # named children replacement
    for (child in named_tags) {
        insert_child <- child
        insert_child$attribs$slot <- NULL
        res <- template_query |>
            q_filter("slot") |>
            q_filter(function(x, i) {
                identical(child$attribs$slot, x$attribs$name)
            })
        if (q_length(res) > 0L) {
            res |>
                q_replace(insert_child) |>
                q_reset()
        } else {
            error_instance_slot_name(
                instance_object,
                child$attribs$slot
            )
        }
    }

    # slot fallback content
    template_query |>
        q_filter("slot") |>
        q_map(function(x, i) {
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
        }) |>
        q_reset() |>
        q_filter("slot") |>
        q_drop()

    unwrap_tags(template_query)
}

compare_slots <- function(tq, cq, instance_object) {
    tq_len <- tq |>
        q_filter("slot") |>
        q_length()

    cq_len <- cq |>
        q_filter("*") |>
        q_length()

    if (cq_len > 0L && tq_len == 0L) {
        error_instance_slot(instance_object)
    }
}

manage_attributes <- function(contents) {
    lapply(contents, function(x) {
        if (!inherits(x, "shiny.tag")) {
            x
        }
    })
}
