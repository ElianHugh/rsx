set_namespace <- function(id) {
    if (!is.null(rsx_env[["ns"]])) {
        out <- paste0(rsx_env$ns, "-", id)
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
    tag <- htmltools::tag(
        instance_object$component$name,
        varArgs = contents,
        .noWS = .noWS
    )

    hooks <- list(
        function(element) {
            element$name <- "component"
            children <- element[["children"]]
            element$children <- template(
                ns = set_namespace(instance_object$instance_id)
            )
            element <- manage_slots(element, children, instance_object)
            element <- manage_scoping(element, instance_object)

            # return element children as the node is
            # wrapped in a template node
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
        instance = instance_object
    )
}

manage_data <- function(..., instance_object) {
    contents <- list(...)
    supplied_data <- contents[["data"]]
    if (!is.null(supplied_data)) {
        if (length(instance_object$component$data) > 0L &&
            check_data_specification(supplied_data, instance_object$internal$self)) {
            contents <- contents[names(contents) != "data"]
            list2env(supplied_data, instance_object$internal$self)
        } else {
            error_instance_data(supplied_data, instance_object)
        }
    }
    contents
}

manage_scoping <- function(element, instance_object) {
    if (length(instance_object$component$styles$scoped) > 0L) {
        args <- alist("")
        names(args) <- paste0("data-rsx-", instance_object$component$component_id)
        htmltools::tagQuery(wrap_tags(element))$
            each(function(x, i) add_scoping(x, i, args))$
            find("*")$
            each(function(x, i) add_scoping(x, i, args))$
            allTags()
    } else {
        element
    }
}

add_scoping <- function(tag, i, args) {
    do.call(
        shiny::tagAppendAttributes,
        args = c(
            tag = tag,
            args
        )
    )
}

manage_slots <- function(element, children, instance_object) {
    tq <- htmltools::tagQuery(wrap_tags(element))
    slot_named_children(tq, children)
    slot_unnamed_children(tq, children)
    use_fallback_slots(tq)
    unwrap_tags(tq$allTags())
}

slot_named_children <- function(tq, children) {
    tq$
        find("slot")$
        filter(
        function(x, i) {
            !is.null(x$attribs[["name"]])
        })$
        each(
        function(x, i) {
            name <- x$attribs$name
            possible_child <- get_named_slotted_children(children)$filter(
                function(child, i) {
                    identical(child$attribs$slot, x$attribs$name)
                }
            )
            if (possible_child$length() > 0L) {
                child <- htmltools::as.tags(possible_child$selectedTags())[[1L]]
                child$attribs$slot <- NULL
                x$name <- child$name
                x$children <- child$children
                x$attribs <- child$attribs
            }
            x
        }
    )$resetSelected()
}

slot_unnamed_children <- function(tq, children) {
    # todo
    # only allow 1 default slot
    unnamed_children <- get_unnamed_slotted_children(children)
    if (unnamed_children$length() > 0L) {
        child <- htmltools::as.tags(unnamed_children$selectedTags())
        tq$
            find("slot")$
            filter(
            function(x, i) {
                is.null(x$attribs[["name"]])
            }
        )$replaceWith(child)$resetSelected()
    }
}

use_fallback_slots <- function(tq) {
   tq$
       find("slot")$
       each(function(x, i) {
       if (length(x$children) > 0L) {
            get_index <- function(parent) {
                which(as.logical(lapply(parent$children, function(child) {
                    identical(child$envKey, x$envKey)
                })))
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
}

get_named_slotted_children <- function(children) {
    taglist <- wrap_tags(children)
    htmltools::tagQuery(taglist)$
        find("*")$
        filter(function(x, i) {
        !is.null(x[["attribs"]][["slot"]])
    })
}

get_unnamed_slotted_children <- function(children) {
    taglist <- wrap_tags(children)
    htmltools::tagQuery(taglist)$
        find("*")$
        filter(function(x, i) {
        is.null(x[["attribs"]][["slot"]])
    })
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
        tagq <- x$allTags()
    }
    if (inherits(x, "shiny.tag") && identical(x$name, "template")) {
        x$children
    } else {
        x
    }
}
