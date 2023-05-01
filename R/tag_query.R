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

wrapped_query <- function(element) {
    htmltools::tagQuery(wrap_tags(element))
}

q_filter <- function(query, cond) {
    if (is.character(cond)) {
        query$find(cond)
    } else {
        query$filter(cond)
    }
}

q_filter_all <- function(query, cond) {
    q_filter(query$children("*"), cond)
}

q_map <- function(query, fn) {
    query$each(fn)
}

q_drop <- function(query) {
    query$remove()
}

q_replace <- function(query, tags) {
    query$replaceWith(tags)
}

q_reset <- function(query) {
    query$resetSelected()
}

q_selected <- function(query) {
    query$selectedTags()
}

q_length <- function(query) {
    query$length()
}