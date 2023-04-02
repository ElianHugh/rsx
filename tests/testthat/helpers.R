get_tag_output <- function(tag) {
    paste0(
        capture.output(
            print(tag)
        ),
        collapse = ""
    )
}

as_shiny_tag <- function(x) {
    class(x) <- "shiny.tag"
    x
}
