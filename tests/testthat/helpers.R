get_tag_output <- function(tag) {
    paste0(
        capture.output(
            print(tag)
        ),
        collapse = ""
    )
}