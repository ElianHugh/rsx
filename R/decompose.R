#' Decompose a component instance
#'
#' @description
#' Given a component instance tag `x`, decompose the instance
#' into separate server and UI elements
#'
#' @param x a shiny tag returned from calling a component
#' @return list
#' @export
decompose <- function(x) {
    inst <- attributes(x)[["instance"]]
    if (is.null(inst)) {
        stop("Cannot decompose object. Shiny tag is not a component instance.")
    } else {
        list(
            server = inst[["methods"]][["setup"]] %||% NULL,
            ui = x
        )
    }
}
