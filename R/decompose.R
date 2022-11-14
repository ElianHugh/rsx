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
    list(
        server = inst[["methods"]][["setup"]] %||% NULL,
        ui = x
    )
}
