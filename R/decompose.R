#' Decompose a component instance
#'
#' @description
#' Given a component instance tag `x`, decompose the instance
#' into separate server and UI elements.
#'
#' @param x a shiny tag returned from calling a component
#' @return list
#'
#' @examples
#' comp <- component(
#'    name = "decompose",
#'    template = function(ns) {
#'        shiny::div("hello world")
#'    },
#'    methods = list(
#'        setup = function(input, output, session) {
#'            # noop
#'        }
#'    )
#' )
#' x <- decompose(comp())
#' print(x)
#'
#' @export
decompose <- function(x) {
    inst <- attributes(x)[["instance"]]
    stopifnot(
        "Value must be a shiny tag" = inherits(x, "shiny.tag"),
        "cannot decompose object. Value is not a component instance." = !is.null(inst)
    )
    list(
        server = inst$methods$setup %||% NULL,
        ui = x
    )
}
