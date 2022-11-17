#' Create an rsx app object
#'
#' @description
#' Create a new instance of an rsx application by passing a top level
#' rsx::component as the application root. This is analagous to running `shiny::shinyApp()`.
#'
#' @param root
#' an `rsx::component` object
#' @param ...
#' further arguments passed to `shiny::shinyApp`
#' @param resource_path
#' path to a resource folder, if `NULL` styles will be inlined
#' @param app_class
#' the html class attribute for the app wrapper
#'
#' @family compilation
#' @export
rsx_app <- function(root, ..., resource_path = NULL, app_class = "App") {
    stopifnot(is.character(resource_path) || is.null(resource_path))
    stopifnot(is.character(app_class))

    if (!is.null(resource_path)) {
        static_path <- compile_styles(resource_path)
    }

    if (inherits(root, "component")) {
        shiny::shinyApp(
            ui = rsx_ui(
                root,
                app_class = app_class,
                resource_path = resource_path
            ),
            server = rsx_server(),
            ...
        )
    } else {
        stop(
            "rsx app cannot be called without a top-level rsx::component passed as a root argument."
        )
    }
}

#' Create an rsx UI taglist
#'
#' @description
#' `rsx_ui` is a low-level function for creating rsx ui without the use of `rsx_app`.
#'
#' @param entry a component used as the top-level node for the shiny app
#' @param id TODO
#' @param resource_path
#' path to a resource folder, if `NULL` styles will be inlined
#' @param app_class
#' the html class attribute for the app wrapper
#'
#' @return `shiny::tagList` object
#' @family compilation
#' @export
rsx_ui <- function(
    entry,
    id = NULL,
    app_class = "App",
    resource_path = NULL
    ) {

    # used when rsx is run via module server
    if (!is.null(id)) {
        rsx_env[["ns"]] <- id
    }

    header <- if (is.null(resource_path)) {
        htmltools::tags$head(
            htmltools::tags$style(aggregate_styles())
        )
    } else {
        shiny::addResourcePath(resource_path, file.path(getwd(), resource_path))
        htmltools::tags$head(
            htmltools::singleton(
                htmltools::tags$link(
                    rel = "stylesheet",
                    href = file.path(
                        resource_path,
                        "rsx.min.css"
                    ),
                    type = "text/css"
                )
            )
        )
    }
    htmltools::tagList(
        header,
        htmltools::tags$body(
            htmltools::div(
                class = app_class,
                entry()
            )
        )
    )
}



#' Create an rsx server
#'
#' @description
#' `rsx server` is a low-level function that loads up the module servers of all instantiated rsx::components.
#' If the `rsx_app` function is not viable for your shiny application setup, the rsx_server function
#' can be used.
#'
#' @details
#' Some things to note:
#'     * Due to randomised hashing of component namepaces, shiny modules are not nested in rsx
#'     * If a component does not have a setup function, a module will not be created for the instance
#'
#' @return shiny server function
#' @family compilation
#' @export
rsx_server <- function() {
    function(input,
             output,
             session = shiny::getDefaultReactiveDomain()) {
        for (instance in names(rsx_env$instances)) {
            if (is.function(rsx_env$instances[[instance]]$methods$setup)) {
                shiny::moduleServer(
                    id      = rsx_env$instances[[instance]]$instance_id,
                    module  = rsx_env$instances[[instance]]$methods$setup,
                    session = session
                )
            }
        }
    }
}