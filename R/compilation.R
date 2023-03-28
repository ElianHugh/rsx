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


    if (inherits(root, "component")) {
        root <- root()

        if (
            !is.null(resource_path) &&
            requireNamespace("V8", quietly = TRUE)
        ) {
            static_path <- compile_styles(resource_path)
        }
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
#' @param root a component used as the top-level node for the shiny app
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
    root,
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
                root
            )
        )
    )
}



#' Create an rsx server
#'
#' @description
#' `rsx_server` is a low-level function that loads up the module servers of all instantiated rsx::components.
#' If the `rsx_app` function is not viable for your shiny application setup, the rsx_server function
#' can be used.
#'
#' @details
#' Some things to note:
#' * Due to randomised hashing of component namepaces, shiny modules are not nested in rsx
#' * If a component does not have a setup function, a module server will not be created for the instance
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

#' Create an rsx module server
#'
#' @description
#' `rsx_module_server` is a low-level function that loads up the module servers of all instantiated components.
#' This is similar to `rsx_server`, but also allows for namespacing
#' the rsx server.
#'
#' @param id unique namespace
#' @export
rsx_module_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
        for (instance in names(rsx_env$instances)) {
            if (is.function(rsx_env$instances[[instance]]$methods$setup)) {
                shiny::moduleServer(
                    id = rsx_env$instances[[instance]]$instance_id,
                    module  = rsx_env$instances[[instance]]$methods$setup,
                    session = session
                )
            }
        }
    })
}