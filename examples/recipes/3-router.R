library(shiny)
library(rsx)

# create dummy components for UI
comps <- lapply(
    c("ui1", "ui2", "ui3"),
    function(x) {
        component(
            name = x,
            template = function(ns) {
                div(rsx:::random_id())
            }
        )
    }
)

routes <- c(
    list(
        shiny.router::route(
            path = "/",
            ui = shiny::div(comps[[1L]]())
        ),
        shiny.router::route(
            path = "two",
            ui = shiny::div(comps[[2L]]())
        ),
        shiny.router::route(
            path = "three",
            ui = shiny::div(comps[[3L]]())
        )
    )
)

router <- component(
    name = "router",
    data = function() {
        list(
            routes = list(NULL),
            router = list(NULL)
        )
    },
    template = function(ns) {
        if (!is.null(self$routes)) {
            self$router <- purrr::lift(shiny.router::make_router)(self$routes)
            self$router$ui
        }
    },
    methods = list(
        setup = function(input, output, session) {
            if (!is.null(self$router)) {
                self$router$server(input, output, session)
            }
        }
    )
)

app <- component(
    "main",
    template = function(ns) {
        fluidPage(
            h1("Example: {shiny.router}"),
            div(
                tags$button(
                    a("Page One", href = shiny.router::route_link("/"))
                ),
                tags$button(
                    a("Page Two", href = shiny.router::route_link("two"))
                ),
                tags$button(
                    a("Page Three", href = shiny.router::route_link("three"))
                ),
            ),
            router(
                data = list(routes = routes)
            )
        )
    }
)


rsx_app(app)
