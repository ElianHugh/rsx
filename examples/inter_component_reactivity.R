library(rsx)

library(shiny)
x <- component(
    name = "x",
    data = function() {
        list(
            count = reactiveVal(0L)
        )
    },
    template = function(ns) {
        fluidPage(
            tagList(
                h3("Parent component:"),
                actionButton(ns("button"), "Increment"),
                textOutput(ns("txt")),
                h3("Child component:"),
                y(
                    data = list(
                        count = self$count
                    )
                )
            )
        )
    },
    methods = list(
        setup = function(input, output, session) {
            output$txt <- renderText({
                paste0("Count is: ", self$count())
            })
            observeEvent(input$button, {
                self$count(self$count() + 1L)
            })
        }
    )
)

y <- component(
    name = "y",
    data = function() {
        list(
            count = NULL
        )
    },
    template = function(ns) {
        tagList(
            actionButton(ns("button"), "Increment"),
            textOutput(ns("txt"))
        )
    },
    methods = list(
        setup = function(input, output, session) {
            output$txt <- renderText({
                paste0("Count is: ", self$count())
            })
            observeEvent(input$button, {
                self$count(self$count() + 1L)
            })
        }
    )
)

rsx_app(x)
