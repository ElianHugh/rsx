library(shiny)
library(rsx)

#' # recreation of the module server
#' # from https://shiny.rstudio.com/articles/modules.html
counter <- component(
    name = "counter",
    data = function() {
        list(
            label = "Counter",
            count = reactiveVal(0L)
        )
    },
    template = function(ns) {
        tagList(
            actionButton(ns("button"), label = self$label),
            verbatimTextOutput(ns("out"))
        )
    },
    methods = list(
        setup = function(input, output, session) {
            observeEvent(input$button, {
                self$count(
                    self$count() + 1L
                )
            })
            output$out <- renderText(
                self$count()
            )
        }
    )
)

rsx_app(counter)