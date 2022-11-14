library(shiny)
library(rsx)

data_table <- component(
    name = "data_table",
    data = function() {
        list(
            df = reactiveVal(mtcars)
        )
    },
    template = function(ns) {
        fluidPage(
            tagList(
                h1("Hello Shiny!"),
                radioButtons(
                    ns("data"),
                    "Data:",
                    c("mtcars", "iris")
                ),
                br(),
                div(
                    DT::dataTableOutput(ns("data_table"))
                )
            )
        )
    },
    methods = list(
        setup = function(input, output, session) {
            output$data_table <- DT::renderDataTable({
                self$df()
            })
            observeEvent(
                input$data, {
                    if (input$data == "mtcars") {
                        self$df(mtcars)
                    } else if (input$data == "iris") {
                        self$df(iris)
                    }
                }
            )
        }
    )
)

rsx_app(data_table)