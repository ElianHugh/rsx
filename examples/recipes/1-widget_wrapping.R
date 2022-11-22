library(rsx)
library(shiny)

data_table <- component(
    name = "data_table",
    data = function() {
        list(
            df = mtcars
        )
    },
    template = function(ns) {
        tagList(
            DT::dataTableOutput(ns("data_table"))
        )
    },
    methods = list(
        setup = function(input, output, session) {
            output$data_table <- DT::renderDataTable({
                self$df
            })
        }
    )
)

rsx_app(data_table)
