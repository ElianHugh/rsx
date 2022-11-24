library(shiny)

x <- component(
    data = function() {
        list(
            label = "Counter",
            count = reactiveVal(0L)
        )
    },
    methods = list(
        setup = function(input, output, session) {
            print(session$ns("test"))
            observeEvent(input$button, {
                self$count(
                    self$count() + 1L
                )
            })
            output$out <- renderText(
                self$count()
            )
        }
    ),
    template = function(ns) {
        tagList(
            actionButton(ns("button"), label = self$label),
            verbatimTextOutput(ns("out"))
        )
    }
)


module_ui <- function(id) {
    rsx_ui(
        root = x(),
        id = id
    )
}

ui <- module_ui("rsx")

server <- function(input, output, session) {
    rsx_module_server("rsx")
}

shinyApp(
    ui = ui,
    server = server
)
