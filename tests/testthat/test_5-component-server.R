test_that("server ns == template ns", {
    reset_rsx_env()
    x <- component(
        methods = list(
            setup = function(input, output, session) {
                cat(session$ns("test"))
            }
        ),
        template = function(ns) {
            ns("test")
        }
    )
    template_ns <- capture.output(print(x()))
    server_ns <- capture.output(shiny::testServer(rsx_server(), {}))

    expect_identical(
        template_ns,
        server_ns
    )
})
