test_that("decompose", {
    x <- component(
        name = "decompose",
        template = function(ns) {
            shiny::div("foo")
        },
        methods = list(
            setup = function(input, output, session) {
                # noop
            }
        )
    )

    lst <- decompose(x())
    inst <- get_component_instances("decompose")[[1L]]

    expect_identical(
        inst$methods$setup,
        lst$server
    )

    expect_identical(
        get_tag_output(
            lst$ui
        ),
        get_tag_output(x$template())
    )

    expect_error(decompose(shiny::div()))
})
