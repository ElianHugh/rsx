test_that("styles validation", {
    reset_rsx_env()
    expect_no_error(
        component(
            styles = "a {}"
        )
    )
    expect_error(
        component(
            styles = list(
                bad = ""
            )
        )
    )
    expect_error(
        component(
            styles = list(
                scoped = "",
                global = "",
                bad = ""
            )
        )
    )
})

test_that("scoped styles", {
    reset_rsx_env()

    x <- component(
        styles = ""
    )

    expect_no_error(print(x()))

    reset_rsx_env()

    x <- component(
        name = "test",
        template = function(ns) {
            shiny::div()
        },
        styles =  "* { color: red }"
    )

    expect_identical(
        get_tag_output(as_shiny_tag(x())),
        sprintf('<div data-rsx-%s=""></div>', attr(x, "component_id"))
    )

    expect_identical(
        aggregate_styles(),
        sprintf('*[data-rsx-%s=\"\"] { color: red }', attr(x, "component_id"))
    )
})

test_that("styles should be overwritten with same-named components", {
    suppressMessages({
        reset_rsx_env()
        x <- component(
            name = "foo",
            styles = "a {color: red;}"
        )()
        styles <- aggregate_styles()
        expect_true(grepl("color: red", styles))

        # overwrite
        x <- component(
            name = "foo",
            styles = "a {color: blue;}"
        )()
        styles <- aggregate_styles()
        expect_false(grepl("color: red", styles))
        expect_true(grepl("color: blue", styles))
    })
})

test_that("styles are compiled even when component is created from a method", {
    suppressMessages({
        reset_rsx_env()
        x <- component(
            name = "x",
            template = function(ns) {
                self$new_y()
            },
            methods = list(
                new_y = function() {
                    y()
                }
            )
        )
        y <- component(
            name = "y",
            template = function(ns) {
                "Hello world"
            },
            styles = "* { color: blue }"
        )
        x()
        expect_true(grepl("color: blue", aggregate_styles()))
    })
})