test_that("template validation", {
    reset_rsx_env()
    expect_error(
        component(
            template = "bad"
        )
    )
})


test_that(
    "template self access",
    {
        reset_rsx_env()
        comp <- component(
            data = function() {
                list(
                    bar = "bar"
                )
            },
            methods = list(
                a = function() {
                    shiny::p(self$bar)
                }
            ),
            template = function(ns) {
                shiny::tagList(
                    self$a(),
                    shiny::p(self$bar)
                )
            }
        )
        expect_identical(
            get_tag_output(as_shiny_tag(comp())),
            get_tag_output(shiny::tagList(
                shiny::p("bar"),
                shiny::p("bar")
            ))
        )
    }
)

test_that("slotting", {
    reset_rsx_env()
    x <- component(
        template = function(ns) {
            shiny::tagList(
                shiny::tags$slot(),
                shiny::tags$slot(name = "a")
            )
        }
    )
    expect_identical(
        get_tag_output(as_shiny_tag(x())),
        ""
    )

    reset_rsx_env()
    x <- component(
        name = "slot_test1",
        template = function(ns) {
            shiny::tagList(
                shiny::tags$slot(),
                shiny::tags$slot(name = "a"),
                shiny::div(
                    shiny::tags$slot(name = "b")
                ),
                shiny::tags$slot(name = "c", shiny::p("baz"))
            )
        }
    )

    expect_identical(
        get_tag_output(as_shiny_tag(x())),
        get_tag_output(
            shiny::tagList(
                shiny::div(),
                shiny::p("baz")
            )
        )
    )

    expect_identical(
        get_tag_output(
            as_shiny_tag(x(shiny::p("foo")))
        ),
        get_tag_output(
            shiny::tagList(
                shiny::p("foo"),
                shiny::div(),
                shiny::p("baz")
            )
        )
    )

    expect_identical(
        get_tag_output(
            as_shiny_tag(x(shiny::p("foo", slot = "b")))
        ),
        get_tag_output(
            shiny::tagList(
                shiny::div(
                    shiny::p("foo")
                ),
                shiny::p("baz")
            )
        )
    )

    expect_identical(
        get_tag_output(
            as_shiny_tag(x(
                shiny::p("foo", slot = "a"),
                shiny::p("bar", slot = "b"),
                shiny::p("baz", slot = "c"),
                shiny::p("foobarbaz")
            ))
        ),
        get_tag_output(
            shiny::tagList(
                shiny::p("foobarbaz"),
                shiny::p("foo"),
                shiny::div(
                    shiny::p("bar")
                ),
                shiny::p("baz")
            )
        )
    )
})

test_that("nesting", {
    reset_rsx_env()
    x <- component(
        template = function(ns) {
            shiny::div(
                shiny::tags$slot()
            )
        }
    )

    ## https://github.com/ElianHugh/rsx/issues/6
    # 2 layer
    expect_identical(
        get_tag_output(
            as_shiny_tag(x(
                x()
            ))
        ),
        get_tag_output(
            shiny::div(
                shiny::div()
            )
        )
    )

    # 3 layer
    expect_identical(
        get_tag_output(
            as_shiny_tag(x(
                x(
                    x()
                )
            ))
        ),
        get_tag_output(
            shiny::div(
                shiny::div(
                    shiny::div()
                )
            )
        )
    )


    #
})
