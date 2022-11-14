test_that("methods validation", {
    reset_rsx_env()
    expect_error(
        component(methods = "bad")
    )
    expect_error(
        component(methods = list("bad"))
    )
    expect_error(
        component(
            methods = list(
                bad = function(self) {

                }
            )
        )
    )
    expect_error(
        component(
            methods = list(function() {
                "bad"
            })
        )()
    )
})

test_that("self validation", {
    reset_rsx_env()

    # name duplication
    expect_error(
        component(
            data = function() {
                list(
                    a = "bad"
                )
            },
            methods = list(
                a = function() {

                }
            )
        )()
    )
})

test_that(
    "method self access",
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
                    self
                },
                b = function() {
                    shiny::p(self$bar)
                }
            )
        )
        comp()
        inst <- get_instances()[[1L]]
        self <- inst$internal$self

        expect_identical(inst$methods$a(), self)
        expect_identical(shiny::p("bar"), inst$methods$b())
    }
)
