test_that("methods validation", {
    reset_rsx_env()
    expect_error(
        component(methods = "bad"),
        class = new_rsx_error("component_validation")
    )
    expect_error(
        component(methods = list("bad")),
        class = new_rsx_error("component_validation")
    )
    expect_error(
        component(
            methods = list(
                bad = function(self) {

                }
            )
        ),
        class = new_rsx_error("component_validation")
    )
    expect_error(
        component(
            methods = list(
                function() {
                    "bad"
                }
            )
        )(),
        class = new_rsx_error("instance_validation")
    )
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
        )(),
        class = new_rsx_error("instance_name")
    )
})


test_that(
    "method self access",
    {
        reset_rsx_env()
        c_method_self <- component(
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
        )()
        inst <- get_instances()[[1L]]
        self <- inst$internal$self

        expect_identical(inst$methods$a(), self)
        expect_identical(shiny::p("bar"), inst$methods$b())
    }
)
