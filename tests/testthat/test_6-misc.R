test_that("lexical scoping", {
    reset_rsx_env()
    comp_factory <- function(i) {
        component(
            name = paste0("#", i),
            data = function() {
                list(
                    i = i
                )
            },
            methods = list(
                foo = function() {
                    print(i)
                }
            ),
            template = function(ns) {
                i
            }
        )
    }

    comp <- comp_factory(1L)
    res <- comp()
    inst <- get_instances()[[1L]]

    expect_s3_class(comp, "component")
    expect_s3_class(res, "shiny.tag")
    expect_equal(inst$data$i, 1L)
    expect_output(print(res), "1")
    expect_output(inst$methods$foo(), "1")
})

test_that("withTags", {
    reset_rsx_env()
    expect_no_error(
        component(
            template = function(ns) {
                shiny::p("Foo")
            }
        )()
    )

    expect_no_error(
        component(
            template = function(ns) {
                shiny::withTags(
                    shiny::tagList(
                        x(),
                        p("Bar")
                    )
                )
            }
        )()
    )
})

test_that(
    "components overwrite components with the same name",
    {
        reset_rsx_env()
        suppressMessages({
            x <- component(name = "component_overwrite")
            expect_true(length(get_components()) == 1L)
            x <- component(name = "component_overwrite")
            expect_true(length(get_components()) == 1L)
            y <- component(name = "component_overwrite2")
            expect_true(length(get_components()) == 2L)
        })
    }
)

test_that(
    "programmatic component creation",
    {
        reset_rsx_env()
        comps <- lapply(
            seq(1L),
            function(i) {
                component(
                    name = paste0("component-", i),
                    template = function(ns) {
                        i
                    }
                )
            }
        )
        expect_no_error(print(comps[[1L]]()))
    }
)
