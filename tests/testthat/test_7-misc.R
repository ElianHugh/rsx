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
            x()
            expect_true(length(instances_to_component_list()) == 1L)
            x <- component(name = "component_overwrite")
            x()
            expect_true(length(instances_to_component_list()) == 1L)
            y <- component(name = "component_overwrite2")
            y()
            expect_true(length(instances_to_component_list()) == 2L)
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

test_that("namespace is appropriately setup", {
    ns <- asNamespace("rsx")
    ns[["rsx_env"]] |>
        is.environment() |>
        expect_true()

    ns[["rsx_env"]][["components"]] |>
        is.list() |>
        expect_true()

    ns[["rsx_env"]][["instances"]] |>
        is.list() |>
        expect_true()
})

test_that("is.x", {
    reset_rsx_env()
    x <- component()
    expect_true(is.component(x))
    expect_true(is.instance_tag(x()))
})

test_that("subsetting", {
    reset_rsx_env()
    x <- component()
    expect_error(x["temp"])
    expect_error(x[["temp"]])
    expect_no_error(x[["template"]])
    expect_error(x["temp"] <- 5L)
    expect_error(x[["temp"]] <- 5L)
    expect_no_error(
        x[["template"]] <- function(ns) {
            shiny::div()
        }
    )
})