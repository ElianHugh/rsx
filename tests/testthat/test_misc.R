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
    expect_error(
        x["temp"],
        class = new_rsx_error("illegal_subset")
    )
    expect_error(
        x[["temp"]],
        class = new_rsx_error("unknown_subset")
    )
    expect_no_error(
        x[["template"]]
    )
    expect_error(
        x["temp"] <- 5L,
        class = new_rsx_error("illegal_subset")
    )
    expect_error(
        x[["temp"]] <- 5L,
        class = new_rsx_error("unknown_subset")
    )
    expect_no_error(
        x[["template"]] <- function(ns) {
            shiny::div()
        }
    )
    expect_error(
        x[["template"]] <- 5L,
        class = new_rsx_error("component_validation")
    )
})

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
            as_shiny_tag(lst$ui)
        ),
        get_tag_output(as_shiny_tag(x$template()))
    )

    expect_error(decompose(shiny::div()))
})

test_that("cannot modify instances", {
    reset_rsx_env()
    c_no_modify <- component(
        data = function() {
            list(
                test = "test"
            )
        }
    )
    inst <- c_no_modify() |>
        attr("instance")
    expect_error(inst$data$test <- "error")
})

test_that("instances are registered & returned by get_instances", {
    reset_rsx_env()
    x <- component()
    y <- component()
    x()
    y()
    expect_length(get_instances(), 2L)
    expect_length(get_component_instances(x$name), 1L)
})
