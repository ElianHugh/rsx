test_that("templates are validated", {
    expect_error(
        component(template = "bad"),
        class = new_rsx_error("component_validation")
    )
    expect_no_error(
        component(template = function(ns) {

        })
    )
    expect_error(
        component(
            template = function(ns, bad) {

            }
        ),
        class = new_rsx_error("component_validation")
    )
})

test_that("templates output shiny tags", {
    reset_rsx_env()
    c_template_output <- component(
        template = function(ns) {
            shiny::div()
        }
    )
    expect_identical(
        as.character(c_template_output()),
        as.character(shiny::div())
    )

    expect_true(is.instance_tag(c_template_output()))
    expect_true(inherits(c_template_output(), "shiny.tag"))

    c_template_output2 <- component(
        template = function(ns) {
            "test"
        }
    )
    expect_no_error(as.character(c_template_output2()))
})

test_that("template namespace is correct", {
    reset_rsx_env()
    c_namespace <- component(
        template = function(ns) {
            ns("test")
        },
        methods = list(
            setup = function(input, output, session) {
                cat(session$ns("test"))
            }
        )
    )
    template_ns <- as.character(c_namespace())
    expected_ns <- paste0(get_instances()[[1L]]$instance_id, "-test")
    expect_identical(template_ns, expected_ns)

    server_ns <- capture.output(shiny::testServer(rsx_server(), {
        # noop
    }))
    expect_identical(template_ns, server_ns)
})

test_that("templates have access to `self`", {
    reset_rsx_env()
    c_self_access <- component(
        data = function() {
            list(
                test = "test"
            )
        },
        template = function(ns) {
            shiny::div(self$test)
        }
    )
    expect_identical(
        as.character(c_self_access()),
        as.character(shiny::div("test"))
    )

    c_self_access2 <- component(
        data = function() {
            list(
                test = "test"
            )
        },
        template = function(ns) {
            shiny::withTags(
                p(self$test)
            )
        }
    )
    expect_identical(
        as.character(c_self_access2()),
        as.character(shiny::p("test"))
    )
})

test_that("templates can use basic slotting", {
    reset_rsx_env()
    c_no_slotting <- component(
        template = function(ns) {
            shiny:div()
        }
    )
    expect_error(c_no_slotting(shiny::div()))

    c_unnamed_slotting <- component(
        template = function(ns) {
            shiny::div(
                shiny::div("foo"),
                shiny::tags$slot()
            )
        }
    )
    expect_identical(
        as.character(c_unnamed_slotting()),
        as.character(
            shiny::div(shiny::div("foo"))
        )
    )
    expect_identical(
        as.character(c_unnamed_slotting(shiny::p("bar"))),
        as.character(
            shiny::div(shiny::div("foo"), shiny::p("bar"))
        )
    )

    c_named_slotting <- component(
        template = function(ns) {
            shiny::div(
                shiny::tags$slot(name = "a"),
                shiny::tags$slot(name = "b")
            )
        }
    )
    expect_identical(
        as.character(
            c_named_slotting(
                shiny::p(slot = "b", "bar"),
                shiny::p(slot = "a", "foo")
            )
        ),
        as.character(
            shiny::div(
                shiny::p("foo"),
                shiny::p("bar")
            )
        )
    )

    c_fallback_content <- component(
        template = function(ns) {
            shiny::div(
                shiny::tags$slot(name = "a", shiny::p("foo")),
                shiny::tags$slot(shiny::p("bar"))
            )
        }
    )
    expect_identical(
        as.character(c_fallback_content()),
        as.character(
            shiny::div(
                shiny::p("foo"),
                shiny::p("bar")
            )
        )
    )
})

test_that("templates can be nested", {
    reset_rsx_env()
    c_nestable <- component(
        template = function(ns) {
            shiny::div(
                shiny::tags$slot()
            )
        }
    )
    # 2 layer
    expect_identical(
        as.character(c_nestable(c_nestable())),
        as.character(shiny::div(shiny::div()))
    )
    # 3 layer
    expect_identical(
        as.character(c_nestable(c_nestable(c_nestable()))),
        as.character(shiny::div(shiny::div(shiny::div())))
    )
})

test_that("templates with top-level nodes can be passed attributes", {
    reset_rsx_env()
    c_attributes <- component(
        template = function(ns) {
            shiny::div()
        }
    )
    expect_identical(
        as.character(c_attributes(class = "foo")),
        as.character(shiny::div(class = "foo"))
    )

    c_taglist_no_attr <- component(
        template = function(ns) {
            shiny::tagList(
                shiny::div(),
                shiny::div()
            )
        }
    )
    expect_identical(
        as.character(c_taglist_no_attr(class = "foo")),
        as.character(shiny::tagList(
            shiny::div(),
            shiny::div()
        ))
    )
})

test_that("can't pass unnamed slots that don't exist", {
    reset_rsx_env()
    c_unnamed_slots <- component(
        template = function(ns) {
            shiny::div("no slot")
        }
    )
    expect_error(
        as.character(c_unnamed_slots(shiny::div())),
        class = new_rsx_error("instance_slot")
    )
})


test_that("can't pass named slots that don't exist", {
    reset_rsx_env()
    c_named_slots <- component(
        template = function(ns) {
            shiny::tags$slot(
                name = "a"
            )
        }
    )
    expect_error(
        as.character(
            c_named_slots(shiny::div(slot = "b"))
        ),
        class = new_rsx_error("instance_slot_name")
    )
})