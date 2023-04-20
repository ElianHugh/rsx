test_that("styles validation", {
    reset_rsx_env()
    expect_no_error(
        component(
            styles = function() {
                "a {}"
            }
        )
    )
    expect_error(
        component(
            styles = function() {
                1L
            }
        )
    )
    expect_error(
        component(
            styles = list(
                scoped = "",
                global = "",
                bad = ""
            )
        ),
        class = new_rsx_error("component_validation")
    )
})

test_that("scoped styles", {
    reset_rsx_env()

    c_scoped_styles <- component(
        name = "test",
        template = function(ns) {
            shiny::div()
        },
        styles = function() {
            "* { color: red }"
        }
    )

    expect_identical(
        as.character(c_scoped_styles()),
        sprintf('<div class="%s"></div>', attr(c_scoped_styles, "component_id"))
    )

    expect_identical(
        gsub("\\s", "", aggregate_styles()),
        sprintf(".%s*{color:red;}", attr(c_scoped_styles, "component_id"))
    )
})

test_that("sass", {
    reset_rsx_env()
    c_wide <- component(
        name = "wide",
        template = function(ns) {
            shiny::tagList(
                shiny::div(),
                shiny::div(
                    shiny::p()
                )
            )
        },
        styles = function() {
            "div { color: red }"
        }
    )
    wide_tag <- c_wide()
    expect_identical(
        as.character(htmltools::renderTags(wide_tag)$html),
        sprintf(
            "<div class=\"%s\"></div>\n<div class=\"%s\">\n  <p></p>\n</div>",
            attr(c_wide, "component_id"),
            attr(c_wide, "component_id")
        )
    )

    expect_identical(
        gsub("\\s", "", aggregate_styles()),
        sprintf(".%sdiv{color:red;}", attr(c_wide, "component_id"))
    )
})

test_that("styles should be overwritten with same-named components", {
    suppressMessages({
        reset_rsx_env()
        c_overwrite1 <- component(
            name = "overwrite",
            styles = function() {
                "a {color: red;}"
            }
        )()
        styles <- aggregate_styles()
        expect_true(grepl("color: red", styles))

        # overwrite
        c_overwrite2 <- component(
            name = "overwrite",
            styles = function() {
                "a {color: blue;}"
            }
        )()
        styles <- aggregate_styles()
        expect_false(grepl("color: red", styles))
        expect_true(grepl("color: blue", styles))
    })
})

test_that("styles are compiled for each used component", {
    reset_rsx_env()
    c_compile1 <- component(
        name = "comp1",
        template = function(ns) {
            c_compile2()
        }
    )
    c_compile2 <- component(
        name = "comp2",
        styles = function() {
            "* { color: blue }"
        }
    )
    c_compile1()
    expect_true(grepl("color: blue", aggregate_styles()))
})
