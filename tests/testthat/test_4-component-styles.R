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
        styles = "* { color: red }"
    )

    expect_identical(
        get_tag_output(as_shiny_tag(x())),
        sprintf('<div class="%s"></div>', attr(x, "component_id"))
    )

    expect_identical(
        gsub("\\s", "", aggregate_styles()),
        sprintf(".%s*{color:red;}", attr(x, "component_id"))
    )

    reset_rsx_env()

    wide <- component(
        name = "wide",
        template = function(ns) {
            shiny::tagList(
                shiny::div(),
                shiny::div(
                    shiny::p()
                )
            )
        },
        styles = "
            div { color: red }
        "
    )
    wide_tag <- wide()
    expect_identical(
        as.character(htmltools::renderTags(wide_tag)$html),
        sprintf(
            "<div class=\"%s\"></div>\n<div class=\"%s\">\n  <p></p>\n</div>",
            attr(wide, "component_id"),
            attr(wide, "component_id")
        )
    )

    expect_identical(
        gsub("\\s", "", aggregate_styles()),
        sprintf(".%sdiv{color:red;}", attr(wide, "component_id"))
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

test_that("styles are compiled for each used component", {
    reset_rsx_env()
    comp1 <- component(
        name = "comp1",
        template = function(ns) {
            comp2()
        }
    )
    comp2 <- component(
        name = "comp2",
        styles = "* { color: blue }"
    )
    comp1()
    expect_true(grepl("color: blue", aggregate_styles()))
})
