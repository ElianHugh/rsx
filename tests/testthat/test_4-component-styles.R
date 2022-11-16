test_that("styles validation", {
    reset_rsx_env()
    expect_error(
        component(
            styles = "a"
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
        styles = list(
            scoped = ""
        )
    )

    expect_no_error(print(x()))

    reset_rsx_env()

    x <- component(
        name = "test",
        template = function(ns) {
            shiny::div()
        },
        styles = list(
            scoped = "* { color: red }"
        )
    )

    expect_identical(
        get_tag_output(x()),
        get_tag_output(shiny::div("data-rsx-test" = ""))
    )
})

# test_that("styles should be overwritten with same-named components", {
#     suppressMessages({
#         reset_rsx_env()
#         x <- component(
#             name = "foo",
#             styles = list(
#                 "scoped" = "a {color: red;}"
#             )
#         )
#         # overwrite
#         x <- component(
#             name = "foo",
#             styles = list(
#                 "scoped" = "a {color: blue;}"
#             )
#         )
#         styles <- aggregate_styles()
#         expect_false(grepl("color:red", styles))
#         expect_true(grepl("color:blue", styles))
#     })
# })
