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
    x <- component(
        styles = list(
            scoped = ""
        )
    )

    expect_no_error(print(x()))

    x <- component(
        styles = list(
            scoped = "* { color: red }"
        )
    )

    expect_no_error(print(x()))
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
