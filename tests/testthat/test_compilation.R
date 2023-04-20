test_that("rsx_app works as expected", {
    reset_rsx_env()
    expect_error(rsx_app("bad"))
})

test_that("rsx_ui adds styles to head", {
    reset_rsx_env()
    c_ui_styles <- component(
        styles = function() {
            "* { color: red }"
        }
    )()
    res <- rsx_ui(c_ui_styles, resource_path = NULL)
    styles <- as.character(res[[1L]]$children[[1L]])
    expect_true(grepl("color: red", styles))
})

test_that("rsx_ui namespaces correctly", {
    reset_rsx_env()
    c_ui_ns <- component(
        template = function(ns) {
            shiny::div(ns("test"))
        }
    )
    res <- rsx_ui(
        c_ui_ns(),
        id = "test"
    )[[2L]]$children

    # top level is a div of class 'App'
    expect_true(res[[1L]]$attribs$class == "App")
    expect_true(res[[1L]]$name == "div")

    # namespace is correct
    regx <- "(test-instance-)(.*)(-test)"
    expect_true(
        grepl(
            as.character(regx),
            as.character(res[[1L]]$children[[1L]])
        )
    )
})