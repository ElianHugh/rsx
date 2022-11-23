test_that("instance namespaces", {
    reset_rsx_env()
    x <- component(
        name = "namespacing",
        template = function(ns) {
            ns("test")
        }
    )
    res <- as.character(x())
    expected_ns <- paste0(get_instances()[[1L]]$instance_id, "-test")
    expect_identical(res, expected_ns)
})

test_that("instance data setting", {
    reset_rsx_env()
    x <- component(
        data = function() {
            list(
                dat = 1L
            )
        }
    )
    x(
        data = list(
            dat = 2L
        )
    )
    expect_identical(get_instances()[[1L]]$data$dat, 2L)
})
