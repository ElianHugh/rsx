test_that("data validation", {
    reset_rsx_env()
    # must be function
    expect_error(component(data = "bad"))
    # no args
    expect_error(
        component(
            data = function(bad) {

            }
        )
    )
    # must return list
    expect_error(
        component(
            data = function() {
                "bad"
            }
        )()
    )
    # must return *named* list
    expect_error(
        component(
            data = function() {
                list("bad")
            }
        )()
    )
})

test_that("can pass data to instances", {
    reset_rsx_env()
    c_pass_data <- component(
        data = function() {
            list(
                dat = 1L
            )
        }
    )
    c_pass_data(
        data = list(
            dat = 2L
        )
    )
    expect_identical(get_instances()[[1L]]$data$dat, 2L)
})
