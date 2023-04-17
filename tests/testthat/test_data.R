test_that("data validation", {
    reset_rsx_env()
    # must be function
    expect_error(
        component(data = "bad"),
        class = new_rsx_error("component_validation")
    )
    # no args
    expect_error(
        component(
            data = function(bad) {}
        ),
        class = new_rsx_error("component_validation")
    )
    # must return list
    expect_error(
        component(
            data = function() {
                "bad"
            }
        )(),
        class = new_rsx_error("instance_validation")
    )
    # must return *named* list
    expect_error(
        component(
            data = function() {
                list("bad")
            }
        )(),
        class = new_rsx_error("instance_validation")
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

test_that("can't pass invalid data to instances", {
    reset_rsx_env()
    c_bad_data <- component(
        data = function() {
            list(
                dat = 1L
            )
        }
    )
    expect_error(
        c_bad_data(
            data = list(bad = "bad")
        ),
        class = new_rsx_error("instance_data")
    )
})