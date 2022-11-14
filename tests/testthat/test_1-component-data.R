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

