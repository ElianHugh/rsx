test_that("components list their subcomponents", {
    c_printing <- component(
        name = "component_printing",
        template = function(ns) {

        },
        data = function() {

        },
        methods = list(),
        styles = function() {
            ""
        }
    )
    fmt <- format(c_printing)
    expect_true(any(grepl("`component_printing`", fmt)))
    expect_true(any(grepl("data: function", fmt)))
    expect_true(any(grepl("template: function", fmt)))
    expect_true(any(grepl("methods: list", fmt)))
    expect_true(any(grepl("styles: css", fmt)))
    expect_true(any(grepl("Namespace:", fmt)))
})

test_that("instances print appropriately", {
    c_inst_printing <- component(
        name = "instance_printing",
        template = function(ns) {
            shiny::div()
        }
    )
    fmt <- format(c_inst_printing())
    expect_true(any(grepl("`instance_printing`", fmt)))
    expect_true(any(grepl("  <div></div>", fmt)))
})
