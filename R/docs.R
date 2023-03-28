#' Component Data
#'
#' @description
#'
#' Data is used for a component's internal state and
#' can also be used to pass information from a parent component to its children.
#' Both component templates and methods have access to component data.
#'
#' @details
#'
#' ## Creating data
#'
#' For instance, the following is a valid data function:
#'
#' ```r
#' function() {
#'  list(
#'    rctv = shiny::reactiveVal(),
#'    df   = mtcars
#'  )
#' }
#' ```
#'
#' ## Accessing data
#'
#' Data can be accessed in both the methods and template parts of the component via the `self` keyword.
#' For example, the following component defines the data `foo`, and accesses it in the template via `self$foo`.
#'
#' ```{r}
#' x <- component(
#'     data = function() {
#'         list(
#'             foo = 1L
#'         )
#'     },
#'     template = function(ns) {
#'         self$foo
#'     }
#' )
#'
#' print(x())
#' ```
#'
#' ## Passing data
#'
#' ```{r}
#' x <- component(
#'     data = function() {
#'         list(
#'             foo = 1L
#'         )
#'     },
#'     template = function(ns) {
#'         self$foo
#'     }
#' )
#'
#' print(x(data = list(foo = 2L)))
#' ```
#'
#' @name component-data
#' @family components
#' @rdname component-data
NULL

#' Component Methods
#'
#' @description
#'
#' Methods are a list of functions that are contained within a given component,
#' and typically will do work with component data.
#'
#' Methods can be accessed in both other methods and the component template
#' via the `self` keyword.
#'
#' @details
#'
#' The following is an excerpt from a simple counter component:
#'
#' ```r
#' methods = list(
#'     setup = function(input, output, session) {
#'         output$txt <- renderText({
#'             paste0("Count is: ", self$count())
#'         })
#'         observeEvent(input$button, {
#'             self$count(self$count() + 1L)
#'         })
#'     },
#'     increment = function() {
#'         self$count(self$count() + 1L)
#'     }
#' )
#' ```
#'
#' ## Hooks
#'
#' ### Setup
#'
#' The `setup` hook is defined by passing a function named "setup" to the
#' methods list. Setup is used as the module server for the component.
#'
#' ```r
#' setup = function(input, output, session) {
#'
#' }
#' ````
#'
#' ### Render
#'
#' The `render` hook is defined by passing a function named "render" to
#' the methods list. The render hook allows for the modification of template code
#' prior to its rendering.
#'
#' ```r
#' render = function(element) {
#'
#' }
#' ````
#'
#' @name component-methods
#' @family components
#' @rdname component-methods
NULL

#' Component Template
#'
#' @description
#'
#' The template refers to the UI-generator of a given component.
#' This is analagous to the UI function of a given {shiny} module.
#'
#'
#' @details
#'
#' The component template function must be of the following structure (note
#' the ns argument):
#'
#' ```r
#' function(ns) {
#'  # must either return an object that can
#'  # be coerced to a `shiny::tags` object or a `shiny::tagList`
#' }
#' ```
#'
#' The following is an example of a valid template:
#'
#' ```r
#' function(ns) {
#'  shiny::div("Hello world!")
#' }
#' ```
#'
#' # Namespacing
#'
#' The template function requires one argument: `ns`.
#' `ns` is used identically to shiny modules, and
#' helps distinguish between component instances.
#'
#' # Slots
#'
#' Component templates can be nested through the use of slots.
#' Slots are placeholder elements that tell {rsx} where tags should be placed.
#'
#' ```{r}
#' x <- component(
#'     template = function(ns) {
#'         shiny::tagList(
#'             shiny::tags$slot(),
#'             shiny::p("bar")
#'         )
#'     }
#' )
#'
#' print(x(shiny::p("foo")))
#' ````
#'
#' You can also specify if you'd like content to be used in the case that a
#' slot isn't filled -- this is typically called "fallback" content.
#'
#' ```{r}
#' x <- component(
#'     template = function(ns) {
#'         shiny::tags$slot(
#'             shiny::p("Hello!")
#'         )
#'     }
#' )
#'
#' print(x())
#' ````
#'
#' ## Named Slots
#'
#' Slots can be further distinguished by name attributes, which
#' can be used to target specific areas of the template.
#'
#' ```{r}
#' x <- component(
#'     template = function(ns) {
#'         shiny::tagList(
#'             shiny::tags$slot(name = "a"),
#'             shiny::tags$slot(name = "b")
#'         )
#'     }
#' )
#' print(x(shiny::p("bar", slot = "b"), shiny::p("foo", slot = "a")))
#' ```
#'
#' @name component-template
#' @family components
#' @rdname component-template
NULL

#' Component Styles
#'
#' @description
#'
#' The styles argument is a character vector of length 1
#'
#' @details
#'
#' ```r
#' "a { color: red; }"
#' ````
#'
#' @name component-styles
#' @family components
#' @rdname component-styles
NULL
