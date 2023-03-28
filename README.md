# rsx

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/ElianHugh/rsx/workflows/R-CMD-check/badge.svg)](https://github.com/ElianHugh/rsx/actions)
[![Codecov test coverage](https://codecov.io/gh/ElianHugh/rsx/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ElianHugh/rsx?branch=main)
<!-- badges: end -->

## -- Work In Progress --

Create complex yet manageable Shiny applications with nestable components that encapsulate both UI and state. Heavily inspired by many JS frameworks, particularly vue.

<b>Features</b>:

* Nestable and modular templating system
* Scoped CSS to isolate styles between components

## Why rsx?

When writing Shiny apps, I often found that I was getting confused when juggling modules. Specifically, I found having to remember the relationship between module server and module UI frustrating -- if the the UI for `module A` *always* links to the server for `module A`, why do I have to keep track of two objects? This package seeks to resolve this cognitive overhead by ensuring that module servers and UI are *always* coupled under one object.

## Installation

### Release build

``` r
install.packages('rsx', repos = 'https://elianhugh.r-universe.dev')
```

### Development build

``` r
# install.packages("devtools")
devtools::install_github("ElianHugh/rsx")
```
