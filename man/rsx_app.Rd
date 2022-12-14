% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compilation.R
\name{rsx_app}
\alias{rsx_app}
\title{Create an rsx app object}
\usage{
rsx_app(root, ..., resource_path = NULL, app_class = "App")
}
\arguments{
\item{root}{an \code{rsx::component} object}

\item{...}{further arguments passed to \code{shiny::shinyApp}}

\item{resource_path}{path to a resource folder, if \code{NULL} styles will be inlined}

\item{app_class}{the html class attribute for the app wrapper}
}
\description{
Create a new instance of an rsx application by passing a top level
rsx::component as the application root. This is analagous to running \code{shiny::shinyApp()}.
}
\seealso{
Other compilation: 
\code{\link{rsx_server}()},
\code{\link{rsx_ui}()}
}
\concept{compilation}
