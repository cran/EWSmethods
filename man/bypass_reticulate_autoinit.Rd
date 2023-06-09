% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bypass_autoinit.R
\name{bypass_reticulate_autoinit}
\alias{bypass_reticulate_autoinit}
\title{Prevent Reticulate Autoconfiguring Python environments}
\usage{
bypass_reticulate_autoinit()
}
\value{
No return value, called for side effects.
}
\description{
Adds \code{RETICULATE_AUTOCONFIGURE = "FALSE"} to \code{.Renviron} to prevent \code{reticulate} automatically loading Python environments.
}
