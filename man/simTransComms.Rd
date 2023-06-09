% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{simTransComms}
\alias{simTransComms}
\title{Three Simulated Transitioning Communities.}
\format{
A list of three dataframes with 301 rows and 7 variables each:
\describe{
  \item{community_id}{the identity of the simulated community}
  \item{time}{time index}
  \item{spp_1}{density of species 1}
  \item{spp_2}{density of species 1}
  \item{spp_3}{density of species 1}
  \item{spp_4}{density of species 1}
  \item{spp_5}{density of species 1}
  \item{inflection_pt}{the time index where transition occurs}
}
}
\usage{
simTransComms
}
\description{
A dataset containing three simulated five species communities stressed through a critical transition.
}
\examples{
data("simTransComms", package = "EWSmethods")

community_data <- simTransComms$community1
}
\keyword{datasets}
