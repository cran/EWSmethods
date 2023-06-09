% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detrend_ts.R
\name{detrend_ts}
\alias{detrend_ts}
\title{Detrend Time Series}
\usage{
detrend_ts(data, method = "linear", bandwidth = NULL, span = 0.25, degree = 2)
}
\arguments{
\item{data}{The dataframe to be detrended. The first column must be a vector of dates with all other columns the individual time series.}

\item{method}{The method of detrending. Options include \code{"linear"} (residuals of a linear regression), \code{loess} (smoothing by local polynomial regression), \code{gaussian} (smoothing by a gaussian kernel), or \code{first.difference}.}

\item{bandwidth}{If \code{method = "gaussian"}, dictates the bandwidth of the gaussian kernel. If \code{NULL}, this is estimated from the data.}

\item{span}{If \code{method = "loess"}, controls the degree of smoothing as a proportion of points to be used (if \code{span = 1}, all points are used)}

\item{degree}{If \code{method = "loess"}, specifies the degree polynomials allowed. Options are normally \code{1} or \code{2}.}
}
\value{
Dataframe of deseasoned time series.
}
\description{
Removes directional signals from time series using loess, linear regression or gaussian detrending.
}
\examples{
#Generate five random monthly time series
#of 5 years length.

spp_data <- matrix(nrow = 5*12, ncol = 5)
spp_data <- sapply(1:dim(spp_data)[2], function(x){
spp_data[,x] <- rnorm(5*12,mean=20,sd=5)})
multi_spp_data <- cbind("time" =
 seq(as.Date('2000/01/01'), as.Date('2004/12/01'), by="month"),
   as.data.frame(spp_data))

detrend_dat <- detrend_ts(data = multi_spp_data,
method = "gaussian",
bandwidth = 2)

}
