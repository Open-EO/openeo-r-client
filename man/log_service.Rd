% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/services.R
\name{log_service}
\alias{log_service}
\title{Service log}
\usage{
log_service(service, offset = NULL, limit = NULL, con = NULL)
}
\arguments{
\item{service}{the service or the service_id}

\item{offset}{the id of the log entry to start from}

\item{limit}{the limit of lines to be shown}

\item{con}{an optional connection if you want to address a specific service}
}
\value{
a \code{Log} object
}
\description{
Opens the log of secondary service.
}
