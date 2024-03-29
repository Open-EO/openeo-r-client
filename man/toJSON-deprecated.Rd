% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{graphToJSON-deprecated}
\alias{graphToJSON-deprecated}
\alias{graphToJSON}
\alias{processToJSON-deprecated}
\alias{processToJSON}
\title{*toJSON functions}
\usage{
graphToJSON(x, ...)

processToJSON(x, ...)
}
\arguments{
\item{x}{Graph or Process object}

\item{...}{arguments for jsonlite::toJSON}
}
\description{
Those functions serialized a Graph or Process object into JSON text. They are deprecated. Use \code{toJSON} instead.
}
