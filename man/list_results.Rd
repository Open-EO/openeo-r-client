% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jobs.R
\name{list_results}
\alias{list_results}
\title{Creates a list of download paths}
\usage{
list_results(job, con = NULL)
}
\arguments{
\item{job}{the job object or the id of the job}

\item{con}{connected and authenticated openEO client object (optional) otherwise \code{\link[=active_connection]{active_connection()}}
is used.}
}
\value{
result object containing of URLs for download
}
\description{
The function queries the back-end to receive the URLs to the downloadable files of a particular job.
}
