% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user.R
\name{upload_file}
\alias{upload_file}
\title{Upload data into the users workspace}
\usage{
upload_file(
  content,
  target,
  encode = "raw",
  mime = "application/octet-stream",
  con = NULL
)
}
\arguments{
\item{content}{the file path of the file to be uploaded}

\item{target}{the relative server path location for the file, e.g. where to find the file in the users workspace}

\item{encode}{the encoding type used to upload the data, e.g. 'multipart','form','json','raw' ('raw' by default)}

\item{mime}{mime type used in upload_file ('application/octet-stream' as a default)}

\item{con}{authorized Connection (optional) otherwise \code{\link[=active_connection]{active_connection()}}
is used.}
}
\value{
the relative file path on the server
}
\description{
This function sends the file retrieved by the 'content' parameter to the specified target location (relative file path in the
user workspace) on the back-end.
}
