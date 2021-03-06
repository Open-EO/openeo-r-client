% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_defined_processes.R
\name{UserProcessCollection}
\alias{UserProcessCollection}
\title{User Defined Process Collection}
\description{
This object contains template functions for process graph building from the user defined processes at an openEO service. 
This object is an R6 object that is not locked, in order to add new functions at runtime, in the same fashion as 
\code{\link{ProcessCollection}} for predefined processes. The object is usually created at 
\code{\link{user_processes}} and resembles a given status, meaning, when a new user process is created or another one
is updated the \code{UserProcessCollection} needs to be recreated with "fresh" data.
}
\section{Methods}{

\describe{
   \item{\code{$new(con = NULL)}}{The object creator created an openEO connection.} 
}
}

\section{Arguments}{

\describe{
   \item{con}{optional an active and authenticated Connection (optional) otherwise \code{\link{active_connection}}
is used.}
}
}

