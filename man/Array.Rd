% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/argument_types.R
\name{Array}
\alias{Array}
\title{Array}
\value{
Object of \code{\link{R6Class}} which represents a single valued array.
}
\description{
Inheriting from \code{\link{Argument}} in order to represent an array of a single data type.
}
\section{Methods}{

\describe{
  \item{\code{$getMinItems}}{returns the minimum number of items}
  \item{\code{$getMaxItems}}{returns the maximum number of items}
  \item{\code{$setMinItems(value)}}{sets the minimum number of items}
  \item{\code{$setMaxItems(value)}}{sets the maximum number of items}
  \item{\code{$getItemSchema}}{returns the item schema of the items in the array}
  \item{\code{$setItemSchema(value)}}{sets the schema for the items in the array}
}
}

\section{Arguments}{

\describe{
  \item{\code{value}}{either a number describing the minimum and maximum number of elements in an array or the
  parsed JSON schema of a single item in the array}
}
}

\seealso{
\code{\link{Array}}, \code{\link{Integer}}, \code{\link{EPSGCode}}, \code{\link{String}}, \code{\link{Number}}, 
\code{\link{Date}}, \code{\link{RasterCube}}, \code{\link{VectorCube}}, \code{\link{ProcessGraphArgument}}, 
\code{\link{ProcessGraphParameter}}, \code{\link{OutputFormatOptions}}, \code{\link{GeoJson}},
\code{\link{Boolean}}, \code{\link{DateTime}}, \code{\link{Time}}, \code{\link{BoundingBox}}, \code{\link{Kernel}}, 
\code{\link{TemporalInterval}}, \code{\link{TemporalIntervals}}, \code{\link{CollectionId}}, \code{\link{OutputFormat}},
\code{\link{AnyOf}}, \code{\link{ProjDefinition}}, \code{\link{UdfCodeArgument}}, \code{\link{UdfRuntimeArgument}} and 
\code{\link{UdfRuntimeVersionArgument}}
}
