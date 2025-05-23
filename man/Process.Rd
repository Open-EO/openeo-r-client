% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_graph_building.R
\name{Process}
\alias{Process}
\title{Process object}
\value{
Object of \code{\link[R6:R6Class]{R6::R6Class()}} with methods for storing meta data of back-end processes and user assigned data
}
\description{
This object reflects a process offered by an openEO service in order to load and manipulate data collections. It will be created
with the information of a received JSON object for a single process, after the arguments of the process have been translated
into \code{\link[=Argument]{Argument()}} objects.
}
\section{Fields}{

\describe{
\item{\code{parameters}}{\itemize{
\item a named list of Argument objects
}}

\item{\code{isUserDefined}}{logical - depending if the process is offered by the openEO service or if it was user defined}
}}

\section{Methods}{

\describe{
\item{$new(id,parameters,description=character(), summary = character(), parameter_order=character(),returns)}{}
\item{$getId()}{returns the id of a process which was defined on the back-end}
\item{$getParameters()}{returns a named list of arguments}
\item{$getReturns()}{returns the schema for the return type as list}
\item{$getFormals()}{returns the function formals for this process - usually a named vector of the specified default values, but NA where no default value was specified}
\item{$setId(id)}{sets the id of a process}
\item{$setSummary(summary)}{sets the summary text}
\item{$setDescription(description)}{sets the description text}
\item{$getParameter(name)}{returns the Argument object with the provided name}
\item{$getProcessGraph()}{returns the ProcessGraph to which this Process belongs}
\item{$setProcessGraph(process_graph)}{sets the ProcessGraph to which this Process belongs}
\item{$validate()}{validates the processes argument values}
\item{$serialize()}{serializes the process - mainly used as primary serialization for a \code{\link[=ProcessNode]{ProcessNode()}}}
\item{$getCharacteristics()}{select all non functions of the private area, to be used when copying process
information into a process node}
}
}

\section{Arguments}{

\describe{
\item{id}{process id from the back-end}
\item{parameters}{a list of Argument objects}
\item{description}{the process description}
\item{summary}{the summary of a process}
\item{returns}{the returns part of the process definition or an already evaluated parameter}
\item{name}{a parameter name}
\item{value}{the value for a parameter or the description text}
}
}

