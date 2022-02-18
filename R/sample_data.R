#' @include jobs.R
NULL

#' Get sample data
#' 
#' In order to inspect data locally a very small spatial extent will be processed, downloaded and made available in R. 
#' 
#' @details 
#' In order to get a better understanding about the processing mechanisms and the data structures used in the openEO backend, it helps
#' to check the actual data from time to time. This function aids the user in doing to. It replaces all spatial extents of the derived 
#' process graph with a new spatial extent which is calculated by the first spatial extent of the mandatory openEO process 'load_collection'.
#' We take the center of the extent and add 0.0003 degrees to it. Currently just spatial bounding boxes with the default CRS EPSG:4326 are
#' replaced.
#'
#' @param graph a ProcessGraph, a Process or the final node in a process for which the sample shall be calculated
#' @param replace_aoi a logical flag to indicate whether or not the original spatial extent shall be substituted with a different one, default TRUE
#' @param execution \code{sync} or \code{async} which indicates the processing chain, a not "async" value results in a synchronous processing
#' @param immediate flag to be considered if the retrieval shall be immediately queued on the back-end
#' @param con connected and authenticated openEO client (optional) otherwise \code{\link{active_connection}}
#' is used.
#' @param ... additional parameters that are passed to \code{\link{compute_result}} or \code{\link{create_job}}
#' 
#' @export
get_sample = function(graph, replace_aoi = TRUE,execution="sync",immediate=TRUE,con=NULL, ...) {
  tryCatch({
    con = .assure_connection(con)
    dots = list(...)
    
    if (isTRUE(replace_aoi)) {
      # extract spatial_extent of load_collection, create a variable from it, 
      # get any location inside the AOI to create a very small subset
      
      # TODO check if graph is save_results otherwise create one to instruct storing
      
      if ("Process" != class(graph)[1]) {
        graph = as(graph,"Process")
      }
      
      # we need a copy of the whole process otherwise the original graph gets overwritten due to the environments in R6
      graph = parse_graph(graph$serialize())
      
      
      ns=graph$getNodes()
      subset = which(sapply(ns, function(x) {
        id = x$getId()
        !is.null(id) && id == "load_collection"
      }))
      
      load_collections = ns[subset]
      var = create_variable("extent")
      extents = lapply(load_collections,function(x) {
        ext = x$parameters$spatial_extent$getValue()
        x$parameters$spatial_extent = var
        return(ext)
      })
      
      # theoretically the boundingboxes should be the same for each entry, even if multiple collections are used they should cover the
      # same spatial extent, one objection would be reference areas of some sort...
      e = extents[[1]]
      if ("bounding-box" %in% class(e)) {
        v = e$getValue()
        
        if (!"crs" %in% names(v) || v[["crs"]] == 4326) {
          center = c(lon=mean(v[["west"]],v[["east"]]),
                     lat=mean(v[["south"]],v[["north"]]))
          dlon = 0.0003 #between 30 and 40m
          dlat = 0.0003
          
          sample_extent = list(west = center["lon"]-dlon/2, 
                               east = center["lon"]+dlon/2, 
                               south=center["lat"]-dlat/2, 
                               north = center["lat"]+dlat/2)
          var$setValue(sample_extent)
        } else {
          #TODO not sure what to do exactly without depending on proper geometry packages like sf
        }
      }
    }
    
    if (!is.null(execution) && execution == "async") {
      # create job
      arg_names = names(formals(create_job))
      arg_names = arg_names[-which("..." == names(arg_names))]
      job_meta = dots[which(arg_names %in% names(dots))]
      job = do.call(create_job, c(list(graph=graph),job_meta))
      
      # queue job
      if (isTRUE(immediate)) {
        job = start_job(job,con=con)
      }
      return(job)
      # download results on your own
    } else {
      # compute_results
      arg_names = names(formals(compute_result))
      compute_config = dots[which(names(dots) %in% arg_names)]
      res = do.call(compute_result, c(list(graph=graph,con=con),compute_config))
      
      return(res)
    }
    return(graph)
  })
  
}

