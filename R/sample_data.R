#' @include jobs.R
NULL

#' Get sample data
#' 
#' In order to inspect data locally a very small spatial extent will be processed, downloaded and made available in R. 
#' 
#' @details 
#' In order to get a better understanding about the processing mechanisms and the data structures used in the openEO back-end, 
#' it helps to check the actual data from time to time. This function aids the user in doing to. It replaces all spatial 
#' extents of the derived process graph with a new spatial extent which is calculated by the first spatial extent of the 
#' mandatory openEO process 'load_collection'. We take the center of the extent and add 0.0003 degrees to it. In case the
#' coordinate reference system is not in WGS84, then the bounding box will be transformed into geodetic WGS84 beforehand, if
#' the package 'sf' is present.
#' 
#' If the spatial extent was explicitly set to a small custom extent, then you can disable the replacement of the area of 
#' interest with `replace_aoi = FALSE`.
#'
#' @param graph a ProcessGraph, a Process or the final node in a process for which the sample shall be calculated
#' @param replace_aoi a logical flag to indicate whether or not the original spatial extent shall be substituted with a different one, default TRUE
#' @param spatial_extent a bounding box or a spatial feature from which to derive a bounding box
#' @param execution `sync` or `async` which indicates the processing chain, a not "async" value results in a synchronous processing
#' @param immediate flag to be considered if the retrieval shall be immediately queued on the back-end
#' @param con connected and authenticated openEO client (optional) otherwise [active_connection()]
#' is used.
#' @param ... additional parameters that are passed to [compute_result()] or [create_job()]
#' 
#' @export
get_sample = function(graph, replace_aoi = TRUE, spatial_extent=NULL,execution="sync",immediate=TRUE,con=NULL, ...) {
  tryCatch({
    con = .assure_connection(con)
    dots = list(...)
    if (isTRUE(replace_aoi)) {
      # extract spatial_extent of load_collection, create a variable from it, 
      # get any location inside the AOI to create a very small subset
      
      if ("Process" != class(graph)[1]) {
        graph = as(graph,"Process")
      }
      
      # we need a copy of the whole process otherwise the original graph gets overwritten due to the environments in R6
      graph = parse_graph(graph$serialize())

      load_collections = .find_process_by_name(graph,"load_collection") 
      filter_spatials = .find_process_by_name(graph,"filter_spatial")
      filter_bboxes = .find_process_by_name(graph,"filter_bbox")
      
      spatial_subset_operators = c(load_collections,filter_spatials,filter_bboxes)
      
      if (length(spatial_subset_operators) == 0) stop("Cannot find 'load_collection', 'filter_spatial' or 'filter_bbox' in the process definition.")
      
      var = create_variable("spatial_extent")
      extents = lapply(spatial_subset_operators,function(x) {
        if (length(x) == 0) return(x)
        
        process = x$getId()
        
        switch(process,
               load_collection={
                 ext = x$parameters$spatial_extent
                 x$parameters$spatial_extent = var
               },
               filter_bbox={
                 ext = x$parameters$extent
                 x$parameters$extent = var
               },
               filter_spatial = { 
                 # not sure if this messes things up, maybe this needs to get to be replaced later, 
                 # e.g. write bbox into load_collection and delete process filter_spatial
                 ext = x$parameters$geometries
                 x$parameters$geometries = var
               }, 
               # default
               {
                 ext = NULL
               })
        
        if (is.null(ext) || ("Argument" %in% class(ext) && ext$isEmpty())) {
          ext = NULL
        }
        
        return(ext)
      })
      extents[sapply(extents,is.null)] = NULL
      
      graph$setVariables(c(graph$getVariables(),var))
      
      if (length(extents) == 0 && is.null(spatial_extent)) {
        message("Please state an area of interest. Either in parameter 'spatial_extent' or in the workflow. Returning parametrized workflow.")
        return(as(graph,"Process"))
      }
      
      # TODO also use filter_spatial or filter_polygon, if NULL in load_collection it should be there otherwise set load_collection
      
      # theoretically the bounding boxes should be the same for each entry, even if multiple collections are used they should cover the
      # same spatial extent, one objection would be reference areas of some sort...
      if (length(spatial_extent) == 0) {
        e = extents[[1]] # TODO take the first non null
        sample_extent = .create_sample_bbox(e)
      } else {
        # TODO transform the spatial_extent parameter into a bounding box
        args = list(BoundingBox$new(required=TRUE), GeoJson$new(required = TRUE))
        args = lapply(args,function(arg, e) {
          tryCatch({
            arg$setValue(e)
            msgs = arg$validate()
            if (length(msgs) > 0) {
              return(NULL)
            } else {
              return(arg)
            }
          }, error=function(e) {
            return(NULL)
          })
        }, e = spatial_extent)
        args[sapply(args,is.null)] = NULL
        
        if (length(args) == 0) stop("Cannot fill a Bounding Box argument with value from parameter 'spatial_extent'")
        
        sample_extent = args[[1]]$serialize()
      }
      
      var$setValue(sample_extent)
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
    } else if (!is.null(execution) && execution == "sync") {
      # compute_results
      arg_names = names(formals(compute_result))
      compute_config = dots[which(names(dots) %in% arg_names)]
      
      # TODO if empty add at least tmp file for output?
      
      if ("options" %in% names(dots)) { # currently this is the only additional parameter for save_result
        compute_config$options = dots$options 
      }
      res = do.call(compute_result, c(list(graph=graph,con=con),compute_config))
      
      return(res)
    } else {
      return(graph)
    }
    return(graph)
  })
  
}

# make it optional to pass the deltas in lon and lat
.create_sample_bbox = function(e, dlon = 0.0003, dlat = 0.0003) {
  # interprete e: bbox -> getValue -> list, geojson -> getValue -> sf, list -> getValue -> list
  center=NULL
  if ("bounding-box" %in% class(e)) {
    e = e$getValue()
  }
  
  if ("geojson" %in% class(e)) {
    e = e$getValue()
  }
  
  if (is.list(e) && !any(c("sf","sfc") %in% class(e)) && all(c("west","south","east","north") %in% names(e))) {
    v = e
    
    if ("crs" %in% names(v) && v[["crs"]] != 4326) {
      # check if sf is installed, if not, its not supported
      if (!.is_package_installed("sf")) {
        warning("Cannot automatically replace area of interest in a non EPSG:4326 without package 'sf' being installed. Please install the package or specify the AOI and run again with replace_aoi=FALSE")
        return(invisible(NULL))
      }
      
      center = c(lon=mean(c(v[["west"]],v[["east"]])),
                 lat=mean(c(v[["south"]],v[["north"]])))
      
      center = sf::st_transform(sf::st_sfc(sf::st_point(center),crs=sf::st_crs(v[["crs"]])), sf::st_crs(4326))
      center = as.numeric(center[[1]])
      names(center) = c("lon","lat")
    } else if (all(c("west","east","south","north") %in% names(v))) {
      center = c(lon=mean(c(v[["west"]],v[["east"]])),
                 lat=mean(c(v[["south"]],v[["north"]])))
    }
  } else if (any(c("sf","sfc") %in% class(e))) {
      if (!.is_package_installed("sf")) stop("Package 'sf' ist not installed to handle spatial vector data.")
      suppressWarnings({
        # take the first polygon object of the potential collection, get the center and create wgs84 coordinates  
        
        obj = sf::st_transform(e,4326)
        center = as.numeric(unlist(sf::st_centroid(obj[[1]])))
        # always treat coordinates in lon/lat like the default in sf
        names(center) = c("lon", "lat")
      })
      
  }
  
  if (is.null(center)) {
    stop("Cannot extract spatial extent.")
  }
  
  
  # returns always a WGS84 lon/lat bbox
  sample_extent = list(west = center[["lon"]]-dlon, 
                       east = center[["lon"]]+dlon, 
                       south=center[["lat"]]-dlat, 
                       north = center[["lat"]]+dlat)
  
  return(sample_extent)
}
