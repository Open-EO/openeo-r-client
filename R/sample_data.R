#' @include jobs.R

#' @export
get_sample = function(graph, replace_aoi = TRUE,execution="sync",con=NULL) {
  tryCatch({
    con = openeo:::.assure_connection(con)
    
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
          dlat = (1-abs(center["lat"])/90) * dlon
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
      # queue job
      # download results
    } else {
      # compute_results
    }
    return(graph)
  })
  
}

