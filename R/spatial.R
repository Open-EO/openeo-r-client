.getSpatialExtents = function(graph, ...) {
  # sf needs to be installed, error else (later for bbox creation)
  
  # almost the same code is applied in get_sample, but no replacement is made and no "reserialization"
  
  con = .assure_connection(con)
  dots = list(...)
  # extract spatial_extent of load_collection, create a variable from it, 
  # get any location inside the AOI to create a very small subset
  
  if ("Process" != class(graph)[1]) {
    graph = as(graph,"Process")
  }
  
  # we need a copy of the whole process otherwise the original graph gets overwritten due to the environments in R6
  load_collections = .find_process_by_name(graph,"load_collection") 
  filter_spatials = .find_process_by_name(graph,"filter_spatial")
  filter_bboxes = .find_process_by_name(graph,"filter_bbox")
  
  spatial_subset_operators = c(load_collections,filter_spatials,filter_bboxes)
  
  if (length(spatial_subset_operators) == 0) stop("Cannot find 'load_collection', 'filter_spatial' or 'filter_bbox' in the process definition.")
  
  extents = lapply(spatial_subset_operators,function(x) {
    if (length(x) == 0) return(x)
    
    process = x$getId()
    
    switch(process,
           load_collection={
             ext = x$parameters$spatial_extent$getValue()
           },
           filter_bbox={
             ext = x$parameters$extent$getValue()
           },
           filter_spatial = { 
             # not sure if this messes things up, maybe this needs to get to be replaced later, 
             # e.g. write bbox into load_collection and delete process filter_spatial
             ext = x$parameters$geometries$getValue()
           }, 
           # default
           {
             ext = NULL
           })
    
    if (is.null(ext) || ext$isEmpty()) {
      ext = NULL
    }
    
    return(ext)
  })
  extents[sapply(extents,is.null)] = NULL
  
  extents = lapply(extents, function(e) as(e,"bbox"))
  
  return(extents)
  
}

#' @export
st_bbox.ProcessNode = function(obj, ...) {
  .getSpatialExtents(obj,...)
}


register_all_s3_methods = function() {
  register_s3_method("sf", "st_bbox", "ProcessNode")
}


# from r-spatial/stars @ github /R/tidyverse
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }
  
  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

# coercions ----

setOldClass(c("bounding-box","Argument","Parameter","R6"))

#' @export
`as.bbox.bounding-box` = function(from) {
  box = from$getValue()
  
  if (length(box) == 0) {
    return(NULL)
  }
  names = names(box)
  
  names[names=="west"] = "xmin"
  names[names=="east"] = "xmax"
  names[names=="south"] = "ymin"
  names[names=="north"] = "ymax"
  
  if ("crs" %in% names) {
    crs = box$crs
    box["crs"]=NULL
    names["crs"] = NULL
  } else {
    crs = NULL
  }
  
  box = as.numeric(box)
  names(box) = names
  
  stbbox = sf::st_bbox(box)
  if (length(crs) > 0 ) {
    sf::`st_crs<-`(stbbox) = crs
  }
  
  return(stbbox)
}

suppressWarnings({
  setAs(from="bounding-box",to="bbox",`as.bbox.bounding-box`)
})