#' @importFrom sf st_bbox
NULL

.getSpatialExtents = function(graph, ...) {
  # sf needs to be installed, error else (later for bbox creation)
  
  # almost the same code is applied in get_sample, but no replacement is made and no "reserialization"
  con = active_connection()
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
             ext = x$parameters[["spatial_extent"]]
             if ("anyOf" %in% class(ext)) ext = ext$getValue()
           },
           filter_bbox={
             ext = x$parameters[["extent"]]$getValue()
             if ("anyOf" %in% class(ext)) ext = ext$getValue()
           },
           filter_spatial = { 
             # not sure if this messes things up, maybe this needs to get to be replaced later, 
             # e.g. write bbox into load_collection and delete process filter_spatial
             ext = x$parameters[["geometries"]]$getValue()
             if ("anyOf" %in% class(ext)) ext = ext$getValue()
           }, 
           # default
           {
             ext = NULL
           })
    
    if (is.null(ext) || !any(c("geojson","bounding-box") %in% class(ext)) || ("Argument" %in% class(ext) && ext$isEmpty())) {
      ext = NULL
    }
    
    return(ext)
  })
  extents[sapply(extents,is.null)] = NULL
  
  extents = lapply(extents, function(e) as(e,"bbox"))
  
  return(extents)
  
}

# set the class definition for sf (it was not exported)
setOldClass("bbox")

#' st_bbox for ProcessNode
#' 
#' Traverses the graph from end node to roots and searches for defined bounding boxes in load_collection, filter_spatial, filter_bbox.
#' 
#' @param obj the process node
#' @param ... not used
#' 
#' @return sf bbox object if one element was found, else a list of all bounding boxes (usually returned in EPSG:4326)
#' 
#' @export
st_bbox.ProcessNode = function(obj, ...) {
  .getSpatialExtents(obj,...)
}


register_all_spatial_s3_methods = function() {
  register_s3_method("sf", "st_bbox", "ProcessNode")
}


# coercions ----

setOldClass(c("bounding-box","Argument","Parameter","R6"))


#' coerce to bbox
#' 
#' A coercion function for extracting a 'bbox' object that can usualy be obtained by [sf::st_bbox()]. This coercion
#' function was created to easily extract the boúnding box from the openeos argument objects [BoundingBox()] and [GeoJson()].
#'
#' @param from a [BoundingBox()] argument object or a [GeoJson()] argument object
#' 
#' @return a bbox object from [sf::st_bbox()]
#' 
#' @name as.bbox
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

setOldClass(c("geojson","Argument","Parameter","R6"))

#' @name as.bbox
#' @export
`as.bbox.geojson` = function(from) {
  if (is.null(from) || !"Argument" %in% class(from) || from$isEmpty()) {
    return(NA)
  }
  
  return(sf::st_bbox(from$getValue()))
}

suppressWarnings({
  setAs(from="bounding-box",to="bbox",`as.bbox.bounding-box`)
  setAs(from="geojson",to="bbox",`as.bbox.geojson`)
})
