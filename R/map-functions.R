#' Creates a map with service
#' 
#' The function uses leaflet as a map component and having an OSM layer as background and stacks the
#' OGC services on top.
#' 
#' @param con a authenticated connection to an openeo back-end
#' @param service_id The service id which is going to be used
#' @param layers A character string containing the layers that shall be shown
#' @param transparent logical if the "free" area shall be transparent
#' 
#' @return a leaflet map
#' @export
mapService = function(con, service_id, layers,transparent=TRUE) {
  service = describeService(con,service_id)
  url = service$service_url
  
  map = addWMSTiles(addTiles(leaflet()),baseUrl = url, 
                layers = layers,
                options = WMSTileOptions(format = "image/png", 
                                         transparent = transparent,
                                         version="1.1.1")
                )
  
  return(map)
}

#' Creates a map of collection footprints
#' 
#' This function uses a back-end connection and one or more collection ids and prints their footprints
#' on a mapview map.
#' 
#' @param con A connected openeo back-end connection
#' @param collection character one string or a vector of strings with valid collection ids
#' @return a mapview object containing footprints and baselayers
#' 
#' @export
mapCollection = function(con, collection) {
  suppressWarnings({
    if (!require(mapview)) {
      message("Library 'mapview' was not installed.")
      return(invisible())
    }
  })
  
  
  collections = lapply(collection, function(coll_name,con) {
    col = describeCollection(con,coll_name)
    return(col)
  }, con=con)
  
  P4S.latlon <- sp::CRS("+proj=longlat +datum=WGS84")
  sps = lapply(collections, function(coll_desc,crs) {

    polygon = .bboxToSpatialPolygon(coll_desc$extent,coll_desc$crs)
    
    return(spTransform(polygon,crs))
  },crs=P4S.latlon)
  
  map = mapview()
  for (i in 1:length(collection)) {
    coll_name = collection[[i]]
    coll_sp_lat_lon = sps[[i]]
    map = addFeatures(map = map, data=coll_sp_lat_lon,label=coll_name)
  }
  # TODO add automatic centering and zoom level to map
  return(map)
}

.bboxToSpatialPolygon = function(bbox,crs) {
  
  coordinates= rbind(
    c(bbox["x","min"],bbox["y","min"]),
    c(bbox["x","max"],bbox["y","min"]),
    c(bbox["x","max"],bbox["y","max"]),
    c(bbox["x","min"],bbox["y","max"]),
    c(bbox["x","min"],bbox["y","min"])
  )
  
  colnames(coordinates) = c("x","y")
  
  p = Polygon(coordinates)
  pp = Polygons(list(p),ID=1)
  sp = SpatialPolygons(list(pp),proj4string = crs)
  
  return(sp)
}