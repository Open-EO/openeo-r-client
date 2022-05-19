test_that("geojson as list is valid", {
  geometries = list("type" = "Polygon", 
                    "coordinates" = list(list(list(11.43151370647882, 46.8947771204201), 
                                              list(11.42408903273992, 46.8947771204201),
                                              list(11.42408903273992, 46.900314165242335), 
                                              list(11.43151370647882, 46.900314165242335), 
                                              list(11.43151370647882, 46.8947771204201))))
  
  
  arg = openeo:::GeoJson$new(name = "polygon",required = TRUE)
  process = openeo:::Process$new(id = "test_process",parameters = list(arg))
  node = openeo:::ProcessNode$new(node_id = "some_node",process = process)
  arg$setProcess(node)
  
  arg$setValue(geometries)
  msg = arg$validate()
  testthat::expect(length(msg) == 0, failure_message = "not valid")
  sers = arg$serialize()
  testthat::expect(is.list(sers) && all(c("type","features") %in% names(sers)), failure_message = "geojson serialization is not a list with 'type' and 'features' fields")
  
  testthat::expect("sf" %in% class(arg$getValue()), failure_message = "geojson value is not a sf object")
})


test_that("geojson as sf is valid", {
  point = sf::st_point(c(11.1167, 46.0665))
  point = sf::st_as_sf(sf::st_sfc(point,crs=4326))
  
  
  
  arg = openeo:::GeoJson$new(name = "polygon",required = TRUE)
  process = openeo:::Process$new(id = "test_process",parameters = list(arg))
  node = openeo:::ProcessNode$new(node_id = "some_node",process = process)
  arg$setProcess(node)
  
  arg$setValue(point)
  msg = arg$validate()
  testthat::expect(length(msg) == 0, failure_message = paste0("not valid: ",msg))
  sers = arg$serialize()
  testthat::expect(is.list(sers) && all(c("type","features") %in% names(sers)), failure_message = "geojson serialization is not a list with 'type' and 'features' fields")
  
  testthat::expect("sf" %in% class(arg$getValue()), failure_message = "geojson value is not a sf object")
  
  testthat::expect(sf::st_as_text(sf::st_as_sfc(arg$getValue())) == "POINT (11.1167 46.0665)",failure_message = "Coordinates have been manipulated")
})


test_that("geojson as point feature list", {
  point = list(type="FeatureCollection",
               name="xy",
               features= list(
                 list(
                   type="Feature",
                   properties=list(),
                   geometry=list(
                     type="Point",
                     coordinates=c(11.1167,46.0665)
                   )
                 )
               ))
  
  
  
  arg = openeo:::GeoJson$new(name = "polygon",required = TRUE)
  process = openeo:::Process$new(id = "test_process",parameters = list(arg))
  node = openeo:::ProcessNode$new(node_id = "some_node",process = process)
  arg$setProcess(node)
  
  arg$setValue(point)
  msg = arg$validate()
  testthat::expect(length(msg) == 0, failure_message = paste0("not valid: ",msg))
  sers = arg$serialize()
  testthat::expect(is.list(sers) && all(c("type","features") %in% names(sers)), failure_message = "geojson serialization is not a list with 'type' and 'features' fields")
  
  testthat::expect("sf" %in% class(arg$getValue()), failure_message = "geojson value is not a sf object")
  
  testthat::expect(sf::st_as_text(sf::st_as_sfc(arg$getValue())) == "POINT (11.1167 46.0665)",failure_message = "Coordinates have been manipulated")
})