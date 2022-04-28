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
})
