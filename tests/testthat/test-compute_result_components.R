test_that("find 'save_result' works with .find_process_by_id", {
  
  active_connection(con=OpenEOClient$new())
  openeo:::demo_processes()
  p = processes()
  
  ndvi = function(x,...) {
    b04 = x[1]
    b08 = x[2]
    
    (b08-b04)/(b08+b04)
  }
  
  
  collection = "SENTINEL2_L2A_SENTINELHUB"
  spatial_extent = sf::st_bbox(c(xmin=11.52191,xmax=11.53504, ymin=46.4369,ymax=46.4459))
  
  time_range = c("2022-07-01",
                 "2022-07-31")
  bands = c("B04", "B08")
  
  data = p$load_collection(id = collection,
                           spatial_extent = spatial_extent,
                           temporal_extent = time_range,
                           bands = bands)
  
  reduce = p$reduce_dimension(data=data, reducer = ndvi,dimension="bands")
  
  reduce2 = p$reduce_dimension(data=reduce,reducer = function(x,context) {p$min(x)},dimension = "t")
  
  
  result = p$save_result(data = reduce2, format="GTiff")
  
  expect_silent({
    o = openeo:::.find_process_by_name(graph=as(result,"Process"),process_id = "save_result")
  })
  
  expect(length(o) == 1,failure_message = "Less or more than one node found as process")
  
  expect_silent({
    o = openeo:::.find_process_by_name(graph=as(result,"Graph"),process_id = "save_result")
  })
  expect(length(o) == 1,failure_message = "Less or more than one node found as node")
  
})
