test_that("numeric epsg code works", {
  tryCatch({
    code = openeo:::BoundingBox$new()
    code$setValue(list(west = 16.06, south = 48.06, east = 16.65, north = 48.35, crs = 3857))
    
    msg = code$validate()
    expect(length(msg)==0,failure_message = "It doesn't work")
  }, error = function(e) {
    expect(FALSE,failure_message=e$message)
  })
})

test_that("epsg code as string works", {
  tryCatch({
    code = openeo:::BoundingBox$new()
    code$setValue(list(west = 16.06, south = 48.06, east = 16.65, north = 48.35, crs = "EPSG:3857"))
    
    msg = code$validate()
    expect(length(msg)==0,failure_message = "It doesn't work") 
  }, error = function(e) {
    expect(FALSE,failure_message=e$message)
  })
})

