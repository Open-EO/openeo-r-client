test_that("numeric epsg code works", {
  tryCatch({
    code = openeo:::EPSGCode$new()
    code$setValue(4326)
  
    code$validate()
    expect(TRUE,failure_message = "It doesn't work") 
  }, error = function(e) {
    expect(FALSE,failure_message=e$message)
  })
})

test_that("epsg code as string works", {
  tryCatch({
    code = openeo:::EPSGCode$new()
    code$setValue("EPSG:4326")
    
    code$validate()
    expect(TRUE,failure_message = "It doesn't work") 
  }, error = function(e) {
    expect(FALSE,failure_message=e$message)
  })
})

test_that("epsg code as string in lower letter works", {
  tryCatch({
    code = openeo:::EPSGCode$new()
    code$setValue("epsg:4326")
    
    code$validate()
    expect(TRUE,failure_message = "It doesn't work") 
  }, error = function(e) {
    expect(FALSE,failure_message=e$message)
  })
})

test_that("other text for code does not work", {
  tryCatch({
    code = openeo:::EPSGCode$new()
    code$setValue("foo.bar")
    
    code$validate()
    expect(FALSE,failure_message = "It doesn't work") 
  }, error = function(e) {
    expect(TRUE,failure_message=e$message)
  })
})
