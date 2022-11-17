test_that("check array validation with single item types", {

  arr2 = openeo:::Array$new(name = "array_typed",items = list(type="string"))
  arr2$setValue(c("2016", "2017", "2018", "2019", "2020", "2021"))
  validation = arr2$validate()
  testthat::expect(length(validation)==0,failure_message = paste0("Validation failed with reason: ",validation)) 
  
})

test_that("check array validation with single item types and list parameter", {
  
  arr2 = openeo:::Array$new(name = "array_typed",items = list(type="number"))
  arr2$setValue(list(2016, 2017, 2018, 2019, 2020, 2021))
  validation = arr2$validate()
  testthat::expect(length(validation)==0,failure_message = paste0("Validation failed with reason: ",validation)) 
  
})

test_that("check array validation with single item types and list parameter with error", {
  
  arr2 = openeo:::Array$new(name = "array_typed",items = list(type="number"))
  arr2$setValue(list(2016, 2017, 2018, 2019, "2020", 2021))
  validation = arr2$validate()
  testthat::expect(length(validation) > 0,failure_message = paste0("Error was not spotted")) 
  
})

test_that("check array validation with multiple item types", {

  # the solution is that it is too complex to validate locally and the back-end will check
  arr2 = openeo:::Array$new(name = "array_typed",items = list(type=c("number","string")))
  arr2$setValue(c("2016", "2017", "2018", "2019", "2020", "2021"))
  validation = arr2$validate()
  testthat::expect(length(validation)==0,failure_message = paste0("Validation failed with reason: ",validation)) 
  
})
