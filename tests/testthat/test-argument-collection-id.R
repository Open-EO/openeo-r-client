test_that("setting string as collection id works", {
  cid = openeo:::CollectionId$new()
  cid$setValue("some-dem")
  
  expect(cid$serialize() == "some-dem",failure_message = "Value was not set correctly")
  
  expect(is.null(cid$validate()),failure_message = "Validation of String as Collection ID did not work")
})


test_that("setting Collection object as Collection-ID works", {
  cid = openeo:::CollectionId$new()
  
  # very minimal Collection
  value=list(id = "SENTINEL2_L1C")
  class(value) = "Collection"
  
  cid$setValue(value)
  
  expect(cid$serialize() == "SENTINEL2_L1C",failure_message = "Value was not set correctly")
  
  expect(is.null(cid$validate()),failure_message = "Validation of String as Collection ID did not work")
})

