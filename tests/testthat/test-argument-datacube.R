test_that("test parsing parameter description for subtype='datacube'",{
  json = list(
    name="data",
    description= "The data to deliver in the given file format.",
    schema= list(
      type= "object",
      subtype= "datacube"
    )
  )
  
  param = openeo:::parameterFromJson(json)
  expect_contains(class(param),"datacube")
})
