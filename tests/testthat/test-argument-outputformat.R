test_that("OutputFormat works for FileFormat object", {
  ff_arg = openeo:::OutputFormat$new()
  
  ff = list(gis_data_types= list("raster"),
            parameters=list(),
            title="GeoTiff",
            type="output",
            name="GTiff")
  class(ff) = "FileFormat"
  
  ff_arg$setValue(ff)
  
  expect(is.null(ff_arg$validate()),failure_message = "validates with error")
  expect(ff_arg$serialize() == "GTiff",failure_message = "Serialized Value is not correct")
})
