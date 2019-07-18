context("test-coercions")
library(jsonlite)

test_that("bandlist coercion gee", {
  bandlist = fromJSON(txt="[
      {
        \"name\": \"B1\",
        \"description\": \"Aerosols\",
        \"gee:gsd\": \"meters\",
        \"gee:scale\": 0.0001
      },
      {
        \"name\": \"B2\",
        \"description\": \"Blue\",
        \"gee:gsd\": \"meters\",
        \"gee:scale\": 0.0001
      },
      {
        \"name\": \"B3\",
        \"description\": \"Green\",
        \"gee:gsd\": \"meters\",
        \"gee:scale\": 0.0001
      },
      {
        \"name\": \"B4\",
        \"description\": \"Red\",
        \"gee:gsd\": \"meters\",
        \"gee:scale\": 0.0001
      },
      {
        \"name\": \"B5\",
        \"description\": \"Red Edge 1\",
        \"gee:gsd\": \"meters\",
        \"gee:scale\": 0.0001
      },
      {
        \"name\": \"B6\",
        \"description\": \"Red Edge 2\",
        \"gee:gsd\": \"meters\",
        \"gee:scale\": 0.0001
      },
      {
        \"name\": \"B7\",
        \"description\": \"Red Edge 3\",
        \"gee:gsd\": \"meters\",
        \"gee:scale\": 0.0001
      },
      {
        \"name\": \"B8\",
        \"description\": \"NIR\",
        \"gee:gsd\": \"meters\",
        \"gee:scale\": 0.0001
      },
      {
        \"name\": \"B8A\",
        \"description\": \"Red Edge 4\",
        \"gee:gsd\": \"meters\",
        \"gee:scale\": 0.0001
      },
      {
        \"name\": \"B9\",
        \"description\": \"Water vapor\",
        \"gee:gsd\": \"meters\",
        \"gee:scale\": 0.0001
      },
      {
        \"name\": \"B10\",
        \"description\": \"Cirrus\",
        \"gee:gsd\": \"meters\",
        \"gee:scale\": 0.0001
      },
      {
        \"name\": \"B11\",
        \"description\": \"SWIR 1\",
        \"gee:gsd\": \"meters\",
        \"gee:scale\": 0.0001
      },
      {
        \"name\": \"B12\",
        \"description\": \"SWIR 2\",
        \"gee:gsd\": \"meters\",
        \"gee:scale\": 0.0001
      },
      {
        \"name\": \"QA10\",
        \"description\": \"Always empty\",
        \"gee:gsd\": \"meters\"
      },
      {
        \"name\": \"QA20\",
        \"description\": \"Always empty\",
        \"gee:gsd\": \"meters\"
      },
      {
        \"name\": \"QA60\",
        \"description\": \"Cloud mask\",
        \"gee:gsd\": \"meters\"
      }
  ]",simplifyDataFrame = FALSE)
  
  class(bandlist) = "BandList"
  
  df = as.data.frame(bandlist)
  expect_equal(class(df), "data.frame")
  expect_equal(nrow(df),16)
  expect_equal(ncol(df),4)
  expect_equal(is.na(df[16,"gee:scale"]),TRUE)
})
