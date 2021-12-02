test_that("connect with invalid url fails when setting via initialization", {
  x = openeo:::OpenEOClient$new(host = "example.foo")
  
  
  expect_error(x$connect(),regexp = "resolve host: example.foo")
})
