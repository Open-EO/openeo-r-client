test_that("connect with invalid url fails when setting via initialization", {
  x = openeo:::OpenEOClient$new(host = "example.foo")
  
  
  expect_error(x$connect(),regexp = "resolve host: example.foo")
})

test_that("connect with valid url", {
  skip_if_offline()
  x = openeo:::OpenEOClient$new(host = "https://openeo.cloud")
  
  con = x$connect()
  expect(TRUE,failure_message = "connecting got an error")
})