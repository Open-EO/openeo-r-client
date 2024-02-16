test_that("connect with invalid url fails when setting via initialization", {
  x = openeo:::OpenEOClient$new(host = "example.foo")
  
  expect_error(x$connect())
})

test_that("connect with missing url fails when setting via initialization", {
  x = openeo:::OpenEOClient$new(host = NULL)
  expect_message(x$connect(),regexp = "Host-URL is missing")
})

test_that("connect with valid url", {
  skip_if_offline()
  x = openeo:::OpenEOClient$new(host = "https://openeo.cloud")
  
  con = x$connect()
  expect(TRUE,failure_message = "connecting got an error")
})