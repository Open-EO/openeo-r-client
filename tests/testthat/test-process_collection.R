test_that("process list parsing works", {
  openeo:::demo_processes()
  
  expect_silent(processes())
})
