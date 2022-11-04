test_that("process list parsing works", {
  process_list = readRDS(system.file("openeo_processes/openeo_processes_1.2.0.rds",package = "openeo"))
  
  openeo:::active_process_list(process_list = process_list)
  
  expect_silent(processes())
})
