test_that("process description works", {
  openeo:::demo_processes()
  p = processes()
  
  expect_silent({
    p1 = describe_process("load_collection")
  })
  expect_silent({
    p2 = describe_process(p$load_collection())
  })
  
  expect_message({
    p3 = describe_process(p$load_collection)
  }, regexp = ".*Cannot derive the functions name.*")
  
})
