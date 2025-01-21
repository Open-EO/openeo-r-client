test_that("ProcessNode can be validated in Number", {
  p =openeo:::demo_processes()
  n = openeo:::Number$new()
  n$setProcess(p$sd(NA))
  
  pn = openeo:::ProcessNode$new(node_id="bla",process=p$multiply(NA,NA))
  
  n$setValue(pn)
  
  expect(is.null(n$validate()),failure_message = "Number validates with error")
  expect(is.null(pn$validate()),failure_message = "ProcessNode validates with error")
})