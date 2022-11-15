test_that("variable extraction works", {
  openeo:::demo_processes()
  p = processes()
  var1 = create_variable(name="band",description = "The bands to be selected")
  dc = p$load_collection(id = "S2", spatial_extent = list(west=7,east=7.5,south=52,north=52.5),temporal_extent = list("2022-05-20","2022-06-10"),bands = var1)
  
  list_of_vars = variables(dc)
  
  expect(length(list_of_vars) == 1,failure_message = "the one process parameter was not found")
})
