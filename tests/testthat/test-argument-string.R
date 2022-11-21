test_that("privacy of strings", {
  tmp = tempfile()
  cat("privacy issue",file=tmp)
  
  string_arg = openeo:::String$new()
  string_arg$setValue(tmp)
  
  expect(string_arg$serialize() != "privacy issue",failure_message = "String parameter opens file.")
})
