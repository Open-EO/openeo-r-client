test_that("UdfCode Argument takes script as character", {
  code = "udf = function(data,context) {return(data)}"
  
  arg = openeo:::UdfCodeArgument$new()
  
  arg$setValue(code)
  
  error = arg$validate()
  
  expect(is.null(error),failure_message = paste0("not valid, reason: ",error))
})


test_that("UdfCode Argument validates with function", {
  arg = openeo:::UdfCodeArgument$new()
  
  udf = function(a) {
    a
  }
  arg$setValue(udf)
  
  error = arg$validate()
  
  expect(is.null(error),failure_message = paste0("not valid, reason: ",error))
})

test_that("UdfCode Argument validates with list", {
  arg = openeo:::UdfCodeArgument$new()
  
  arg$setValue(list(udf=function(a) {a}))
  
  error = arg$validate()
  
  expect(is.null(error),failure_message = paste0("not valid, reason: ",error))
})


test_that("UdfCode Argument validates with file", {
  arg = openeo:::UdfCodeArgument$new()
  
  tmp = tempfile()
  file.create(tmp)
  
  arg$setValue(tmp)
  
  error = arg$validate()
  file.remove(tmp)
  
  expect(is.null(error),failure_message = paste0("not valid, reason: ",error))
})


test_that("UdfCode Argument serializes with file", {
  arg = openeo:::UdfCodeArgument$new()
  
  tmp = tempfile()
  udf = function(a) {
    a
  }
  
  cat(paste0("udf = ",deparse1(udf,collapse="\n")), file = tmp)
  
  arg$setValue(tmp)
  
  serialized = arg$serialize()
  file.remove(tmp)
  
  if (.Platform$OS.type == "unix") {
    expect(serialized == "udf = function (a) \n{\n    a\n}",failure_message = "file is wrongly serialized on unix")
  } else {
    expect(serialized == "udf = function (a) \r\n{\r\n    a\r\n}",failure_message = "file is wrongly serialized on windows")
  }
  
    
  
  
})

test_that("UdfCode Argument serializes with sting", {
  arg = openeo:::UdfCodeArgument$new()
  
  udf = "udf = function (a) \r\n{\r\n    a\r\n}"
  
  arg$setValue(udf)
  
  serialized = arg$serialize()
  
  expect(serialized == "udf = function (a) \r\n{\r\n    a\r\n}",failure_message = "file is wrongly serialized")
})
