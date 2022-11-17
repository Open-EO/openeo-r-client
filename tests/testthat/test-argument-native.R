test_that("native Argument is empty", {
  
  arg = openeo:::Argument$new(required=TRUE)

  arg$setValue(NA)
  expect_true(arg$isEmpty(),label="NA is empty")
  
  arg$setValue(integer())
  expect_true(arg$isEmpty(),label="empty vector is empty")
  
  arg$setValue(list())
  expect_true(arg$isEmpty(),label="empty list is empty")
  
  arg$setValue(NULL)
  expect_true(arg$isEmpty(),label="NULL is empty")
  
  arg$setValue(list(a=NA))
  expect_true(arg$isEmpty(),label="list with one NA is empty")
  
  arg$setValue(c(a=NA))
  expect_true(arg$isEmpty(),label="vector with one NA is empty")
})

test_that("native Argument not empty", {
  
  arg = openeo:::Argument$new(required=TRUE)
  
  arg$setValue(1)
  expect_false(arg$isEmpty(),label="integer is not empty")
  
  arg$setValue(c(1,NA))
  expect_false(arg$isEmpty(),label="integer with contained NA is not empty")
  
  arg$setValue(c(1,2,3,4))
  expect_false(arg$isEmpty(),label="vector is not empty")
  
  arg$setValue(list(a=NULL))
  expect_false(arg$isEmpty(),label="list with one NULL is not empty")
  
  arg$setValue(list(a=NA,b=1))
  expect_false(arg$isEmpty(),label="list is not empty")
  
  p = openeo:::ProcessGraphParameter$new()
  arg$setValue(p)
  expect_false(arg$isEmpty(),label="process graph parameter (variable) is not empty")
  
})