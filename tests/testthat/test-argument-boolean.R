test_that("validation ProcessNode with Boolean result works", {
  tryCatch({
    br = openeo:::Boolean$new()
    p = openeo:::Process$new(returns=br)
    pn = openeo:::ProcessNode$new(node_id = "ret_bool",process=p)
    
    b2 = openeo:::Boolean$new()
    b2$setValue(pn)
    b2$validate()
    
    expect(TRUE,failure_message = "It doesn't work") 
  }, error = function(e) {
    expect(FALSE,failure_message=e$message)
  })
  
})


test_that("validation ProcessNode with non Boolean result does not succeed", {
  
    int_result = openeo:::Integer$new()
    p = openeo:::Process$new(returns=int_result)
    pn = openeo:::ProcessNode$new(node_id = "ret_int",process=p)
    
    
    b2 = openeo:::Boolean$new()
    ref_process = openeo:::Process$new(returns=b2)
    pn2 = openeo:::ProcessNode$new(node_id = "caller",process=ref_process)
    b2$setProcess(pn2)
    b2$setValue(pn)
    b2$setName("boolean_check")
    err = b2$validate()
    expect(err == "[caller] Parameter 'boolean_check': No logical return from ProcessNode.",failure_message = "It doesn't work") 
})
