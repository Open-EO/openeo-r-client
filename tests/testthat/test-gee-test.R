# test case ----
test_that("everything works with reference back-end", {
  # comment the skip out to run this larger test and set credentials
  testthat::skip("only for manual use")
  host = "https://earthengine.openeo.org"
  user = ""
  pwd = ""
  
  # connecting ----
  con = connect(host = host)
  expect(length(c) > 0, failure_message = "Connection is NULL")
  expect("OpenEOClient" %in% class(con), failure_message = "Connection is not of class OpenEOClient")
  expect_equal(status(con), "connected")
  
  con = login(user = user,
              password = pwd)
  expect_equal(status(con), "authenticated")
  
  c = capabilities()
  
  expect(length(c) > 0, failure_message = "Result is NULL")
  expect("OpenEOCapabilities" %in% class(c), failure_message = "Result is not the capabilities object")
  
  service_types = list_service_types()
  expect(length(service_types) > 0, failure_message = "'service_types' is NULL")
  expect(is.list(service_types), failure_message = "'service_types' is not a list")
  expect("ServiceType" %in% class(service_types[[1]]), failure_message = "Element of process list is not of type 'ServiceInfo'")
  
  formats = list_file_formats()
  expect(length(formats) > 0, failure_message = "'formats' is NULL")
  expect("FileFormatList" %in% class(formats), failure_message = "'formats' is not of type FileFormatList")
  expect("FileFormat" %in% class(formats$output[[1]]), failure_message = "Element of process list is not of type 'ProcessInfo'")
  
  # collections ----
  collections = list_collections()
  expect(length(collections) > 0, failure_message = "collections is NULL")
  expect("CollectionList" %in% class(collections), failure_message = "'collections' is not a CollectionList object")
  
  c1 = describe_collection("COPERNICUS/S2")
  expect(length(c1) > 0 , failure_message = "'c1' is NULL")
  expect("Collection" %in% class(c1), failure_message = "'c1' is not a Collection")
  
  # processes ----
  processes = list_processes()
  expect(length(processes) > 0, failure_message = "'processes' is NULL")
  expect(is.list(processes), failure_message = "'processes' is not a list")
  expect("ProcessInfo" %in% class(processes[[1]]), failure_message = "Element of process list is not of type 'ProcessInfo'")
  
  # process graph definition ----
  p = processes()
  expect(length(p) > 0 , failure_message = "'p' is NULL")
  expect("ProcessCollection" %in% class(p), failure_message = "'p' is not a ProcessCollection")
  
  s2 = collections$`COPERNICUS/S2`
  dims = dimensions(s2)
  
  data1 = p$load_collection(id = s2,
                            spatial_extent = list(west=-2.7634,south=43.0408,east=-1.121,north=43.8385),
                            temporal_extent = c("2018-04-30","2018-06-26"),
                            bands = c("B4","B8"))
  
  ndvi = p$reduce_dimension(data = data1, dimension = dims$bands,reducer = function(x, context) {
    B4 = x["B4"]
    B8 = x["B8"]
    
    return(p$normalized_difference(x = B4,y = B8))
  })
  
  reducer = p$reduce_dimension(data = ndvi,dimension = dims$t, reducer = function(x, context) {
    min(x)
  })
  
  apply_linear_transform = p$apply(data = reducer, process = function(x,context) {
    p$linear_scale_range(x = x, inputMin = -1, inputMax = 1,outputMin = 0,outputMax = 255)
  })
  
  final = p$save_result(data = apply_linear_transform,format = formats$output$PNG)
  
  expect(length(final) > 0,failure_message = "'final' is NULL")
  expect("ProcessNode" %in% class(final), failure_message = "'final' is not a ProcessNode")
  
  graph = as(final,"Graph")
  expect(graph$validate(),failure_message = "Client-side validation failed.")
  
  # validation_result = validate_process(graph=final)
  expect_message(validate_process(graph=final), regexp = ".*successfully validated.*")
  
  # services ----
  service = create_service(type = service_types$xyz,
                           graph = final,
                           title = "UC1 reference service with R", 
                           description = "Created a XYZ service from R using the graph for Use Case 1 (NDVI calculation). The service is used from the RNotebook example for GEE")
  lservices = list_services()
  expect("ServiceList" %in% class(lservices), failure_message = "Return of 'list_services' is not a 'ServicesList'")
  
  service = describe_service(service = service)
  expect("Service" %in% class(service),failure_message = "Returned value for describe service is not class 'Service'")
  
  # try a non existent service
  expect_error(describe_service("gsdf3sd"))
  
  expect_message(delete_service(service = service),regexp = ".*successfully.*")
  
  # sync call ----
  r = compute_result(graph=graph,output_file = "gee_test.png")
  
  expect(file.exists("gee_test.png"),failure_message = "sync call failed to store a file")
  file.remove("gee_test.png")
  
  # async call ----
  job =  create_job(graph=final,title="Testthat::UC1 Rclient NDVI")
  job_started = start_job(job = job)
  
  jobs = list_jobs()
  expect("JobList" %in% class(jobs),failure_message = "job list is not returned as 'JobList'")
  
  job_description = describe_job(job)
  expect("Job" %in% class(job), failure_message = "Job information is no 'Job' object")
  expect("ProcessInfo" %in% class(job$process),failure_message = "defined process in job is no 'ProcessInfo' object")
  expect("Json_Graph" %in% class(job$process$process_graph), failure_message = "process graph representation is no 'Json_Graph' object")
  
  Sys.sleep(25)
  expect(status(job) == "finished", failure_message = "Async job was not 'finished' after 25 seconds.")
  
  job_results = list_results(job)
  expect("ResultList" %in% class(job_results), failure_message = "the list of results is not a 'ResultList'")
  expect("AssetList" %in% class(job_results$assets), failure_message = "the files in the ResultList are not an 'AssetList'")
  
  assets = as.data.frame(job_results)
  expect(nrow(assets) > 0,failure_message = "No processed files...")
  
  
  files = download_results(job = job,folder = "./gee_test/")
  expect(length(files)>0,failure_message = "no files downloaded")
  
  expect(all(sapply(files,file.exists)), failure_message = "async call did not process data to be stored")
  
  expect_message(delete_job(job = job),regexp = ".*successfully deleted.*")
  unlink("./gee_test/")
})
