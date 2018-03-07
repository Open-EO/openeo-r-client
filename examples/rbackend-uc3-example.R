con = connect(host = "http://localhost:8000/api/",user = "test",password = "test",rbackend=TRUE)

con %>% listProcesses()
con %>% describeProcess("filter_bands")
con %>% describeProcess("zonal_statistics")

con %>% uploadUserData(content="polygons.geojson",target="/polygons.geojson")
con %>% listFiles()

task = collection("sentinel2_subset",id_name="product_id") %>%
  process("filter_bands",prior.name = "imagery",bands = "8") %>%
  process("filter_daterange",prior.name = "imagery", from="2017-04-01", to="2017-07-01") %>%
  process("zonal_statistics",prior.name = "imagery", regions = "/users/me/files/polygons.geojson", func="median")

file = con %>% executeTask(task=task,format="GPKG",output_file = "use_case3_median.gpkg")

con %>% listJobs()
con %>% defineJob(task = task,format = "GPKG")
file = con %>% downloadJob(job_id="FkcX8HjuC5kUct5") # output is currently raw binary, writeBin(file,"file"), should be a list of urls