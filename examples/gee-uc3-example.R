# POC: Use Case 3 RClient -> GEE back-end v0.6.0 
#status: not working, because missing process at gee back-end (v0.4)

library(openeo)
library(magrittr)
library(tibble)

gee_host_url = "https://earthengine.openeo.org"

user = "group8"
pwd = "test123"

gee = connect(host = gee_host_url, version = "0.4.2", user = user, password = pwd, login_type = "basic")

# 1. Create a batch job Part 1: create the process graph

graph = process_graph_builder(con = gee)

data = graph$load_collection(id = graph$data$`COPERNICUS/S2`, 
                             spatial_extent = list(west = 16.1, east = 16.6, north = 48.6, south = 47.2), 
                             temporal_extent = list("2017-01-01", "2017-02-01"), 
                             bands = list("B8"))

spectral_reduction = graph$reduce(data = data, dimension = "spectral")

# hint: starting at this point the example doesn't work... no implementation for aggregate_polygon but this is how it should look like
zonal_statistics = graph$aggregate_polygon(data = spectral_reduction,
                                           polygons = list(type = "Polygon", 
                                                           coordinates = list(c(16.138916, 48.320647), 
                                                                              c(16.524124, 48.320647), 
                                                                              c(16.524124, 48.1386), 
                                                                              c(16.138916, 48.1386), 
                                                                              c(16.138916, 48.320647))),
                                           reducer = function(x) {
                                             graph$zonal_statistics(x)
                                           })

final = graph$save_result(data = zonal_statistics,format = "JSON") 

graph$setFinalNode(node = final)

# Part 2: create a job
job_id = create_job(con = gee,
                   graph = graph, 
                   title = "Zonal Statistics / Sentinel 2", 
                   description = "Compute time series of zonal (regional) statistics of Sentinel 2 imagery over user-specified polygons.")

# 2. Start batch processing the job
start_job(con = gee, job = job_id)

# 2.* ping the job to get status updates
describe_job(con = gee, job = job_id)

# 3. Retrieve download links (after the job has finished)
list_results(con = gee, job = job_id)

# 4. Download file(s)
download_results(con = gee, job = job_id, folder = paste0(job_id, "_results"))
