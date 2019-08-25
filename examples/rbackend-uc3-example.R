# checked with R back-end version v0.3.1-3 version 0.3.1

library(openeo)

rbackendHost = "http://localhost:8000/"
con = connect(host = rbackendHost, user = "test", password = "test")

con %>% describeProcess("filter_bands")
con %>% describeProcess("zonal_statistics")

download.file("https://raw.githubusercontent.com/Open-EO/openeo-r-client/master/examples/polygons.geojson", destfile = "polygons.geojson")

# upload the polygons to the r-back-end
con %>% uploadUserData(content = "polygons.geojson", target = "/polygons.geojson")

# check if they were uploaded correctly
con %>% listFiles()

# process graph definition using the process graph builder
pgb = con %>% pgb()

graph = pgb$collection$sentinel2_subset %>% pgb$filter_bands(bands = "B8") %>% pgb$filter_daterange(extent = c("2017-04-01T00:00:00Z", "2017-07-01T00:00:00Z")) %>% 
    pgb$zonal_statistics(regions = "/polygons.geojson", func = "median")

# calculate and download the data using the preview feature
file = con %>% preview(task = graph, format = "GPKG", output_file = "use_case3_median.gpkg")


con %>% listJobs()

# create a job
job_id = con %>% defineJob(task = graph, title = "Use Case 3", format = "GPKG")

# queue the processing
con %>% orderResults(job_id = job_id)

results = con %>% listResults(job_id = job_id)
link_list = sapply(results$links, function(link) link$href)

# download the file(s) either with utils::download.file or httr::GET
