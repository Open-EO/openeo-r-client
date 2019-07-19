# RClient -> EURAC
# version: 0.3.1


library(openeo)

euracHost = "http://saocompute.eurac.edu/openEO_0_3_0/openeo/"

eurac = connect(host = euracHost,disable_auth = TRUE)
eurac %>% listProcesses()
eurac %>% listCapabilities()
eurac %>% listFormats()

descriptions = eurac %>% listCollections()

eurac %>% describeCollection("S2_L2A_T32TPS_20M")

eurac %>% describeProcess("filter_bbox")
eurac %>% describeProcess("filter_daterange")
eurac %>% describeProcess("NDVI")
eurac %>% describeProcess("min_time")

# Build a process graph using the ProcesGraphBuilder pgb()
pgb = eurac %>% pgb()

task = pgb$collection$S2_L2A_T32TPS_20M %>%
  pgb$filter_bbox(extent=list(west=10.99,east=11.25,south=46.59,north=46.76)) %>%
  pgb$filter_daterange(extent = c("2016-01-01T00:00:00Z","2016-03-10T23:59:59Z")) %>%
  pgb$NDVI(red="B04",nir="B8A") %>%
  pgb$min_time() %>%
  pgb$stretch_colors(min=-1, max=1)

raster = eurac %>% preview(task=task,
                               format="GTiff",
                               output_file = "eurac_test.tif")

# eurac %>% listJobs()

job_id = eurac %>% defineJob(task=task,format="GTiff")

eurac %>% describeJob(job_id)

eurac %>% orderResults(job_id = job_id)

eurac %>% listResults(job_id)