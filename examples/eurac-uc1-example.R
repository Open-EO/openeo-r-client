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
  pgb$filter_bbox(extent=list(west=10.5,east=11.0,south=46.0,north=46.5)) %>%
  pgb$filter_daterange(extent = c("2017-01-01T00:00:00Z","2017-01-31T00:00:00Z")) %>%
  pgb$NDVI(red="B04",nir="B8A") %>%
  pgb$min_time()

raster = eurac %>% preview(task=task,
                               format="tiff",
                               output_file = "eurac_test.tif")

# eurac %>% listJobs()

job_id = eurac %>% defineJob(task=task,format="tiff")

eurac %>% describeJob(job_id)

eurac %>% orderResults(job_id = job_id)

eurac %>% listResults(job_id)