library(openeo)

euracHost = ""

eurac = connect(host = euracHost,disable_auth = TRUE)
# eurac %>% listProcesses()
eurac %>% listCapabilities()
str(eurac %>% listFormats())

eurac %>% listCollections()
eurac %>% describeCollection("S2_L2A_T32TPS_20M")

eurac %>% describeCollection("S2_L2A_T32TPS_20M")
eurac %>% describeProcess("filter_bbox")
eurac %>% describeProcess("filter_daterange")
eurac %>% describeProcess("NDVI")
eurac %>% describeProcess("min_time")

task = collection("S2_L2A_T32TPS_20M") %>% 
  process("filter_bbox", prior.name = "imagery", left=652000,right=672000,bottom=5181000,top=5161000,srs="EPSG:32632") %>% 
  process("filter_daterange", prior.name="imagery", from= "2017-01-01", to="2017-01-31" ) %>% 
  process("NDVI", prior.name = "imagery", red="B04",nir="B8A") %>% 
  process("min_time",prior.name="imagery")

# Build a process graph using the ProcesGraphBuilder pgb()
pgb = eurac %>% pgb()

alternative = collection("S2_L2A_T32TPS_20M") %>%
  pgb$filter_bbox(left = 652000, right = 672000, bottom = 5181000, top=5161000,srs="EPSG:32632") %>%
  pgb$filter_daterange(from = "2017-01-01",to = "2017-01-31") %>%
  pgb$NDVI(red = "B04", nir = "B8A") %>% 
  pgb$min_time()

taskToJSON(task)
taskToJSON(alternative)

raster = eurac %>% executeTask(task=task,
                               format="tiff",
                               output_file = "eurac_test.tif")
job_id = eurac %>% defineJob(task=task,format="tiff")
str(eurac %>% describeJob(job_id))

list = eurac %>% downloadJob(job_id = job_id,format="tiff")

writeBin(list,"test2.tif")
str(eurac %>% describeJob(job_id))