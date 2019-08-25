# Rclient -> EODC version: 0.2.2

library(openeo)
eodcHost = "http://openeo.eodc.eu"
con = connect(host = eodcHost, user = "", password = "")  #add username and password
con %>% listCapabilities()
con %>% listFormats()

con %>% listCollections()
str(con %>% describeCollection("s2a_prd_msil1c"))
con %>% listProcesses()
str(con %>% describeProcess("NDVI"))

task = collection("s2a_prd_msil1c", "product_id") %>% process("filter_bbox", prior.name = "imagery", left = 652000, right = 5161000, bottom = 5181000, top = 672000, 
    srs = "EPSG:32632") %>% process("filter_daterange", prior.name = "imagery", from = "2017-01-01", to = "2017-01-08") %>% process("NDVI", prior.name = "imagery", 
    red = "B04", nir = "B08") %>% process("min_time", prior.name = "imagery")

job_id = con %>% defineJob(task = task, format = "GTiff")
str(con %>% describeJob(job_id))

con %>% queueJob(job_id)
str(con %>% describeJob(job_id))

writeBin(con %>% downloadJob(job_id = job_id), "eodc-uc1-test.tif")
