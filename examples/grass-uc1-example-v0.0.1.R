# IMPORTANT NOTE: the code is only viable for R-client version 0.1.0

library(devtools)
install_github(repo = "Open-EO/openeo-r-client", ref = "0.1.0")


library(openeo)
library(httr)
library(raster)

graasHost = "http://openeo.mundialis.de:5000"

con = connect(host = graasHost, disable_auth = TRUE)
con %>% capabilities()

collection_names = (con %>% listCollections())$product_id
con %>% listProcesses()

str(con %>% describeCollection("LL.sentinel2A_openeo_subset.strds.S2A_B04"))

str(con %>% describeProcess("NDVI"))

s2a_b04 = collection("LL.sentinel2A_openeo_subset.strds.S2A_B04", id_name = "product_id")
filter_bbox_1 = process(process_id = "filter_bbox", collections = list(s2a_b04), bottom = 38.9, left = -4.8, right = -4.6, top = 39.1, ewres = 1e-04, nsres = 1e-04, 
    srs = "EPSG:4326")
ndvi__b4_chain = process(process_id = "filter_daterange", collections = list(filter_bbox_1), from = "2017-04-12 11:17:08", to = "2017-09-04 11:18:26")

s2a_b08 = collection("LL.sentinel2A_openeo_subset.strds.S2A_B08", id_name = "product_id")
filter_bbox_2 = process(process_id = "filter_bbox", collections = list(s2a_b08), bottom = 38.9, left = -4.8, right = -4.6, top = 39.1, ewres = 1e-04, nsres = 1e-04, 
    srs = "EPSG:4326")
ndvi__b8_chain = process(process_id = "filter_daterange", collections = list(filter_bbox_2), from = "2017-04-12 11:17:08", to = "2017-09-04 11:18:26")

ndvi_calc = process(process_id = "NDVI", collections = list(ndvi__b4_chain, ndvi__b8_chain), red = "S2A_B04", nir = "S2A_B08")
task = process(process_id = "min_time", collections = list(ndvi_calc))
exportedTask = process(process_id = "raster_exporter", collections = list(task))

job_id = con %>% queueTask(list(process_graph = exportedTask))  #for API 0.0.1 it should be only the process graph and not wrapped
# note that queueTask is the equivalent to ...?evaluate=lazy and it is directly processed

job = con %>% queryJob(job_id)
job$resources[[1]]

# con %>% openeo:::deleteJob(job_id)


img = GET(job$resources[[1]])
writeBin(content(img), "graas-uc1.tiff")
r = raster("graas-uc1.tiff")
spplot(r)

r2 = con$uploadJob(task = list(process_graph = exportedTask), "batch")  #batch mode
job2 = con %>% queryJob(r2)
job2$status
job2$resources[[1]]

# con %>% openeo:::deleteJob(r2)

