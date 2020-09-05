# IMPORTANT NOTE: the code is only viable for R-client version 0.1.0
# https://github.com/Open-EO/openeo-r-client/tree/master/examples/

library(devtools)
install_github(repo="Open-EO/openeo-r-client",ref="v0.5.0",dependencies=TRUE)

library(openeo)
library(httr)
library(raster)
library(magrittr) # provides ‘%>%’   forward-pipe operator. etc

# connect to backend
actiniaHost = "https://openeo.mundialis.de"
con = connect(host = actiniaHost, user = "openeo", password = "FIXME", login_type = "basic")  # add username and password

collection = collection("utm32n.openeo_bolzano.strds.openeo_bolzano_S2", id_name = "product_id")
filter_bbox = process(process_id = "filter_bbox", collections = list(collection), bottom = 45.93349650, left = 10.29048407, right = 11.75558492, top = 46.94616825, ewres = 1e-04, nsres = 1e-04, 
    srs = "EPSG:4326")
filter_dr = process(process_id = "filter_daterange", collections = list(filter_bbox), from = "2018-05-0 11:17:08", to = "2018-10-0 11:18:26")
udf_reduce_time = process(process_id = "udf_reduce_time", collections = list(filter_dr), python_file_url = "https://storage.googleapis.com/datentransfer/aggr_func.py")
exportedTask = process(process_id = "raster_exporter", collections = list(udf_reduce_time))


job_id = con$uploadJob(task = list(process_graph = exportedTask), "batch")  # batch mode

job = con %>% queryJob(job_id)
job$status

# due this some times, the process might run longer


img = GET(job$resources[[1]])
filename = "grass-actinia-uc2.tiff"
writeBin(content(img), filename)
r = raster(filename)
spplot(r)
