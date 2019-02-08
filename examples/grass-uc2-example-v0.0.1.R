#
# IMPORTANT NOTE:
# the code is only viable for R-client version 0.1.0
#

library(devtools)
install_github(repo = "Open-EO/openeo-r-client", ref = "0.1.0")


library(openeo)
library(httr)
library(raster)

graasHost = "http://openeo.mundialis.de:5000"

collection = collection("LL.openeo_mapset_0.strds.S2A_B08_filter_daterange_NDVI", id_name = "product_id")
filter_bbox = process(process_id = "filter_bbox", collections=list(collection), bottom=38.9, left=-4.8, right= -4.6, top = 39.1, ewres=0.0001,nsres=0.0001,srs="EPSG:4326")
filter_dr = process(process_id="filter_daterange",collections=list(filter_bbox), from="2017-04-12 11:17:08", to="2017-09-04 11:18:26")
udf_reduce_time = process(process_id = "udf_reduce_time",collections=list(filter_dr),python_file_url="https://storage.googleapis.com/datentransfer/aggr_func.py")
exportedTask = process(process_id="raster_exporter", collections=list(udf_reduce_time))


job_id = con$uploadJob(task = list(process_graph = exportedTask),"batch") #batch mode

job = con %>% queryJob(job_id)
job$status

# due this some times, the process might run longer


img = GET(job$resources[[1]])
filename = "graas-uc2.tiff"
writeBin(content(img),filename)
r = raster(filename)
spplot(r)
