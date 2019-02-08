#
# IMPORTANT NOTE:
# the code is only viable for R-client version 0.1.0
#

library(devtools)
install_github(repo = "Open-EO/openeo-r-client", ref = "0.1.0")


library(openeo)
library(httr)
library(raster)

product = "LL.openeo_mapset_0.strds.S2A_B08_filter_daterange_NDVI"
regions_url = "https://storage.googleapis.com/graas-geodata/roi_openeo_use_case_2.geojson"

coll = collection(product,id_name="product_id")
box = process(process_id = "filter_bbox", collections=list(coll), bottom=38.9, left=-4.8, right= -4.6, top = 39.1, ewres=0.0001,nsres=0.0001,srs="EPSG:4326")
dr = process(process_id="filter_daterange", collections=list(box),from="2017-04-12 11:17:08",to="2017-09-04 11:18:26")
zonal = process(process_id="zonal_statistics",collections=list(dr), regions=regions_url)

job_id = con$uploadJob(task = list(process_graph = zonal),"batch") #batch mode

job = con %>% queryJob(job_id)
job$status

info = job$job_info


