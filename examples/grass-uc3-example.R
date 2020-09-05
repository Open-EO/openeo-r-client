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

product = "utm32n.openeo_bolzano.strds.openeo_bolzano_S2"
regions_url = "https://storage.googleapis.com/graas-geodata/roi_openeo_use_case_2.geojson"

coll = collection(product, id_name = "product_id")
box = process(process_id = "filter_bbox", collections = list(coll), bottom = 45.93349650, left = 10.29048407, right = 11.75558492, top = 46.94616825, ewres = 1e-04, nsres = 1e-04, srs = "EPSG:4326")
dr = process(process_id = "filter_daterange", collections = list(box), from = "2018-05-01 08:17:08", to = "2018-10-01 22:17:08")
zonal = process(process_id = "zonal_statistics", collections = list(dr), regions = regions_url)

job_id = con$uploadJob(task = list(process_graph = zonal), "batch")  #batch mode

job = con %>% queryJob(job_id)
job$status

info = job$job_info
