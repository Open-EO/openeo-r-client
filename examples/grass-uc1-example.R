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

# get help
# library(help="openeo")

# not yet implemented (?)
# conn %>% api_versions()

# list capabilities, collection and processes
con %>% capabilities()
con %>% list_collections()
con %>% list_processes()
summary(con %>% list_processes())
con %>% list_file_types()

descriptions = con %>% list_collections()

collection_names = (con %>% list_collections())$product_id
str(con %>% describe_collection(id="utm32n.openeo_bolzano.strds.openeo_bolzano_S2"))
con %>% describe_collection(id="utm32n.openeo_bolzano.strds.openeo_bolzano_S2")

con  %>% list_jobs()

str(con %>% describe_process("ndvi"))

%% tested up to here.

% old code API v.0.1:
s2a_b04 = con %>% collection("utm32n.openeo_bolzano.strds.openeo_bolzano_S2", id_name = "product_id")
filter_bbox_1 = process(process_id = "filter_bbox", collections = list(s2a_b04), bottom = 45.93349650, left = 10.29048407, right = 11.75558492, top = 46.94616825, ewres = 1e-04, nsres = 1e-04, 
    srs = "EPSG:4326")
ndvi__b4_chain = process(process_id = "filter_daterange", collections = list(filter_bbox_1), from = "2018-05-01 08:17:08", to = "2018-10-01 22:17:08")

s2a_b08 = collection("utm32.openeo_bolzano.strds.S2A_B08", id_name = "product_id")
filter_bbox_2 = process(process_id = "filter_bbox", collections = list(s2a_b08), bottom = 45.93349650, left = 10.29048407, right = 11.75558492, top = 46.94616825, ewres = 1e-04, nsres = 1e-04, 
    srs = "EPSG:4326")
ndvi__b8_chain = process(process_id = "filter_daterange", collections = list(filter_bbox_2), from = "2018-05-01 08:17:08", to = "2018-10-01 22:17:08")

ndvi_calc = process(process_id = "ndvi", collections = list(ndvi__b4_chain, ndvi__b8_chain), red = "S2A_B04", nir = "S2A_B08")
task = process(process_id = "min_time", collections = list(ndvi_calc))
exportedTask = process(process_id = "raster_exporter", collections = list(task))

job_id = con %>% queueTask(list(process_graph = exportedTask))  #for API 0.0.1 it should be only the process graph and not wrapped
# note that queueTask is the equivalent to ...?evaluate=lazy and it is directly processed

job = con %>% queryJob(job_id)
job$resources[[1]]

# con %>% openeo:::deleteJob(job_id)


img = GET(job$resources[[1]])
writeBin(content(img), "grass-actinia-uc1.tiff")
r = raster("grass-actinia-uc1.tiff")
spplot(r)

r2 = con$uploadJob(task = list(process_graph = exportedTask), "batch")  #batch mode
job2 = con %>% queryJob(r2)
job2$status
job2$resources[[1]]

# con %>% openeo:::deleteJob(r2)


% API v0.4
# creating the graph
graph = con %>% process_graph_builder()

data = graph$load_collection(graph$data$utm32n.openeo_bolzano.strds.openeo_bolzano_S2,
                             spatial_extent = list(
                               west=10.29048407,
                               east=11.75558492,
                               north=46.94616825,
                               south=45.93349650
                             ),
                             temporal_extent = list(
                               "2018-05-01", "2018-10-01"
                             ),
                             bands=list("B08","B04","B02"))

spectral_reduce = data %>% graph$reduce(dimension = "bands")

