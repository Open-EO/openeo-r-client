library(openeo)
library(tibble)
library(magrittr)

# Add credentials here (configure connect) or point to a R script that holds the credentials and cares for connection

host = "http://openeo.vgt.vito.be/openeo/0.4.0/"
con = connect(host = host)

con %>% capabilities()

con %>% list_service_types()

con %>% list_file_types() # result is not valid api schema

debug()
colls = con %>% list_collections()
debug.off()

debug()
con %>% describe_collection("S2_FAPAR_SCENECLASSIFICATION_V102_PYRAMID") # message body is 'nul'
debug.off()

con %>% list_processes()

debug()
con %>% list_jobs() # gets a HTTP 200 with info to "create a new batch processing job using POST" as text (no JSON)
debug.off()

# con %>% listServices() does not work despite enabled

pgb = con %>% process_graph_builder()

# 2019-02-07: NDVI not supported -> graph will be incomplete, examples afterwards are given, but cannot be tested
task = pgb$collection$BIOPAR_FAPAR_V1_GLOBAL %>%
  pgb$filter_bbox(left=16.138916, bottom=48.138600, right=16.524124, top=48.320647, srs="EPSG:4326") %>%
  pgb$filter_daterange(from = "2016-01-01T00:00:00Z", to= "2016-03-10T23:59:59Z") %>%
  pgb$NDVI() %>%
  pgb$min_time()

con %>% preview(task=task,format="GTiff","uc1_vito_result.tif")

job_id = con %>% defineJob(task=task,format="GTiff")

con %>% orderResults(job_id = job_id)

results = con %>% listResults(job_id = job_id)

urls = sapply(results$links, function(link)link$url)

#download results by accessing the URLs that are provided, e.g. using httr:GET or utils::download.file



