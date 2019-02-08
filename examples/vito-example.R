library(openeo)

# Add credentials here (configure connect) or point to a R script that holds the credentials and cares for connection
# source("path/to/credentials/vito_v0.3.1.R")
# --> con = connect(host = vitoURL,disable_auth = TRUE)

con %>% listCapabilities()

con %>% services()

con %>% listFormats()

collection = print(con %>% listCollections())

debug()
con %>% describeCollection("S2_FAPAR_V102_WEBMERCATOR2") # message body is 'nul'
debug.off()

con %>% listProcesses()

debug()
con %>% listJobs()
debug.off()

# con %>% listServices() does not work despite enabled

pgb = con %>% pgb()

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



