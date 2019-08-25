# tested against r-back-end v0.3.1-3 version 0.3.1

library(openeo)
library(magrittr)

rbackendHost = "http://localhost:8000"
con = connect(host = rbackendHost, user = "test", password = "test")  #add username and password

# View the offered services
con %>% services()

# Create a process graph builder object
pgb = con %>% pgb()

# define process graph
graph = pgb$collection$sentinel2_subset %>% pgb$filter_daterange(extent = c("2017-04-01T00:00:00Z", "2017-05-31T00:00:00Z")) %>% pgb$filter_bbox(extent = list(west = 699960, 
    south = 7897000, east = 702960, north = 7900000, crs = "EPSG:32734")) %>% pgb$NDVI(red = "B4", nir = "B8") %>% pgb$min_time()

taskToJSON(graph)

con %>% listServices()

service_id = con %>% defineService(type = "wms", process_graph = graph, title = "Minimum NDIV April/Mai 2017", enabled = TRUE, plan = "free", parameters = list(version = "1.3.0"))

service = con %>% describeService(service_id)

service_endpoint = service$url
service_endpoint
