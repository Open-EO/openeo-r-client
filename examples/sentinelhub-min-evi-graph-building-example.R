# Example RClient v0.6.0 <-> Sentinel Hub 0.4.2
# https://openeo.org/openeo/news/2019/03/07/openeo-api-040.html
# deprecated with client version v1.0.0!

library(openeo)
library(magrittr)
library(tibble)
library(jsonlite)


host_url = "https://openeo.sentinel-hub.com/production/"

shub = connect(host = host_url, version="0.4.2")

graph = process_graph_builder(con = shub)

coll = describe_collection(con = shub, id = graph$data$S2L1C)
names(dimensions(coll))
# no dimensions specified, the following graph might be false, because we are not sure how to address them in the
# reducer functions

# creating the graph
data = graph$load_collection(id = graph$data$S2L1C, 
                             spatial_extent = list(west = 11.2792, 
                                                   south = 46.4643, 
                                                   east = 11.4072, 
                                                   north = 46.5182), 
                             temporal_extent = c("2018-06-04T00:00:00Z","2018-06-23T00:00:00Z"),
                             bands = c("B08","B02","B04"))

spectral_reduce = graph$reduce(data=data, dimension = "band",reducer = function(x) {
  B08 = x[1]
  B02 = x[2]
  B04 = x[3]
  (2.5 * (B08 - B04)) / sum(B08, 6 * B04, -7.5 * B02,1)
})

temporal_reduce = graph$reduce(data = spectral_reduce,dimension = "t", reducer = function(x) {
  min(x)
})

final = graph$save_result(data = temporal_reduce,format="GTiff")

graph$setFinalNode(final)

# print as JSON
graph

# client side graph validation
graph$validate()

compute_result(con=shub, graph = graph, format = "GTiff",output_file = "min_evi.tif")

