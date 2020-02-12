# Example RClient v0.6.0 <-> EURAC 0.4.2
# https://openeo.org/openeo/news/2019/03/07/openeo-api-040.html

library(openeo)
library(tibble)

# enter valid credentials!
user = ""
pwd = ""

host_url = "https://openeo.eurac.edu"

eurac = connect(host = host_url, version="0.4.2", user = user, password = pwd, login_type = "basic")

graph = process_graph_builder(con = eurac)

coll = describe_collection(con = eurac, id = graph$data$openEO_S2_32632_10m_L2A)
names(dimensions(coll))

describe_process(eurac,"load_collection")

# creating the graph
data = graph$load_collection(id = graph$data$openEO_S2_32632_10m_L2A, 
                              spatial_extent = list(west = 11.2792, 
                                                    south = 46.4643, 
                                                    east = 11.4072, 
                                                    north = 46.5182), 
                              temporal_extent = c("2018-06-04T00:00:00Z","2018-06-23T00:00:00Z"),
                              bands = c("B08","B02","B04"))

spectral_reduce = graph$reduce(data=data, dimension = "spectral",reducer = function(x) {
  B08 = x[1] / 10000
  B02 = x[2] / 10000
  B04 = x[3] / 10000
  (2.5 * (B08 - B04)) / sum(B08, 6 * B04, -7.5 * B02,1)
})

# temporal_reduce = graph$reduce(data=spectral_reduce, dimension = "DATE", reducer = function(x) {
#   min(x)
# })

temporal_reduce = graph$min_time(data = spectral_reduce)

final = graph$save_result(data = temporal_reduce,format="GTiff")

graph$setFinalNode(final)

# print as JSON
graph

# client side graph validation
graph$validate()

compute_result(con=eurac, graph = graph, format = "GTiff",output_file = "min_evi.tif")

