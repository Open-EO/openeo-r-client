# Example RClient (v0.6.0) <-> EODC (0.4.0)
# https://openeo.org/openeo/news/2019/03/07/openeo-api-040.html

library(openeo)
library(magrittr)
library(tibble)
library(jsonlite)

host_url = "https://openeo.eodc.eu"

con = connect(host = host_url, version="0.4.0", login_type = "oidc",external="google",exchange_token = "id_token")

graph = con %>% process_graph_builder()

# creating the graph
data = graph$load_collection(id = graph$data$s2a_prd_msil1c, 
                             spatial_extent = list(west = 11.2792, 
                                                   south = 46.4643, 
                                                   east = 11.4072, 
                                                   north = 46.5182), 
                             temporal_extent = c("2018-06-04T00:00:00Z","2018-06-23T00:00:00Z"))

spectral_reduce = graph$reduce(data=data, dimension = "bands", reducer = function(x) {
  B08 = x[1]
  B02 = x[2]
  B04 = x[3]
  (2.5 * (B08 - B04)) / sum(B08, 6 * B04, -7.5 * B02,1)
})


temporal_reduce = graph$reduce(data=spectral_reduce, dimension = "temporal", reducer=function(x) {
  min(x)
})

final = graph$save_result(data = temporal_reduce, format = "GTiff")

graph$setFinalNode(node = final)

# print as JSON
graph

# client side graph validation
graph$validate()

con %>% validate_process_graph(graph)
