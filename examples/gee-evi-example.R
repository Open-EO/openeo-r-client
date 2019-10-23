# Example GEE -> RClient
# https://openeo.org/openeo/news/2019/03/07/openeo-api-040.html

library(openeo)
library(magrittr)
library(tibble)
library(jsonlite)

user = "group8"
pwd = "test123"

gee_host_url = "https://earthengine.openeo.org"

con = connect(host = gee_host_url, version="0.4.2", user = user, password = pwd, login_type = "basic")

graph = con %>% process_graph_builder()

# creating the graph
data = graph$load_collection(graph$data$`COPERNICUS/S2`,
                             spatial_extent = list(
                               west=16.1,
                               east=16.6,
                               north=48.6,
                               south= 47.2
                             ),
                             temporal_extent = list(
                               "2018-01-01", "2018-02-01"
                             ),
                             bands=list("B08","B04","B02"))

spectral_reduce = data %>% graph$reduce(dimension = "bands")

evi_graph = con %>% callback(spectral_reduce,parameter = "reducer")

nir = evi_graph$data$data %>% evi_graph$array_element(0)
red = evi_graph$data$data %>% evi_graph$array_element(1)
blue = evi_graph$data$data %>% evi_graph$array_element(2)

p1 = evi_graph$product(data = list(red, 6))
p2 = evi_graph$product(data = list(blue, -7.5))

sub = evi_graph$subtract(data = list(nir,red))
sum = evi_graph$sum(data = list(1,nir,p1,p2))
div = evi_graph$divide(data = list(sub,sum))

p3 = evi_graph$product(data = list(2.5,div))

evi_graph$setFinalNode(p3)

temporal_reduce = spectral_reduce %>% graph$reduce(dimension = "temporal")

min_time_graph = con %>% callback(temporal_reduce, parameter = "reducer")
min_time_graph$min(data = min_time_graph$data$data) %>% min_time_graph$setFinalNode()

apply_linear_transform = temporal_reduce %>% graph$apply()

cb2_graph = gee %>% callback(apply_linear_transform, "process")
cb2_graph$linear_scale_range(x = cb2_graph$data$x, inputMin = -1, inputMax = 1, outputMin = 0, outputMax = 255) %>% cb2_graph$setFinalNode()


apply_linear_transform %>% graph$save_result(format="PNG") %>% graph$setFinalNode()

# print as JSON
graph

# write to file 
cat(graphToJSON(graph),file = "r-evi-phenology-graph.json")

# client side graph validation
graph$validate()

#
# Or do the same using the band_arithmetic
#
graph = con %>% process_graph_builder()

# creating the graph
data = graph$load_collection(graph$data$`COPERNICUS/S2`,
                             spatial_extent = list(
                               west=16.1,
                               east=16.6,
                               north=48.6,
                               south= 47.2
                             ),
                             temporal_extent = list(
                               "2018-01-01", "2018-02-01"
                             ),
                             bands=list("B08","B04","B02"))

evi_calculation = data %>% band_arithmetics(graph = graph,function(x) {
  2.5*((x[1]-x[2])/sum(1,x[1],6*x[2],-7.5*x[3]))
})

temporal_reduce = evi_calculation %>% graph$reduce(dimension = "temporal")

min_time_graph = con %>% callback(temporal_reduce, parameter = "reducer")
min_time_graph$min(data = min_time_graph$data$data) %>% min_time_graph$setFinalNode()

apply_linear_transform = temporal_reduce %>% graph$apply()

cb2_graph = con %>% callback(apply_linear_transform, "process")
cb2_graph$linear_scale_range(x = cb2_graph$data$x, inputMin = -1, inputMax = 1, outputMin = 0, outputMax = 255) %>% cb2_graph$setFinalNode()


apply_linear_transform %>% graph$save_result(format="PNG") %>% graph$setFinalNode()

graph$validate()

graph

