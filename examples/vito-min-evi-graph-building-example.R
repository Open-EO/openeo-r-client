# Example VITO -> RClient
# https://openeo.org/openeo/news/2019/03/07/openeo-api-040.html

library(openeo)
library(magrittr)
library(tibble)
library(jsonlite)


host = "http://openeo.vgt.vito.be/openeo/0.4.2/"

con = connect(host = host)


graph = con %>% process_graph_builder()

# creating the graph
data = graph$load_collection(id = graph$data$SENTINEL2_L2A_SENTINELHUB,
                             spatial_extent = list(
                               west=16.1,
                               east=16.6,
                               north=48.6,
                               south= 47.2
                             ),
                             temporal_extent = list(
                               "2018-01-01", "2018-02-01"
                             ))

spectral_reduce = graph$reduce(data = data, dimension = "bands")

evi_graph = con %>% callback(spectral_reduce,parameter = "reducer",choice_index = 1)

nir = evi_graph$data$data %>% evi_graph$filter_bands(bands = "B08")
red = evi_graph$data$data %>% evi_graph$filter_bands(bands = "B04")
blue = evi_graph$data$data %>% evi_graph$filter_bands(bands = "B02")

p1 = evi_graph$product(data = list(red, 6))
p2 = evi_graph$product(data = list(blue, -7.5))

sub = evi_graph$subtract(data = list(nir,red))
sum = evi_graph$sum(data = list(1,nir,p1,p2))
div = evi_graph$divide(data = list(sub,sum))

p3 = evi_graph$product(data = list(2.5,div))

evi_graph$setFinalNode(p3)

temporal_reduce =  graph$reduce(data = spectral_reduce,dimension = "temporal") # 'binary' is the first parameter, so no pipeing

min_time_graph = con %>% callback(temporal_reduce, parameter = "reducer",choice_index = 1)
min_time_graph$min(data = min_time_graph$data$data) %>% min_time_graph$setFinalNode()

temporal_reduce %>% graph$save_result(format="GTiff") %>% graph$setFinalNode()

# print as JSON
graph

# write to file 
# cat(graphToJSON(graph),file = "r-evi-phenology-graph.json")

# client side graph validation
graph$validate()

# con %>% validate_process_graph(graph)


