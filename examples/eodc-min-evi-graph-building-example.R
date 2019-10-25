# Example RClient <-> EODC
# https://openeo.org/openeo/news/2019/03/07/openeo-api-040.html

library(openeo)
library(magrittr)
library(tibble)
library(jsonlite)

host_url = "https://openeo.eodc.edu"

con = connect(host = host_url, login_type = "oidc",external="google",exchange_token = "id_token")

graph = con %>% process_graph_builder()

# creating the graph
data = graph$load_collection(id = graph$data$s2a_prd_msil1c, 
                             spatial_extent = list(west = 11.2792, 
                                                   south = 46.4643, 
                                                   east = 11.4072, 
                                                   north = 46.5182), 
                             temporal_extent = c("2018-06-04T00:00:00Z","2018-06-23T00:00:00Z"))

spectral_reduce = graph$reduce(data=data, dimension = "bands")

evi_graph = con %>% callback(spectral_reduce,parameter = "reducer", choice_index = 1)

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

temporal_reduce = graph$reduce(data=spectral_reduce, dimension = "temporal")

min_time_graph = con %>% callback(temporal_reduce, parameter = "reducer", choice_index = 1)
min_time_graph$min(data = min_time_graph$data$data) %>% min_time_graph$setFinalNode()

temporal_reduce %>% graph$save_result(format="GTiff") %>% graph$setFinalNode()

# print as JSON
graph

# client side graph validation
graph$validate()

con %>% validate_process_graph(graph)

