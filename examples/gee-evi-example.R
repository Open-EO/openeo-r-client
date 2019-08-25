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

# add missing process definitions for array_element, product, subtract and divide to showcase the evi graph
# this will mock the functions required, but won't be executable or validateable against any back-end
additional_decriptions = list(
  array_element = read_json("https://raw.githubusercontent.com/Open-EO/openeo-processes/master/array_element.json"),
  subtract = read_json("https://raw.githubusercontent.com/Open-EO/openeo-processes/master/subtract.json"),
  product = read_json("https://raw.githubusercontent.com/Open-EO/openeo-processes/master/product.json"),
  divide = read_json("https://raw.githubusercontent.com/Open-EO/openeo-processes/master/divide.json")
)

additional_decriptions = lapply(additional_decriptions, function(elem){
  class(elem) = "ProcessInfo"
  return(elem)
})

con$processes = append(con$processes,additional_decriptions)
  

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

temporal_reduce %>% graph$save_result(format="GTiff") %>% graph$setFinalNode()

graph

graph$validate()




