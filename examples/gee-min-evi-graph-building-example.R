# Example RClient <-> GEE
# https://openeo.org/openeo/news/2019/03/07/openeo-api-040.html

library(openeo)
library(magrittr)
library(tibble)
library(jsonlite)

user = "group8"
pwd = "test123"

gee_host_url = "https://earthengine.openeo.org"

con = connect(host = gee_host_url, version="0.4.2", user = user, password = pwd, login_type = "basic")

graph = process_graph_builder(con = con)

# creating the graph
data = graph$load_collection(graph$data$`COPERNICUS/S2`,
                             spatial_extent = list(
                               west=16.1,
                               east=16.6,
                               north=48.6,
                               south= 47.2
                             ),
                             temporal_extent = list(
                               "2018-04-01", "2018-05-01"
                             ),
                             bands=list("B8","B4","B2"))

spectral_reduce = graph$reduce(data = data, dimension = "bands",reducer = function(x) {
  B08 = x[1]
  B04 = x[2]
  B02 = x[3]
  (2.5 * (B08 - B04)) / sum(B08, 6 * B04, -7.5 * B02, 1)
})

temporal_reduce = graph$reduce(data = spectral_reduce,dimension = "temporal", reducer = function(x) {
  min(x)
})

apply_linear_transform = graph$apply(data=temporal_reduce,process = function(value) {
  graph$linear_scale_range(x = value, 
                           inputMin = -1, 
                           inputMax = 1, 
                           outputMin = 0, 
                           outputMax = 255)
})

final_node = graph$save_result(data=apply_linear_transform,format="PNG")
graph$setFinalNode(final_node)

# print as JSON
graph

# write to file 
cat(graphToJSON(graph),file = "r-evi-phenology-graph.json")

# client side graph validation
graph$validate()

validate_process_graph(con = con, graph = graph)

compute_result(con=con,graph=graph,format = "PNG",output_file = "gee_evi_example.png")

# old code snippets for the callback creations

# old callback creation - spectral reducer
# evi_graph = con %>% callback(spectral_reduce,parameter = "reducer")
# 
# nir = evi_graph$data$data %>% evi_graph$array_element(0)
# red = evi_graph$data$data %>% evi_graph$array_element(1)
# blue = evi_graph$data$data %>% evi_graph$array_element(2)
# 
# p1 = evi_graph$product(data = list(red, 6))
# p2 = evi_graph$product(data = list(blue, -7.5))
# 
# sub = evi_graph$subtract(data = list(nir,red))
# sum = evi_graph$sum(data = list(1,nir,p1,p2))
# div = evi_graph$divide(data = list(sub,sum))
# 
# p3 = evi_graph$product(data = list(2.5,div))
# 
# evi_graph$setFinalNode(p3)


# old callback - temporal reducer
# min_time_graph = con %>% callback(temporal_reduce, parameter = "reducer")
# min_time_graph$min(data = min_time_graph$data$data) %>% min_time_graph$setFinalNode()


# old callback - linear scaling
# cb2_graph = con %>% callback(apply_linear_transform, "process")
# cb2_graph$linear_scale_range(x = cb2_graph$data$x, inputMin = -1, inputMax = 1, outputMin = 0, outputMax = 255) %>% cb2_graph$setFinalNode()
