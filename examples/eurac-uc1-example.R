# RClient v0.6.0 -> EURAC version: 0.4.2

library(magrittr)
library(openeo)
library(tibble)

# enter valid credentials
euracHost = ""
user = ""
password = ""

eurac = connect(host = euracHost, user = user,password = password, login_type = "basic")
list_processes(con = eurac)
capabilities(con = eurac)
list_file_types(con = eurac)

descriptions = list_collections(con = eurac)

describe_collection(con = eurac, id="openEO_S2_32632_10m_L2A")

describe_process(con = eurac,"load_collection")
describe_process(con = eurac,"ndvi")
describe_process(con = eurac,"min_time")

# Build a process graph: Maximum NDVI with a linear scale into a png file
graph = process_graph_builder(con = eurac)

data1 = graph$load_collection(id = graph$data$openEO_S2_32632_10m_L2A, 
                              spatial_extent = list(west = 11.2792, 
                                                    south = 46.4643, 
                                                    east = 11.4072, 
                                                    north = 46.5182), 
                              temporal_extent = c("2018-06-04T00:00:00Z","2018-06-23T00:00:00Z"))

ndvi = data1 %>% graph$ndvi()
max_t = ndvi %>% graph$max_time()

apply_linear_transform = graph$apply(data = max_t, process = function(value) {
  graph$linear_scale_range(x = value, 
                           inputMin = -1, inputMax = 1, 
                           outputMin = 0, outputMax = 255)
})

final = graph$save_result(data = apply_linear_transform, format = "PNG") 

graph$setFinalNode(node = final)


compute_result(con = eurac, graph = graph, format="GTiff",output_file = "eurac_test.png")


job_id = create_job(con = eurac,
                    graph = graph,
                    title = "UC1 Graph from R client")

start_job(con = eurac, job = job_id)

describe_job(con = eurac, job = job_id)

list_results(con = eurac, job=job_id)

download_results(con = eurac,job = job_id,folder = "./eurac_test/")


# Create a process graph calculating maximum NDVI but download as GTiff

graph2 = process_graph_builder(con = eurac)

data2 = graph2$load_collection(id = graph2$data$openEO_S2_32632_10m_L2A, 
                              spatial_extent = list(west = 11.2792, 
                                                    south = 46.4643, 
                                                    east = 11.4072, 
                                                    north = 46.5182), 
                              temporal_extent = c("2018-06-04T00:00:00Z","2018-06-23T00:00:00Z"))


ndvi = graph2$ndvi(data = data2)
max_t = graph2$max_time(data = ndvi)
final = graph2$save_result(data = max_t,format = "GTiff")

graph2$setFinalNode(node = final)


compute_result(con = eurac,graph=graph2, format="GTiff",output_file = "eurac_test.tif")

