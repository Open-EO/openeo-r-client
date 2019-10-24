# RClient -> EURAC version: 0.4.2

library(magrittr)
library(openeo)
library(tibble)

# enter valid credentials
euracHost = ""
user = ""
password = ""

eurac = connect(host = euracHost, user = user,password = password, login_type = "basic")
eurac %>% list_processes()
eurac %>% capabilities()
eurac %>% list_file_types()

descriptions = eurac %>% list_collections()

eurac %>% describe_collection(id="openEO_S2_32632_10m_L2A")

eurac %>% describe_process("load_collection")
eurac %>% describe_process("ndvi")
eurac %>% describe_process("min_time")

# Build a process graph: Maximum NDVI with a linear scale into a png file
graph = eurac %>% process_graph_builder()

data1 = graph$load_collection(id = graph$data$openEO_S2_32632_10m_L2A, 
                              spatial_extent = list(west = 11.2792, 
                                                    south = 46.4643, 
                                                    east = 11.4072, 
                                                    north = 46.5182), 
                              temporal_extent = c("2018-06-04T00:00:00Z","2018-06-23T00:00:00Z"))

ndvi = data1 %>% graph$ndvi()
max_t = ndvi %>% graph$max_time()

apply_linear_transform = graph$apply(data = max_t)

cb2_graph = callback(apply_linear_transform, "process")

cb2_graph$linear_scale_range(x = cb2_graph$data$x, 
                             inputMin = -1, inputMax = 1, 
                             outputMin = 0, outputMax = 255) %>% cb2_graph$setFinalNode()

apply_linear_transform %>% graph$save_result(format = "PNG") %>% graph$setFinalNode()


eurac %>% compute_result(graph, format="GTiff",output_file = "eurac_test.png")


job_id = eurac %>% create_job(graph,title = "UC1 Graph from R client") # Location header is not available -> breaks the client

eurac %>% start_job(job_id)

eurac %>% describe_job(job_id)

eurac %>% list_results(job=job_id)

eurac %>% download_results(job = job_id,folder = "./eurac_test/")

# Create a process graph calculating maximum NDVI but download as GTiff

graph2 = eurac %>% process_graph_builder()

data2 = graph2$load_collection(id = graph2$data$openEO_S2_32632_10m_L2A, 
                              spatial_extent = list(west = 11.2792, 
                                                    south = 46.4643, 
                                                    east = 11.4072, 
                                                    north = 46.5182), 
                              temporal_extent = c("2018-06-04T00:00:00Z","2018-06-23T00:00:00Z"))

data2 %>% 
  graph2$ndvi() %>% 
  graph2$max_time() %>% 
  graph2$save_result(format = "GTiff") %>% 
  graph2$setFinalNode()


eurac %>% compute_result(graph2, format="GTiff",output_file = "eurac_test.tif")

