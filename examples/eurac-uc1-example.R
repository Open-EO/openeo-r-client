# RClient -> EURAC version: 0.4.2


library(openeo)

# enter valid credentials
euracHost = "https://openeo.eurac.edu"
user = NA
password = NA

eurac = connect(host = euracHost, user = user,password = password, login_type = "basic")
eurac %>% list_processes()
eurac %>% capabilities()
eurac %>% list_file_types()

descriptions = eurac %>% list_collections()

eurac %>% describe_collection(id="openEO_S2_32632_10m_L2A")

eurac %>% describe_process("load_collection")
eurac %>% describe_process("ndvi")
eurac %>% describe_process("min_time")

# Build a process graph using the ProcesGraphBuilder pgb()
graph = eurac %>% process_graph_builder()

data1 = graph$load_collection(id = graph$data$openEO_S2_32632_10m_L2A, 
                              spatial_extent = list(west = 11.2792, 
                                                    south = 46.4643, 
                                                    east = 11.4072, 
                                                    north = 46.5182), 
                              temporal_extent = c("2018-06-04T00:00:00Z","2018-06-23T00:00:00Z"))

ndvi = data1 %>% graph$ndvi()
max_t = ndvi %>% graph$max_time()

apply_linear_transform = eurac %>% graph$apply(data = max_t)

cb2_graph = eurac %>% callback(apply_linear_transform, "process")

cb2_graph$linear_scale_range(x = cb2_graph$data$x, 
                             inputMin = -1, inputMax = 1, 
                             outputMin = 0, outputMax = 255) %>% cb2_graph$setFinalNode()

apply_linear_transform %>% graph$save_result(format = "GTiff") %>% graph$setFinalNode()

# eurac %>%  create_job(graph)

eurac %>% compute_result(graph, format="GTiff",output_file = "eurac_test.tif")

job_id = eurac %>% create_job(graph,title = "UC1 Graph from R client")

eurac %>% start_job(job_id)

eurac %>% describe_job(job_id)

eurac %>% list_results(job=job_id)

eurac %>% download_results(job = job_id,folder = "./eurac_test/")

