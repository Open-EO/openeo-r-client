# POC: Use Case 1
# RClient -> GEE back-end
# v0.4.1
user = "group8"
pwd = "test123"

# 1. Requesting the API versions available at the back-end
gee_host_url = "https://earthengine.openeo.org"
api_versions(gee_host_url)

gee = connect(host = gee_host_url, version="0.4.1",user = user,password = pwd,login_type = "basic")
# also inserting the direct link is possible
# gee = connect(host = "https://earthengine.openeo.org/v0.4",user = user,password = pwd)

# 2. Requesting the capabilities of the back-end
gee %>% capabilities()

# 3. Check which collections are available at the back-end
gee %>% list_collections()

graph = gee %>% process_graph_builder()
data1 = graph$load_collection(id = graph$data$`COPERNICUS/S2`,
                              spatial_extent = list(west=-2.7634,south=43.0408,east=-1.121,north=43.8385),
                              temporal_extent = c("2018-04-30","2018-06-26"),bands = c("B4","B8"))
b4 = graph$filter_bands(data = data1,bands = "B4")
b8 = graph$filter_bands(data=data1,bands = "B8")

ndvi = graph$normalized_difference(band1 = b4,band2 = b8)

reducer = graph$reduce(data = ndvi,dimension = "temporal")

# gee %>% callback(reducer)
cb_graph = gee %>% callback(reducer,parameter = "reducer")

cb_graph$min(data = cb_graph$data$data) %>% cb_graph$setFinalNode()


apply_linear_transform = gee %>% graph$apply(data = reducer)

cb2_graph = gee %>% callback(apply_linear_transform, "process")

cb2_graph$linear_scale_range(x = cb2_graph$data$x, inputMin = -1, inputMax = 1,outputMin = 0,outputMax = 255) %>% 
  cb2_graph$setFinalNode()

graph$save_result(data = apply_linear_transform,format = "png") %>% graph$setFinalNode()

graph

graph$validate()

gee %>% validate_process_graph(graph=graph)

job_id = gee %>% create_job(task=graph,title="Job build in R-Client")

gee %>% list_jobs()

gee %>% start_job(job = job_id)

gee %>% describe_job(job_id)

results = gee %>% list_results(job_id)
results = gee %>% list_results("VrZwXc1IsCcY1OeU")


download.file(results$links[[1]]$href, "test.png",mode = "wb")