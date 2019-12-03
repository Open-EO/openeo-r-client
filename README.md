# openEO client in R

[![Status](https://img.shields.io/badge/Status-proof--of--concept-yellow.svg)]()

This R package contains functions and classes that allow interactions with openEO backend server. The package will be under constant development. The master branch should always contain a version that is ready to use. In the future we will start to use tagged releases, when we reach a sufficient number of implemented features.

## Installation
Install the package by using `install_github` from the devtools package.

```
if (!require(devtools)) {
  install.packages("devtools",dependencies=TRUE)
  library(devtools)
}
install_github(repo="Open-EO/openeo-r-client",ref="v0.5.0",dependencies=TRUE)
library(openeo)
```

If you want use a different version, then use for the parameter `ref` either "master", "develop" or another version specified under [releases](https://github.com/Open-EO/openeo-r-client/releases).

Since the openEO project is under heavy development regarding the openeo API that connects clients and backends, all R client versions < 1.0.0 versions will only be compatible with a certain API version.

| openeo R client version | openeo API version |
| --- | --- |
| v0.0.1 | [v0.0.1](https://open-eo.github.io/openeo-api/v/0.0.1/) |
| 0.1.0 | [v0.0.1](https://open-eo.github.io/openeo-api/v/0.0.1/) |
| v0.2.0-poc | [v0.0.2](https://open-eo.github.io/openeo-api/v/0.0.2/) |
| v0.2.2 | [v0.0.2](https://open-eo.github.io/openeo-api/v/0.0.2/) |
| v0.3.1 | [v0.3.1](https://open-eo.github.io/openeo-api/v/0.3.1/) |
| v0.4.x | [v0.4.2](https://open-eo.github.io/openeo-api/v/0.4.2/) |
| v0.5.x | [v0.4.2](https://open-eo.github.io/openeo-api/v/0.4.2/) |


## Getting Started
After loading the package, you need to connect to the openeo backend you want to use. The object that is returned by the `connect` function is essential for the interaction with this particular backend. For example you can explore offered data and processes and explore their detailed information.
Starting with API version 0.4.x the process graph creation has drastically changed. To get insights on the process graph building, please [see Wiki: Process Graph Building](https://github.com/Open-EO/openeo-r-client/wiki/Process-Graph-Building).

After we defined the process graph, we send it as a task to the backend where it is processed. It can be processed _immediately_ (the client is holding the connection and waits for the results), _directly asynchronous_ (the client sends the task and queues the process execution and the results are stored in the user workspace on the backend, meanwhile the client receives an acceptance notification).

```
library(openeo)
conn = connect(host="http://backend1.openeo.org/",user="test",password="test",login_type="basic")

# list collection and processes
conn %>% list_collections()
conn %>% list_processes()

# get detailed descriptions
conn %>% describe_collection(c("sentinel2_subset","landsat7_ndvi"))
conn %>% describe_process("filter_bbox")

# create a process graph / task
graph = conn %>% process_graph_builder()

data1 = graph$load_collection(id = graph$data$`COPERNICUS/S2`,
                              spatial_extent = list(west=-2.7634,south=43.0408,east=-1.121,north=43.8385),
                              temporal_extent = c("2018-04-30","2018-06-26"),bands = c("B4","B8"))
b4 = graph$filter_bands(data = data1,bands = "B4")
b8 = graph$filter_bands(data=data1,bands = "B8")

ndvi = graph$normalized_difference(band1 = b4,band2 = b8)

reducer = graph$reduce(data = ndvi,dimension = "temporal")

cb_graph = conn %>% callback(reducer,parameter = "reducer")

cb_graph$min(data = cb_graph$data$data) %>% cb_graph$setFinalNode()


apply_linear_transform = graph$apply(data = reducer)

cb2_graph = conn %>% callback(apply_linear_transform, "process")

cb2_graph$linear_scale_range(x = cb2_graph$data$x, inputMin = -1, inputMax = 1,outputMin = 0,outputMax = 255) %>% 
  cb2_graph$setFinalNode()

graph$save_result(data = apply_linear_transform,format = "png") %>% graph$setFinalNode()

                                
job_id = conn %>% create_job(graph=graph, title="Example graph", description="This graph is just a general example",format="png")

conn %>% start_job(job_id)

result_obj = conn %>% list_results(job_id)

conn %>% download_results(job = job_id, folder = ".")

```
To get an overview which functions the packages offers and to access the function documentation you can either navigate in RStudio into the "Packages" tab and select the "openeo" package and click on the function you are interested in. Or you can use the following command line operations:

```R
library(help="openeo")

# ?<function_name>, e.g.
?connect
```

## Additional Examples
If you are interested, you can have a look at some example scripts that were used during the Proof-of-Concept under [examples](https://github.com/Open-EO/openeo-r-client/tree/master/examples) to get a feeling, how to use the package. Some of the scripts are outdated and will be replaced in the future.

## Further Information
The [Wiki](https://github.com/Open-EO/openeo-r-client/wiki) contains also additional information on process graph building and other topics.

## Links
* [openEO.org](http://openeo.org/)
* [openEO core API](https://open-eo.github.io/openeo-api/)
