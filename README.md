# openEO client in R

This R package contains functions and classes that allow interactions with openEO backend server. The package will be under constant development. The master branch should always contain a version that is ready to use.

## Installation
Install the package by using `install_github` from the devtools package.

```
if (!require(devtools)) {
  install.packages("devtools",dependencies=TRUE)
  library(devtools)
}
install_github(repo="Open-EO/openeo-r-client",dependencies=TRUE)
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
| v0.4.x | [v0.4.2](https://openeo.org/documentation/0.4/developers/api/reference.html) |
| v0.5.x | [v0.4.2](https://openeo.org/documentation/0.4/developers/api/reference.html) |
| v0.6.x | [v0.4.2](https://openeo.org/documentation/0.4/developers/api/reference.html) |
| v1.0.x | [v1.0.0](https://openeo.org/documentation/1.0/developers/api/reference.html) |

## Prerequirements

The openeo package won't process anything at the local machine. It will always interact with a designated back-end. Only at the back-end the data is stored and the computations are performed. Therefore please make sure you are registered with any of the available openEO back-ends, in order to obtain credentials and the access URLs (see: [openEO Hub](https://hub.openeo.org/) for getting an overview about available back-ends). 

## Getting Started
After loading the package, you need to connect to the openeo backend you want to use. The object that is returned by the `connect` function is essential for the interaction with this particular backend. For example you can explore offered data and processes and explore their detailed information.
To get started with the process graph creation, please [see Wiki: Process Graph Building](https://github.com/Open-EO/openeo-r-client/wiki/Process-Graph-Building) for further information.

After we defined the process graph, we send it as a task to the backend where it is processed. It can be processed _immediately_ (the client is holding the connection and waits for the results), _directly asynchronous_ (the client sends the task and queues the process execution and the results are stored in the user workspace on the backend, meanwhile the client receives an acceptance notification).

```
library(openeo)
conn = connect(host="http://backend1.openeo.org/",user="test",password="test",login_type="basic")

# list collection and processes
colls = list_collections()
list_processes()

# get detailed descriptions
describe_collection(c("sentinel2_subset","landsat7_ndvi"))
describe_process("filter_bbox")

# create a process graph / task
p = processes()

data = p$load_collection(id = colls$`COPERNICUS/S2`,
                             spatial_extent = list(
                               west=16.1,
                               east=16.6,
                               north=48.6,
                               south= 47.2
                             ),
                             temporal_extent = list(
                               "2018-04-01", "2018-05-01"
                             ),
                             bands=list("B8","B4","B2")))

spectral_reduce = p$reduce_dimension(data = data, dimension = "bands",reducer = function(data,context) {
  B08 = data[1]
  B04 = data[2]
  B02 = data[3]
  (2.5 * (B08 - B04)) / sum(B08, 6 * B04, -7.5 * B02, 1)
})

temporal_reduce = p$reduce_dimension(data=spectral_reduce,dimension = "t", reducer = function(x,y){
  p$min(x)
})

apply_linear_transform = p$apply(data=temporal_reduce,process = function(value,...) {
  p$linear_scale_range(x = value, 
                           inputMin = -1, 
                           inputMax = 1, 
                           outputMin = 0, 
                           outputMax = 255)
})

result = p$save_result(data=apply_linear_transform,format="PNG")
                                
job_id = create_job(graph=result, title="Example graph", description="This graph is just a general example",format="png")

start_job(job_id)

result_obj = list_results(job_id)

download_results(job = job_id, folder = ".")

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
The package documentation can be found here: <https://open-eo.github.io/openeo-r-client/index.html>

## Links
* [openEO.org](http://openeo.org/)
* [openEO core API](https://openeo.org/documentation/1.0/developers/api/reference.html)
* [openEO hub](https://hub.openeo.org/)
