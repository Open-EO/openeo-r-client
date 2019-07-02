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
install_github(repo="Open-EO/openeo-r-client",ref="v0.2.2",dependencies=TRUE)
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
| v0.3.1 (not yet released) | [v0.3.1](https://open-eo.github.io/openeo-api/v/0.3.1/) |


## Getting Started
After loading the package, you need to connect to the openeo backend you want to use. The object that is returned by the `connect` function is essential for the interaction with this particular backend. For example you can explore offered data and processes and explore their detailed information.
An important part of the openEO infrastructure is the creation of process graphs that are executed on the openEO backends. Process graphs are created in a bottom up principal using the `magrittr` pipes `%>%`. Starting with the data (`collection`) we use multiple processes that are provided by this backend to form a process chain. You can use this technique if you are already familiar with the choosen backend. As an alternative you can use the Process Graph Builder `pgb()` [see Wiki: Process Graph Building](https://github.com/Open-EO/openeo-r-client/wiki/Process-Graph-Building).

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

describeCollection
# create a process graph / task
pgb = con %>% pgb()

task = pgb$collection$sentinel2 %>%
          pgb$filter_daterange(extent = c("2017-07-21","2017-07-28")) %>%
          pgb$filter_bbox(extent = list(west=7.5, east= 8.5, south=51.0, north=52.0, crs="EPSG:4326")) %>%
          pgb$NDVI(nir="B8",red = "B4") %>%
          pgb$min_time()
                                
job_id = conn %>% defineJob(task=task, title="Example graph", description="This graph is just a general example",format="GTiff")

result_obj = conn %>% listResults(job_id)
urls = sapply(result_obj$link,function(link)link$href)

# download the urls as you like
```
To get an overview which functions the packages offers and to access the function documentation you can either navigate in RStudio into the "Packages" tab and select the "openeo" package and click on the function you are interested in. Or you can use the following command line operations:

```R
library(help="openeo")

# ?<function_name>, e.g.
?connect
```

## Additional Examples
If you are interested, you can have a look at some example scripts that were used during the Proof-of-Concept under [examples](https://github.com/Open-EO/openeo-r-client/tree/master/examples) to get a feeling, how to use the package.

## Links
* [openEO.org](http://openeo.org/)
* [openEO core API](https://open-eo.github.io/openeo-api/)
