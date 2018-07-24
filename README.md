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

## Getting Started
After loading the package, you need to connect to the openeo backend you want to use. The object that is returned by the `connect` function is essential for the interaction with this particular backend. For example you can explore offered data and processes and explore their detailed information.
An important part of the openEO infrastructure is the creation of process graphs that are executed on the openEO backends. Process graphs are created in a bottom up principal using the `magrittr` pipes `%>%`. Starting with the data (`collection`) we use multiple processes that are provided by this backend to form a process chain. You can use this technique if you are already familiar with the choosen backend. As an alternative you can use the Process Graph Builder `pgb()` [see Wiki: Process Graph Building](https://github.com/Open-EO/openeo-r-client/wiki/Process-Graph-Building).

After we defined the process graph, we send it as a task to the backend where it is processed. It can be processed _immediately_ (the client is holding the connection and waits for the results), _directly asynchronous_ (the client sends the task and queues the process execution and the results are stored in the user workspace on the backend, meanwhile the client receives an acceptance notification).

```
library(openeo)
conn = connect(host="http://openeo.backend1.org/api/v0",user="test",password="test")

# list collection and processes
conn %>% listCollections()
conn %>% listProcesses()

# get detailed descriptions
conn %>% describeProduct(c("sentinel2","landsat7"))
conn %>% describeProcess("filter_daterange")

# create a process graph / task
task = collection("sentinel2") 
          %>% process("filter_daterange",from = "2017-07-21",to = "2017-07-28") 
          %>% process("filter_bbox", left=7.5, right= 8.5, bottom=51.0, top=52.0, crs="EPSG:4326") 
          %>% defineUDF(con = conn,
                  language = "R",
                  type = "apply_scene",
                  content = file.path("C:/files/atmo_corr.R"),
                  target = "udfs/atmospherical_correction.R")
                                
job_id = conn %>% defineJob(task)
conn %>% downloadJob(job_id)
```

## Additional Examples
If you are interested, you can have a look at some example scripts that were used during the Proof-of-Concept under [examples](https://github.com/Open-EO/openeo-r-client/tree/master/examples) to get a feeling, how to use the package.

## Links
* [openEO.org](http://openeo.org/)
* [openEO core API](https://open-eo.github.io/openeo-api/)
