# openEO client in R

[![Status](https://img.shields.io/badge/Status-proof--of--concept-yellow.svg)]()

This R package contains functions and classes that allow interactions with openEO backend server. The package will be under constant development. The master branch should always contain a version that is ready to use. In the future we will start to use tagged releases, when we reach a sufficient number of implemented features.

## Installation
Install the package by using `install_github` from the devtools package.

```
library(devtools)
install_github(repo="Open-EO/openeo-r-backend",ref="master")
install_github(repo="Open-EO/openeo-r-client",ref="master")
```

Currently this package depends on the [openeo-r-backend](https://github.com/Open-EO/openeo-r-backend) for shared data models. The installation of the 'openeo-r-backend' will take some time, unfortunately. In the future the models will be stored in a separate package that will be much smaller. 

## Getting Started
After loading the package, you need to connect to the openeo backend you want to use. The object that is returned by the `connect` function is essential for the interaction with this particular backend. For example you can explore offered data and processes and explore their detailed information.
An important part of the openEO infrastructure is the creation of process graphs that are executed on the openEO backends. Process graphs are created in a bottom up principal using pipes `%>%`. Starting with the data (`collection`) we use multiple processes that are provided by this backend to form a process chain. After we defined the process graph, we send it as a task to the backend where it is processed. It can be processed _immediately_ (the client is holding the connection and waits for the results), _directly asynchronous_ (the client sends the task and queues the process execution and the results are stored in the user workspace on the backend, meanwhile the client receives an acceptance notification) and _lazy asynchronous_ (task is send and stored and executed on demand, e.g. when it needs to be downloaded).

```
library(openeo)
conn = connect(host="http://openeo.backend1.org/api/v0",user="test",password="test")

# list collection and processes
conn %>% listCollections()
conn %>% listProcesses()

# get detailed descriptions
conn %>% describe(product_id = c("sentinel2","landsat7"))
conn %>% describe(process_id = "filter_daterange")

# create a process graph / task
task = collection("sentinel2") %>% process("filter_daterange",from = "2017-07-21",to = "2017-07-28") %>% process("filter_bbox", left=7.5, right= 8.5, bottom=51.0, top=52.0, crs="EPSG:4326") %>% defineUDF(con = conn,
                                language = "R",
                                type = "apply_scene",
                                content = file.path("C:/files/atmo_corr.R"),
                                target = "udfs/atmospherical_correction.R")
                                
conn %>% queueTask(task)
```

Notes: if you are running the openeo-r-backend as backend solution for testing, then please use the optional parameter `rbackend=TRUE`, when calling `connect`. The current openeo-r-backend differs from the strict openeo API in terms of some trailing slashes "/". This is adressed when setting `rbackend=TRUE`. Alternatively set `conn$is_rserver = TRUE` after `connect`.

## Links
* [openEO.org](http://openeo.org/)
* [openEO core API](https://open-eo.github.io/openeo-api-poc/)
