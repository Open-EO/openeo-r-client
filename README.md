openeo: Client Interface for openEO Servers
====
<!-- badges: start -->
[![R-CMD-check](https://github.com/Open-EO/openeo-r-client/workflows/R-CMD-check/badge.svg)](https://github.com/Open-EO/openeo-r-client/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/openeo)](https://CRAN.R-project.org/package=openeo)
<!-- badges: end -->

<img src='man/figures/logo.png' align="right" height="116" />

# openEO Background

The amount of available Earth Observation data (EO data) is steadily increasing due to different space missions such as Landsat and Sentinel. Resulting data products are often too large to be processed locally and therefore require new processing tools and functionalities. The [core concept of "openEO"](https://openeo.org/about.html) is related to big data processing strategies. "openEO"" defines a unified API for back-end and client software as well as a number of common processes for manipulating spatio-temporal data cubes. The basic idea is to distinguish between computation (back-end server) and workflow definition (client software).
While some back-ends were developed in the main [openEO](https://openeo.org/) project, others were currently improved or developed within the ESA project [openEO Platform](https://openeo.cloud/). Those back-ends offer access to their data collections and processing platform while the client software (e.g., R, Python, JavaScript, QGIS) help creating processing workflows in a programming environment familiar to the user.

## openEO client in R

This R package contains functions and classes that allow interactions with openEO back-end server. The main goals of this package are:
* enable an R-user to explore openEO back-ends for distributed data and implemented operations.
* aid an R-user to create processing workflows on EO data executable on openEO back-ends.
* retrieve results for further analysis in R

# Installation

The most recent code is located on Github. To install it, you can use the following code:
```
if (!require(devtools)) {
  install.packages("devtools",dependencies=TRUE)
  library(devtools)
}
install_github(repo="Open-EO/openeo-r-client",dependencies=TRUE)
library(openeo)
```

Otherwise, once accepted by CRAN, you can install the latest stable version by:

```
install.packages("openeo")
library(openeo)

```

If you want use a different package version we recommend to use the parameter `ref`. Define this parameter as "master", "develop" or another version specified in [releases](https://github.com/Open-EO/openeo-r-client/releases).

Currently, the package complies to the major openEO API version 1.0.x. It is also possible to manually install older versions that comply to the API version 0.4.2. This is not recommended since most - if not all - back-ends won't support this version anymore. The old versions are stated here for historic reasons. Starting with the stable API version 1.0.0 the package will be backward compatible within the semantic versioning.

| openeo R client version | openEO API version | openEO API status |
| --- | --- | --- |
| not yet | [v1.1.x](https://openeo.org/documentation/1.0/developers/api/reference.html) | stable |
| v1.0.x | [v1.0.x](https://openeo.org/documentation/1.0/developers/api/reference.html) | stable |
| v0.6.x | [v0.4.2](https://openeo.org/documentation/0.4/developers/api/reference.html) | deprecated |
| v0.5.x | [v0.4.2](https://openeo.org/documentation/0.4/developers/api/reference.html) | deprecated |
| v0.4.x | [v0.4.2](https://openeo.org/documentation/0.4/developers/api/reference.html) | deprecated |

# Requirements

The 'openeo' package won't process anything on the local machine. It will always interact with a designated back-end. Data storage and computations are performed directly at the back-end. Therefore, please make sure that you are registered with any of the available openEO back-ends in order to obtain credentials and the access URLs (see: [openEO Hub](https://hub.openeo.org/) for getting an overview about available back-ends). 

# Getting Started

After installing and loading the package, you need to connect to the openEO back-end you want to use. The object returned by the `connect` function is essential for the interaction with this particular back-end. Afterwards, users can explore the data and processes and start creating a processing workflows, free of charge. To start processing data or publishing web services, however, the user needs to be registered and authenticated with the openEO back-end provider. The provider offers different execution plans the user can choose. These may include free-of-charge plans or other pricing concepts.

Exemplary back-end providers are:
* https://earthengine.openeo.org
* https://openeo.vito.be
* https://openeo.cloud

The Google Earth Engine (GEE) interface for openEO is not actively maintained. The credentials for accessing and testing are included in the demo section at the [openEO GEE Github repository](https://github.com/Open-EO/openeo-earthengine-driver). Please bear in mind that the access is free, but Google might revoke the rights if the processing load is too high. Use it only for playing around with the different openEO clients and not for productive purposes. "openeo.cloud" is the link to ESAs ["openEO Platform" project](https://openeo.cloud/), for which you have to be [signed up via EGI and openEO platform](https://docs.openeo.cloud/authentication/#join-openeo-platform).

# Examples
The following code sample shows how to create a processing workflow that calculates the minimum NDVI of a spatial and temporal subset on Sentinel-2 data and perform a linear scaling to store the results as PNG file. 

```
library(openeo)
connect(host="https://earthengine.openeo.org")

# list collection and processes
colls = list_collections()
list_processes()

# get detailed descriptions
describe_collection("COPERNICUS/S2")
describe_process("load_collection")

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
                             bands=list("B8","B4")))

spectral_reduce = p$reduce_dimension(data = data, dimension = "bands",reducer = function(data,context) {
  B08 = data[1]
  B04 = data[2]
  return((B08-B04)/(B08+B04))
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
```

At this point (latest here) you need to log in with your personal credentials for further computations.

```                                
login(user="",password="")
job_id = create_job(graph=result, title="Example graph", description="This graph is just a general example",format="png")

start_job(job_id)

result_obj = list_results(job_id)

download_results(job = job_id, folder = ".")

```

To get an overview of the functions offered by the packages and to access the function documentation you can navigate to the "Packages" tab in RStudio, select the "openeo" package and click on the function you are interested in. Another option is to use the following command line operations:

```R
library(help="openeo")

# ?<function_name>, e.g.
?connect
```

If you are interested in more information, you can have a look at some [example scripts](https://github.com/Open-EO/openeo-r-client/tree/master/examples) that were created during the Proof-of-Concept phase to get a feeling on how to use the package. Some of the scripts are outdated and will be replaced in the future.


# Funding
The authors acknowledge the financial support for the development of this package during the H2020 project "openEO" (Oct 2017 to Sept 2020) by the European Union, funded by call EO-2-2017: EO Big Data Shift, under grant number 776242. We also acknowledge the financial support received from ESA for the project "R4openEO" (Sept 2021 to Sept 2022). 

Furthermore, openEO was improved and operationalized by ESA funding in the project "openEO Platform" (Sept 2020 to Sept 2022).

# Links
* [openEO.org](https://openeo.org/)
* [openEO core API](https://openeo.org/documentation/1.0/developers/api/reference.html)
* [openEO hub](https://hub.openeo.org/)
* [openEO R Client package API](https://open-eo.github.io/openeo-r-client/)
