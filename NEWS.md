# Version 1.4.0

## Added

* Support for the OpenID Connect Client Credentials flow
* compute_result, create_job, update_job, create_service and update_service have an additional parameter to inject custom properties into the request (e.g., to set memory or CPU limits)
* Implement the datacube subtype through the new DataCube class (which RasterCube and VectorCube inherit from now)

## Changed

* OpenID Connect scopes can be provided as lists
* changed authentication setup for OIDC to instantiate a workflow object with a preconfigured `Provider` class

## Fixed

* `describe_collection` and `collection_viewer` accept string IDs again
* fixed issues when creating an `Argument` without schema
* replaced `is.na()` with `rlang::is_na()` in several occurrences in the `$typeCheck()` functions of argument classes, because is.na on lists and objects returns more than one result
* fixed `Number` validation with Argument `ProcessNode`

# Version 1.3.1

## Added
* internal functions to serialize and load the session information like connection, process collection, etc.

## Changed
* changed OIDC provider and default client selection
  * automatically selects first OIDC provider withe `default_clients` defined [#142](https://github.com/Open-EO/openeo-r-client/issues/142),
  * automatically selects a default client by the its id containing "openeo" at first, otherwise the first default client in the list is selected [#146](https://github.com/Open-EO/openeo-r-client/issues/146)
* argument bounding box tries to extract the EPSG code it EPSGCode was provided as WKT2
* compute_result now looks for a format in "save_result" if it was not stated in the call

## Fixes
* CollectionId argument now checks if a string pattern is available
* OutputFormat argument checks the "FileFormat" class separately to avoid warnings
* fixed an issue where it was not possible to connect to an openEO back-end with a dedicated link (also for reconnect in the connection contract)
* fixed warning messages shown on `describe_collection()` using an object of class `Collection` from `list_collection()` [#145](https://github.com/Open-EO/openeo-r-client/issues/145)
* .create_sample_bbox interpretes now correctly a serialized extent object
* .find_process_by_name is no longer giving a false message when a process is passed to it

# Version 1.3.0

## Added
* new vignettes:
  - Package Software Architecture
  - Process Graph Building Concepts
  - Process Graph Building Application
  - Developer Implementation Details
* added the default openEO processes v1.2.0 as test data to the package in the RDS file  format (`system.file("openeo_processes/openeo_process_1.2.0.rds")`) and added a test case for process parsing
* `active_process_list()` and `active_process_collection()` which refer to the currently connected back-ends process list as well as the package interpretation of those process descriptions. See environment variables `process_list` and `process_collection`
* `active_data_collection()` to fetch the data collection of the currently active back-end, which refers to the package variable `data_collection`
* vignette about the software and package architecture and design choices based on API requirements

## Changed
* added line breaks if there are multiple validation messages of ProcessNodes
* `describe_process()` also works now with `Process` and `ProcessNode` objects
* use the active process list when looking up the description of an openEO process
* package now depends on R > 3.5.0, because of RDS object de-/serialization (openEO test processes)
* added a warning and a note for `compute_result()` if neither parameter 'format' of the function was set nor process 'save_result' was used in the process graph
* refresh data collections after separate login as part of [#83](https://github.com/Open-EO/openeo-r-client/issues/83)
* improved disconnection behavior by adding a `disconnect()` function and a logout and disconnect on package unload
* decoupled processes and data collection from the OpenEO connection
* moved the setup of the RStudio connection observer into a separate helper function
* naming of the vignettes to achieve an order in the vignette index entries

## Fixes
* `variables()` function now returns the extracted variables correctly
* coercion problems from checking with is.na on values with length > 1 have been fixed by using rlang::is_na [#130](https://github.com/Open-EO/openeo-r-client/issues/130), [#123](https://github.com/Open-EO/openeo-r-client/issues/123)
* an issue when calling the JSON serialization of class Process

## Removed
* removed utility functions `.getProcessCollection()` and `.find_var_in_stack()` which are replaced by the `active_process_collection()` function
* functions to call data collection (`$getCollectionNames()`, `$getDataCollection()`) from connection as well as the fields (`private$data_collection`)
* functions (`$getProcessCollection()`) and fields (`private$process_collection`, `$processes`) to store the process list and process collection
  

# Version 1.2.2

## Changed
* allowed objects as values for argument BoundingBox where `st_bbox()` can be applied
* very minor refinement in argument UdfCode and added test case [#132](https://github.com/Open-EO/openeo-r-client/issues/132)
* now throws a more readable error when the authentication provider is missing in the wellknown document

## Fixes
* OIDC authentication on remote machines prints now correctly URL and device code after fixes in httr2 package, now version 0.2.2 or higher is required (@m-mohr, @flahn, [#131](https://github.com/Open-EO/openeo-r-client/pull/131), [#119](https://github.com/Open-EO/openeo-r-client/pull/119))
* remote environments now login with `rlang::is_interactive() == FALSE` so that for example Jupyter environments print the device code URL and the code instead of trying to open the systems internet browser [#128](https://github.com/Open-EO/openeo-r-client/issues/128)
* capabilities request no longer appends an additional "/" to the host, which caused problems with some back-ends (@m-mohr [#133](https://github.com/Open-EO/openeo-r-client/pull/133), [#134](https://github.com/Open-EO/openeo-r-client/issues/134))
* `download_results` could not interprete a Job object due to the `length(job) != 1` restriction
* added a Job to Process coercion to match the messages instruction, when printing a Job [#115](https://github.com/Open-EO/openeo-r-client/issues/115)
* during JSON serialization numbers were truncated to a certain amount of digits. Setting `digits=NA` during serialization removes the default value for the digits [#64](https://github.com/Open-EO/openeo-r-client/issues/64)
* fixed parameter in logout query (req -> .req)

# version 1.2.1

## Added
* allowed a Process object to be passed to `validate_process`

## Changed
* not serializing basic `Argument` if not required, which affects mostly the "context" parameter of reducer functions
* skip local validation of `Array` type if there is more than one sub type allowed
* adapted GeoJSON coordinate order, now urn:ogc:def:crs:OGC::CRS84 is used as CRS, which means longitude / latitude order

## Fixes
* compute_result now correctly swaps the file format from save_result, if it was already used in the process graph [#124](https://github.com/Open-EO/openeo-r-client/issues/124)
* fixed messages thrown when validating arrays with multiple different allowed element types
* fixed missing NA / NULL setting in GeoJSON argument
* fixed missing brackets in coercion from Process to Graph



# version 1.2.0

## Added
* implemented a method for sample data retrieval (`get_sample()`), which will modify a process graph by replacing the original spatial extent of the request with a smaller subset of 0.0003 degrees extent [#94](https://github.com/Open-EO/openeo-r-client/issues/94)
* added a vignette explaining sample data retrieval
* rendering of classes as HTML widgets using the openEO Vue components, which can be used in Jupyter Notebook, Rmarkdown, RStudio and knitr
* implemented the argument type `metadata-filter` in order to filter collections based on a list of functions mostly applied in `load_collection` [#102](https://github.com/Open-EO/openeo-r-client/issues/102)
* implemented an abstract OIDC authentication
* implemented OIDC authentications for the following grant types:
  - authorization_code
  - authorization_code+pkce
  - urn:ietf:params:oauth:grant-type:device_code+pkce
  - urn:ietf:params:oauth:grant-type:device_code [#116](https://github.com/Open-EO/openeo-r-client/issues/116)
* temporarily added changed code to realize device_code+pkce from the `httr2` package until the `httr2` package changes are made available on CRAN
  - PR at httr2 [r-lib/httr2#109](https://github.com/r-lib/httr2/pull/109)
* overloaded function `toJSON` for `Process` and `Graph`
  - `processToJSON` to easily convert a process to JSON
  - `processToJSON` and `graphToJSON` are marked as deprecated

## Fixes
* `process_viewer` for user defined processes [#110](https://github.com/Open-EO/openeo-r-client/issues/110)

## Changed
* `compute_result()` will set or override existing `save_result` node entries when parameter `format` is provided [#62](https://github.com/Open-EO/openeo-r-client/issues/62)
* `compute_result()` will return a `stars` object if the parameter `as_stars` is set TRUE [#39](https://github.com/Open-EO/openeo-r-client/issues/39)
* `download_results()` uses httr2 functions instead of `download.file()` [#108](https://github.com/Open-EO/openeo-r-client/issues/108), also parameter `job` is allowed to be a ResultList or AssetList
* `timeout` parameter in `logs()` is now optional. If omitted 60s timeout is used for active batch processes and enabled services [#109](https://github.com/Open-EO/openeo-r-client/issues/109)
* replaced package `httr` by `httr2` to include the device_code and PKCE authentication methods
* `connect` no longer carries the login parameters separately, but uses `...` to pass on those information
* added 'kableExtra' as suggest to render the tables prettier in knitr

## Removed
* login parameter `login_type` removed. type will be deduced based on the other parameters.

# version 1.1.1

## Added
* in addition to integer it is now allowed to state text as input for argument `EPSGCode` like *EPSG:4326* [#99](https://github.com/Open-EO/openeo-r-client/issues/99) and wrote a test case for that
* Started test case for `OpenEOClient`
* Test case for `Boolean` argument validation

## Fixes
* fixed an issue with token refreshment, where the expiry time was not set correctly [#101](https://github.com/Open-EO/openeo-r-client/issues/101)
* fixed an issue where the host URL was not recognized and treated missing [#100](https://github.com/Open-EO/openeo-r-client/issues/100)
* fixed a validation problem of the Boolean argument when a `ProcessNode` was passed as a value
* fixed a problem with the overloaded R operators for `|`, `&` and `xor` where an old signature of the corresponding openEO process was used

# version 1.1.0
Bugfixes and preparation for CRAN release, 'openEO API' version 1.1.0

## Added
* added a vignette for getting started [#81](https://github.com/Open-EO/openeo-r-client/issues/81)

## Changes
* clarified the currently used OIDC code flow and adapted documentation [#71](https://github.com/Open-EO/openeo-r-client/issues/71)
* usability improvements when selecting and configuring the OIDC Authentication provider [#74](https://github.com/Open-EO/openeo-r-client/issues/74)
* cleaning empty fields when serializing user defined process results [#68](https://github.com/Open-EO/openeo-r-client/issues/68)
* changes related to UDP visualization: prior the process graph was visualized as JSON, which caused trouble, when exchanging JSON code between different clients. Now you can use `as(x,"Process")` to get a valid process from the result node [#54](https://github.com/Open-EO/openeo-r-client/issues/54)
* changed the printing output of User defined processes and services to not show the pure process graph anymore, use as(x,"Process") in that case
* documentation changes to discourage the use of `as(x,"Graph")` [#68](https://github.com/Open-EO/openeo-r-client/issues/68)
* allowed to modify service and UDP by passing a modified process object
* fixed NA checks when updating a process graph in an UDP, which threw irritating warnings
* modified documentation (Readme, function documentation)

## Fixes
* modified a descriptive text for "production-readiness" of back-ends
* modified the documentation and examples of `connect()` in terms of the parameter `version`
* modified the implemented openEO API version of the client (removed the release candidate suffix)
* fixed interpretation of an one entry discovery object [#59](https://github.com/Open-EO/openeo-r-client/issues/59)
* fixed an issue when querying the versions of a back-end [#72](https://github.com/Open-EO/openeo-r-client/issues/72)
* Updated to latest version of Vue Components and Processes 'DocGen' (which importantly fixes the collection viewer) [#75](https://github.com/Open-EO/openeo-r-client/issues/75)
* fixed missing enum validation [#65](https://github.com/Open-EO/openeo-r-client/issues/65)
* fixed process parsing of processes with completely nullable arrays [#79](https://github.com/Open-EO/openeo-r-client/issues/79)
* fixed a problem with NULL values set as value for AnyOf, the traversing of the graph did not work properly, because of a NULL-pointer issue
* fixed a parsing problem, where the "optional" field was not correctly interpreted in every case
* fixed stale URLs in documentation [#84](https://github.com/Open-EO/openeo-r-client/issues/84)

## Removed
* deleted some out-dated example scripts
* removed links to Github Wiki, because this will be discontinued soon and moved to package vignettes

# version 1.0.0

In general this release will cover the changes between API versions 0.4.2 and 1.0.0 without backward compatibility. Changes in the data models due to the new API are not listed here. 

Note: similarly named functions that work on different objects are abbreviated by a `*`, e.g. `describe_*()` for `describe_job()`, `describe_service()`, ...

## Added
* added a serialization of object `Parameter` into a descriptive form
* added a function to parse and coerce an R function into a `Graph` (e.g. `as(x,"Graph")`, where x is a R function returning a `ProcessNode`)
* new functions `privacy_policy()` and `terms_of_service()` to view the privacy policy and the terms of service
* function `conformance()` operated on the new corresponding API endpoint
* added class `ProcessCollection` in order to divide the Graph object and the graph building. It is created upon connection and obtainable via `processes()`.
* added a function `logs()`, `log_job()` and `log_service()` to request the back-end logs of a job or a service. The logging blocks the console and requests every second an update of the log at the back-end. Also when queuing a job via `start_job()` an immediate logging can be triggered.
* added S3 class labels, coerce and print functions for some exchange object:
   - `FileFormatList` (`as.data.frame.FileFormaList`)
   - `FileFormat` (`print.FileFormat`)
   - `ServiceType` (`print.ServiceType`)
   - `ServiceList` (`as.data.frame.ServiceList`, `print.ServiceList`)
   - `Service` (`print.Service`)
   - `CollectionList`
   - `Collection` (`print.Collection`)
   - `Log` (`print.Log`)
   - `Job` (`print.Job`)
   - `JobList` (`print.JobList`)
   - `ResultList` (`print.ResultList`)
   - `AssetList` (`as.data.frame.AssetList`)
   - `ProcessInfo` (`as.Graph.ProcessInfo`)
   - `UdfRuntime` (`as.character.UdfRuntime`)
   - `UdfRuntimeList`
   - `CubeDimension` (`as.character.CubeDimension`)
   - `CubeDimensions` (`print.CubeDimensions`)
   - `ProcessInfo` (`as.Process.ProcessInfo`)
* implemented the RStudio connection interface which shows the active connection and the available data sets with their dimensional description
* added classes for arguments: `UdfRuntimeArgument`, `UdfRuntimeVersionArgument`, `UdfCodeArgument`
* implemented and added a class `ArgumentList` to `Process` in order to allow an easier value assignment, e.g. `node1$parameter$x = ...`, which comes in handy, when the node is assigned to a variable and a parameter has to changed later
* added a builder for user-defined processes (`UserProcessCollection` and `user_processes()`) like `ProcessCollection` for predefined processes, which offers a users stored process graph as a usable function that creates a `ProcessNode`
* added function `status` for the connection, jobs and services to immediate get the status information. It also always queries the back-end for an update on call
* added ... parameter to the request, PUT and POST function to control the JSON serialization in `toJSON` (e.g. especially when stating the number of digits to use during this serialization)
* also enabled the ... parameter for functions where a process (graph) is sent to the back-end

## Fixed
* improved reading of files [#50]
* switching the connection in RStudio connection panel now correctly updates the panel
* fixed a problem with trailing slashes when setting the host URL in the connection or reading it from the well-known document
* fixed a problem with environments (mostly R6 classes) in the validate function of some arguments

## Changed
* added more setter functions to `Parameter` parent class in order to manipulate parameter representations later, when defining Parameters in user defined processes
* `ProcessGraph` as a parameter requires a function wrapping - meaning you should not pass an openEO process like `p$min` directly, because with the elaborated `ProcessGraphParameter` we cannot do a reliable automatic variable matching, e.g. often the `ProcessGraphParameter` "context" is misinterpreted
* updated and externalized the scripts in the `*_viewer()` functions to visualize collection and processes following the new api definition
* 'anyOf' was dropped as a top-level parameter definition schema in the API. The parameter `anyOf` is still be used in the client.
* classes were renamed:
   - `Callback` was renamed into `ProcessGraphArgument` (Graph is still a different Concept), which is used in reducer or aggregation functions
   - `callback-value` was renamed into `ProcessGraphParameter`
* unset `ProcessGraphParameter` are derived automatically from R functions that are passed as `ProcessGraphArgument` (former `Callback`) and they become parameters that shall be set by a user upon use of this user defined graph
* updated the endpoint file for version 1.0.0
* adapted the exchange models (JSON models) and their parsing / serialization to the new API accordingly, especially in the `print` functions
* harmonized various function behavior:
   - `update_*` and `create_*` functions return their respective object (e.g. `update_job` --> `Job`) instead of their ID (previous behavior)
   - `list_*` functions return a named list of those particular exchange objects (e.g. `list_jobs` --> named list of `Job`)
   - parameters, where before only the ID of particular object was passed on, were changed to allow the overview or detailed object description also
* the `Graph` object does not hold a reference about the available data sets any longer
* the user defined processes management exchanges now Process objects instead of former Graph objects with the back-end
* when creating and storing user defined processes on a back-end, they now require a distinct id selected by the user
* updating an user defined process is done via replacement, so the old representation is queried in the `update_process_graph` and merged with the new information given by the function parameter
* renamed functions
   - `list_file_formats()` to `list_file_formats()`
   - `list_process_graphs()` to `list_user_processes()`
   - `describe_process_graph()` to `describe_user_process()`
   - `delete_process_graph()` to `delete_user_process()`
   - `create_process_graph()` to `create_user_process()`
   - `update_process_graph()` to `update_user_process()`
   - `validate_process_graph()` to `validate_process()`
* `FileFormat` object can passed to `OutputFormat` parameter
* in addition to ID also the objects of respective `list_*` and `describe_*` can be passed as parameter
* old `*Info` class names are reduced to the respective shorter class name without "Info", except for `ProcessInfo`
* print functions of exchange objects obtained by `list_*` and `describe_*` are labeled with the same class and printed with the information available (either overview or detailed view)
* made the OIDC connection configurable by the user, since the user needs a client id and secret by the back-end provider
* enabled and reimplemented the `GeoJson` argument
* when starting the computation of a job you can trigger the immediate request to the job log with parameter `log` at `start_job()`
* Kernel argument does not inherit from Array and allows matrix, array and data.frame as kernel value

## Removed
* removed function `follow_job()` (replaced by `log_job()`)
* removed the field `connection` which linked to the used openEO client from the process graph object
* removed function `process_graph_builder()`
* removed function `callback()`, because those Process Graphs are either created from an R function or an resulting `ProcessNode`
* removed URL encoding procedure when managing files on the back-end
* removed `user_id` as a parameter from the files request, because the user information is send with the bearer token

# version 0.6.2

## Fixed
* interpretation of send_udf response into correct json
* tibble version 3.0.0 issue, when searching for endpoints in the endpoint mapping [#49]

## Changed
* `send_udf` can now handle user_context
* `send_udf` has now a parameter for legacy support (which calls the legacy endpoint of a UDF service with API version 0.1.0)
* String parameter can now handle file paths and calls (in context with `run_udf` openEO process) [#48]

## Added
* URI parameter

# version 0.6.1

## Fixed
* fixed #46 were the process link of arguments was not set correctly

## Removed
* package dependency `commonmark`

# version 0.6.0

## Added
* added a collection visualization in a HTML viewer based on "openEO JS commons"
* bridging towards the "openEO JS commons" by reusing the process viewer, a new function `process_viewer` spawns now a viewer in RStudio and renders a beautiful website containing the process information
* function `processes` that will create an object that has the processes that are available on the connected openEO service. It is quite similar to the return value of `process_graph_builder` which will create a `Graph` object with basically the same functionality plus the graph related functions
* transferred a function to directly interact with UDF services from an example script of R UDF service repository `send_udf` and added documentation to the function
* added an example script for the new callback functions
* added a Rmd for the new callback behavior

## Changed
* variables are decoupled from the graph object which means more flexibility when dealing with variables, they will be parsed from the Graph / end node
* it is no longer needed to create a `Graph` object explicitly, passing the end node of the defined workflow is now possible.
* connection is not required to be passed into the functions that directly interact with the openEO service, `active_connection` now gets and sets the latest connected service into a package variable
* the mathematical constants `pi` and `e` (in R `pi` and `exp(1)`) are parsed from the data and translated into the corresponding openEO processes in order to avoid errors due to rounded values
* overloaded mathematical operators for `ProcessNode` and `callback-value` classes
* allowed functions as "callbacks" for reducer function, e.g. in reduce or apply (not only for band arithmetic)
* processes can now be created from any graph in callbacks provided as functions
* slight change for indirect parameter order in processes (backward compatible)

## Fixed
* fixed the server sided validation response handling (HTTP 200 were always OK, despite having errors)
* fixed bad ordering of bounding box parameter

## Removed
* removed the `markdownViewer` function in favor of `process_viewer` and `collection_viewer`
* removed the function `band_arithmetic`, because callbacks can now be functions


# version 0.5.0

## Added
* example process graph building for minimum EVI with EURAC, EODC and VITO
* added a Google OIDC authentication option
* band_arithmetic function that parses a mathematical function into a process graph if the back-end supports some of the core processes
* example script shown in the open Geo Hub Summer School
* notebooks for comparing the NDVI calculation on EURAC back-end and GEE

## Changed
* modified `connect` with new parameters `exchange_token` and `external`. With the latter we can specify 'google' as an external identity provider and with `exchange_token` we can specify if we send the default `access_token` or the `id_token` as Bearer token in the authentication header
* some endpoints allowed optional sending of a bearer token in the header, which was not yet considered. Added a function to check if a user has logged in (`OpenEOClient$isLoggedIn()`) and made it optional at the following endpoints to sent a Bearer token as Authorization-header depending on whether the user is logged in: 
```
GET /output_formats
GET /udf_runtimes
GET /service_types
GET /collections
GET /collections/{collection_id}
GET /processes
POST /validation
```
* modified `gee-evi-example` now that the GEE driver supports all relevant openEO processes, it now creates the graph with apply, linear_scale and saves it as PNG; added also an example for the band_arithmetic use case
* client public function `client_version` was renamed into `api_version` and returns the openEO API version which the client uses
* graphs are initialized with a connection instead of the parsed processes list
* connection is now available via a private field in the Graph object
* callback now reuses the obtained process in JSON format of the connection

## Fixed
* fixed callback function which not replaced the AnyOf parameter with the newly created Graph
* `create_job` now refers to the HTTP response header "openEO-Identifier" rather than parsing the "Location" header
* fixed a problem in `band_arithmetic` where the parameter in array_element were falsely assigned
* fixed callback function to recognize nested callback arguments in anyOf (#35)
* fixed the attempt to parse the response body when evaluating HTTP 202
* typo in the README

# version 0.4.2

## Changed
* modified the Use case 1 R client -> EURAC example (mostly working) based on API version 0.4.2
* return a list of file paths on `download_results`

## Fixed
* fixed error on `list_file_types`
* fixed bad JSON graph formatting when sending a graph for immediate computation (`compute_result`)
