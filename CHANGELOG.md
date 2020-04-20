# Changelog

## unreleased

### Added
- function `conformance` operated on the new corresponding api endpoint
- new functions `privacy_policy()` and `terms_of_service()` to view the privacy policy and the terms of service

### Fixed

### Changed
- `format` was dropped was a parameter and is replaced by `subtype` when parsing process definitions
- Callback was renamed into ProcessGraph (Graph is still a different Concept)
- callback-values have been replaced by ProcessGraphParameters
- anyOf was dropped as a top-level parameter definition schema, now a list of parameter is evaluated into an `anyOf` parameter object to maintain to intended behavior
- modified viewer scripts to visualize collection and processes following the new api definition
- `ProcessGraph` as a parameter requires a function wrapping - meaning you should not pass an openeo process like `p$min` directly, because with the elaborated `ProcessGraphParameter` we cannot do a reliable automatic variable matching, e.g. often the ProcessGraphParameter "context" is misinterpreted
- serialization of `ProcessGraphParameter` from_argument -> from_parameter

### Removed

## [0.6.2] - 2020-04-09

### Fixed
- interpretation of send_udf response into correct json
- tibble version 3.0.0 issue, when searching for endpoints in the endpoint mapping [#49]

### Changed
- `send_udf` can now handle user_context
- `send_udf` has now a parameter for legacy support (which calls the legacy endpoint of a UDF service with API version 0.1.0)
- String parameter can now handle file paths and calls (in context with `run_udf` openEO process) [#48]

### Added
- URI parameter

## [0.6.1] - 2020-03-23

### Fixed
- fixed #46 were the process link of arguments was not set correctly

### Removed
- package dependency `commonmark`

## [0.6.0] - 2020-02-27 Usability update

### Added
- added a collection visualization in a HTML viewer based on "openEO JS commons"
- bridging towards the "openEO JS commons" by reusing the process viewer, a new function `process_viewer` spawns now a viewer in RStudio and renders a beautiful website containing the process information
- function `processes` that will create an object that has the processes that are available on the connected openEO service. It is quite similar to the return value of `process_graph_builder` which will create a `Graph` object with basically the same functionality plus the graph related functions
- transferred a function to directly interact with UDF services from an example script of R UDF service repository `send_udf` and added documentation to the function
- added an example script for the new callback functions
- added a Rmd for the new callback behavior

### Changed
- variables are decoupled from the graph object which means more flexibility when dealing with variables, they will be parsed from the Graph / end node
- it is no longer needed to create a `Graph` object explicitly, passing the end node of the defined workflow is now possible.
- connection is not required to be passed into the functions that directly interact with the openEO service, `active_connection` now gets and sets the latest connected service into a package variable
- the mathematical constants `pi` and `e` (in R `pi` and `exp(1)`) are parsed from the data and translated into the corresponding openEO processes in order to avoid errors due to rounded values
- overloaded mathematical operators for `ProcessNode` and `callback-value` classes
- allowed functions as "callbacks" for reducer function, e.g. in reduce or apply (not only for band arithmetics)
- processes can now be created from any graph in callbacks provided as functions
- slight change for indirect parameter order in processes (backward compatible)

### Fixed
- fixed the server sided validation response handling (HTTP 200 were always OK, despite having errors)
- fixed bad ordering of bounding box parameter

### Removed
- removed the `markdownViewer` function in favour of `process_viewer` and `collection_viewer`
- removed the function `band_arithmetics`, because callbacks can now be functions

## [0.5.0] - 2019-12-03

### Added
- example process graph building for minimum EVI with EURAC, EODC and VITO
- added a Google OIDC authentication option
- band_arithmetics function that parses a mathematical function into a process graph if the back-end supports some of the core processes
- example script shown in the open geo hub summer school
- notebooks for comparing the NDVI calculation on EURAC back-end and GEE

### Changed
- modified `connect` with new parameters `exchange_token` and `external`. With the latter we can specify 'google' as an external identity provider and with `exchange_token` we can specify if we send the default `access_token` or the `id_token` as Bearer token in the authentication header
- some endpoints allowed optional sending of a bearer token in the header, which was not yet considered. Added a function to check if a user has logged in (`OpenEOClient$isLoggedIn()`) and made it optional at the following endpoints to sent a Bearer token as Authorization-header depending on whether the user is logged in: 
```
GET /output_formats
GET /udf_runtimes
GET /service_types
GET /collections
GET /collections/{collection_id}
GET /processes
POST /validation
```
- modified gee-evi-example now that the GEE driver supports all relevant openeo processes, it now creates the graph with apply, linear_scale and saves it as PNG; added also an example for the band_arithmetics use case
- client public function `client_version` was renamed into `api_version` and returns the openEO API version which the client uses
- graphs are initialized with a connection instead of the parsed processes list
- connection is now available via a private field in the Graph object
- callback now reuses the obtained process in JSON format of the connection

### Fixed
- fixed callback function which not replaced the AnyOf parameter with the newly created Graph
- `create_job` now refers to the HTTP response header "OpenEO-Identifier" rather than parsing the "Location" header
- fixed a problem in `band_arithmetics` where the parameter in array_element were falsely assigned
- fixed callback function to recognize nested callback arguments in anyOf (#35)
- fixed the attempt to parse the response body when evaluating HTTP 202
- typo in the README

## [0.4.2] - 2019-09-09

### Changed
- modified the Use case 1 RClient -> EURAC example (mostly working) based on API version 0.4.2
- return a list of file paths on `download_results`

### Fixed
- fixed error on `list_file_types`
- fixed bad JSON graph formatting when sending a graph for immediate computation (`compute_result`)
