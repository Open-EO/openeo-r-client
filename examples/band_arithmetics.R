library(openeo)
library(magrittr)
library(tibble)
library(jsonlite)

user = "group8"
pwd = "test123"

gee_host_url = "https://earthengine.openeo.org"

con = connect(host = gee_host_url, version="0.4.2", user = user, password = pwd, login_type = "basic")

graph = con %>% process_graph_builder()

# the graph will not be finished! it is just a test case for the improved new callback approach
data = graph$load_collection(id = graph$data$`COPERNICUS/S2`,
                             spatial_extent = list(
                               west=16.1,
                               east=16.6,
                               north=48.6,
                               south= 47.2
                             ),
                             temporal_extent = list(
                               "2018-01-01", "2018-02-01"
                             ))

# multiple different cases
reduce = graph$reduce(data = data, reducer = function(x) {
  B08 = x[8]
  B04 = x[4]
  B02 = x[2]
  (2.5 * (B08-B04)) / ((B08 + 6 * B04 - 7.5 * B02) + 1)
}, dimension = "bands")
toJSON(reduce$serialize(),auto_unbox = TRUE,pretty = TRUE)

reduce = graph$reduce(data = data, reducer = function(x) {
  B08 = x[8]
  B04 = x[4]
  B02 = x[2]
  min(B02^2,sqrt(B08),1,sin(B04)) # min, max or other summary functions require the ProcessNode object to be the first element!
}, dimension = "bands")
toJSON(reduce$serialize(),auto_unbox = TRUE,pretty = TRUE)
reduce$validate()

reduce = graph$reduce(data = data, reducer = function(x) {
  min(x) # min, max or other summary functions require the ProcessNode object to be the first element!
}, dimension = "bands")
toJSON(reduce$serialize(),auto_unbox = TRUE,pretty = TRUE)
reduce$validate()

reduce = graph$reduce(data = data, reducer = function(x) {
  B08 = x[8]
  -B08
}, dimension = "bands")
toJSON(reduce$serialize(),auto_unbox = TRUE,pretty = TRUE)
reduce$validate()
