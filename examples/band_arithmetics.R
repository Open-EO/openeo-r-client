library(openeo)
library(magrittr)
library(tibble)
library(jsonlite)

user = "group8"
pwd = "test123"

gee_host_url = "https://earthengine.openeo.org"

con = connect(host = gee_host_url, version="0.4.2", user = user, password = pwd, login_type = "basic")

graph = con %>% process_graph_builder()

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

reduce = graph$reduce(data = data, reducer = function(x) {
  B08 = x[3]
  B04 = x[2]
  B02 = x[1]
  (2.5 * (B08-B04)) / ((B08 + 6 * B04 - 7.5 * B02) + 1)
}, dimension = "time")