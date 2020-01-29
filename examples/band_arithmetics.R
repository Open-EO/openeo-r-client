library(openeo)
library(magrittr)
library(tibble)
library(jsonlite)

user <- "group8"
pwd <- "test123"

gee_host_url <- "https://earthengine.openeo.org"

con <- connect(
  host = gee_host_url,
  version = "0.4.2",
  user = user,
  password = pwd,
  login_type = "basic"
)


# Test cases ----
# in the following there are some test cases for using R's math operators insinde a function
# "callback"
graph <- con %>% process_graph_builder()

# the graph will not be finished! it is just a test case for the improved new callback approach
data <- graph$load_collection(
  id = graph$data$`COPERNICUS/S2`,
  spatial_extent = list(
    west = 16.1,
    east = 16.6,
    north = 48.6,
    south = 47.2
  ),
  temporal_extent = list(
    "2018-01-01", "2018-02-01"
  )
)

# evi reducer
evi_callback <- function(x) {
  B08 <- x[8]
  B04 <- x[4]
  B02 <- x[2]
  (2.5 * (B08 - B04)) / ((B08 + 6 * B04 - 7.5 * B02) + 1)
}

reduce <- graph$reduce(data = data, reducer = evi_callback, dimension = "bands")
toJSON(reduce$serialize(), auto_unbox = TRUE, pretty = TRUE)

# a bit of everything
reduce2 <- graph$reduce(data = data, reducer = function(x) {
  B08 <- x[8]
  B04 <- x[4]
  B02 <- x[2]
  temp <- min(B02^2, sqrt(B08), 1, sin(B04), log(B02^B04, 2), sum(x))

  mean(x) - temp # min, max or other summary functions require the ProcessNode object to be the first element!
}, dimension = "bands")
toJSON(reduce$serialize(), auto_unbox = TRUE, pretty = TRUE)
reduce2$validate()

# min, max or other summary functions require the ProcessNode object to be the first element!
reduce3 <- graph$reduce(data = data, reducer = function(x) {
  min(x) 
}, dimension = "bands")
toJSON(reduce3$serialize(), auto_unbox = TRUE, pretty = TRUE)
reduce3$validate()

# negative number by multiplying with -1
reduce4 <- graph$reduce(data = data, reducer = function(x) {
  B08 <- x[8]
  -B08
}, dimension = "bands")
toJSON(reduce4$serialize(), auto_unbox = TRUE, pretty = TRUE)
reduce4$validate()

# as mentioned the previous graphs examples are not really supposed to be running rather than
# for testing the overloaded math operators

# POC UC 1 min NDVI example ====
user <- "group8"
pwd <- "test123"

gee_host_url <- "https://earthengine.openeo.org"

con <- connect(host = gee_host_url, version = "0.4.2", user = user, password = pwd, login_type = "basic")

graph <- con %>% process_graph_builder()

# the graph will not be finished! it is just a test case for the improved new callback approach
data <- graph$load_collection(
  id = graph$data$`COPERNICUS/S2`,
  spatial_extent = list(
    west = 16.1,
    east = 16.6,
    north = 48.6,
    south = 47.2
  ),
  temporal_extent = list(
    "2018-01-01", "2018-02-01"
  ),
  bands = c("B4", "B8")
)

band_calc <- data %>% graph$reduce(dimension = "bands", reducer = function(x) {
  B04 <- x[1]
  B08 <- x[2]

  (B08 - B04) / (B08 + B04)
})

min_time <- band_calc %>% graph$reduce(dimension = "temporal", reducer = function(x) {
  min(x)
})

# linear scale with reassignment of process node to internal graph
lin_scale <- min_time %>% graph$apply(
  process = function(x) {
    graph$linear_scale_range(x,
      inputMin = -1,
      inputMax = 1,
      outputMin = 0,
      outputMax = 255
    )
  }
)

graph$save_result(data = lin_scale, format = "png") %>% graph$setFinalNode()

graph$validate()

con %>% validate_process_graph(graph)

graph


# old callback snippet ====
apply_linear_transform <- graph$apply(data = min_time)

cb2_graph <- con %>% callback(apply_linear_transform, "process")

cb2_graph$linear_scale_range(
  x = cb2_graph$data$x,
  inputMin = -1,
  inputMax = 1,
  outputMin = 0,
  outputMax = 255
) %>%
  cb2_graph$setFinalNode()

graph$save_result(data = apply_linear_transform, format = "png") %>% graph$setFinalNode()

graph$clean()
graph$validate()



# work around without using linear_scale_range, but a custom defined process using openEO processes
rescale <- function(x, imin, imax, omin, omax) {
  a <- (omax - omin) / (imax - imin)
  b <- omin - (a * imin)

  a * x + b
}

lin_scale2 <- min_time %>% graph$apply(
  process = function(x) {
    rescale(x, -1, 1, 0, 255)
  }
)


graph$save_result(data = lin_scale2, format = "png") %>% graph$setFinalNode()
graph$validate()

# Min EVI use case ----
user <- "group8"
pwd <- "test123"

gee_host_url <- "https://earthengine.openeo.org"

con <- connect(host = gee_host_url, version = "0.4.2", user = user, password = pwd, login_type = "basic")

graph <- con %>% process_graph_builder()

data <- graph$load_collection(
  id = graph$data$`COPERNICUS/S2`,
  spatial_extent = list(
    west = 16.1,
    east = 16.6,
    north = 48.6,
    south = 47.2
  ),
  temporal_extent = list(
    "2018-01-01", "2018-02-01"
  )
)

band_calc <- data %>% graph$reduce(dimension = "bands", reducer = function(x) {
  B08 <- x[8]
  B04 <- x[4]
  B02 <- x[2]
  (2.5 * (B08 - B04)) / ((B08 + 6 * B04 - 7.5 * B02) + 1)
})

min_time <- band_calc %>% graph$reduce(dimension = "temporal", reducer = function(x) {
  min(x)
})

# linear scale with reassignment of process node to internal graph
lin_scale <- min_time %>% graph$apply(
  process = function(x) {
    graph$linear_scale_range(x,
      inputMin = -1,
      inputMax = 1,
      outputMin = 0,
      outputMax = 255
    )
  }
)

graph$save_result(data = lin_scale, format = "png") %>% graph$setFinalNode()

graph

# test at VITO

con <- connect(host = "http://openeo.vgt.vito.be/openeo/0.4.0/")
con %>% list_file_types()

graph <- con %>% process_graph_builder()

data <- graph$load_collection(
  id = graph$data$CGS_SENTINEL2_RADIOMETRY_V102_001,
  spatial_extent = list(
    west = 16.1,
    east = 16.6,
    south = 47.2,
    north = 48.6,
    crs = "EPSG:4326"
  ),
  temporal_extent = list(
    "2018-04-01", "2018-06-01"
  ),
  bands = c("2", "4", "8")
)

evi_calc <- graph$reduce(data = data, dimension = "bands", reducer = function(x) {
  B08 <- x[3]
  B04 <- x[2]
  B02 <- x[1]
  (2.5 * (B08 - B04)) / ((B08 + 6 * B04 - 7.5 * B02) + 1)
})

mint <- graph$reduce(data = evi_calc, dimension = "temporal", reducer = function(x) {
  min(x)
})

# just another test for the binary function approach
# mint <- graph$reduce(data = evi_calc, dimension = "temporal", reducer = function(x,y) {
#   (x+y)/2
# }, binary = TRUE)

graph$save_result(data = mint, format = "GTiff") %>% graph$setFinalNode()

# graph

con %>% compute_result(graph, format = "GTiff", output_file = "test.tif")
