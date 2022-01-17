library(openeo)
library(tibble)

user = "group7"
pwd = "test123"

# connect  to the back-end and login either via explicit call of login, or use your credentials in the connect function
gee = connect(host = "https://earthengine.openeo.org",version = "1.0.0", user = user,password = pwd)

# get the process collection to use the predefined processes of the back-end
p = processes()

# get the collection list to get easier access to the collection ids, via auto completion
collections = list_collections()

# get the formats
formats = list_file_formats()

# load the intial data collection and limit the amount of data loaded
# note: for the collection id and later the format you can also use the its character value
data = p$load_collection(id = collections$`COPERNICUS/S1_GRD`,
                         spatial_extent = list(west=16.06, 
                                               south=48.06,
                                               east=16.65,
                                               north=48.35),
                         temporal_extent = c("2017-03-01", "2017-06-01"),
                         bands = c("VV"))

# create three monthly sub data sets, which will be merged back into a single data cube later
march = p$filter_temporal(data = data,
                          extent = c("2017-03-01", "2017-04-01"))

april = p$filter_temporal(data = data,
                          extent = c("2017-04-01", "2017-05-01"))

may = p$filter_temporal(data = data,
                        extent = c("2017-05-01", "2017-06-01"))

# The aggregation function for the following temporal reducer
agg_fun_mean = function(data, context) {
  mean(data)
}

march_reduced = p$reduce_dimension(data = march,
                                   reducer = agg_fun_mean,
                                   dimension = "t")

april_reduced = p$reduce_dimension(data = april,
                                   reducer = agg_fun_mean,
                                   dimension = "t")

may_reduced = p$reduce_dimension(data = may,
                                 reducer = agg_fun_mean,
                                 dimension = "t")

# Each band is currently called VV. We need to rename at least the label of one dimension, 
# because otherwise identity of the data cubes is assumed. The bands dimension consists 
# only of one label, so we can rename this to be able to merge those data cubes.
march_renamed = p$rename_labels(data = march_reduced,
                                dimension = "bands",
                                target = c("R"),
                                source = c("VV"))

april_renamed = p$rename_labels(data = april_reduced,
                                dimension = "bands",
                                target = c("G"),
                                source = c("VV"))

may_renamed = p$rename_labels(data = may_reduced,
                              dimension = "bands",
                              target = c("B"),
                              source = c("VV"))

# combine the individual data cubes into one
# this is done one by one, since the dimensionalities have to match between each of the data cubes
merge_1 = p$merge_cubes(cube1 = march_renamed,cube2 = april_renamed)
merge_2 = p$merge_cubes(cube1 = merge_1, cube2 = may_renamed)

# rescale the the back scatter measurements into 8Bit integer to view the results as PNG
rescaled = p$apply(data = merge_2,
        process = function(data,context) {
          p$linear_scale_range(x=data, inputMin = -20,inputMax = -5, outputMin = 0, outputMax = 255)
        })

# export shall be format PNG
# look at the format description
formats$output$PNG

# store the results using the format and set the create options
result = p$save_result(data = rescaled,format = formats$output$PNG, options = list(red="R",green="G",blue="B"))

# create a job
job = create_job(graph = result, title = "S1 Example R", description = "Getting Started example on openeo.org for R-client")

# then start the processing of the job and turn on logging (messages that are captured on the back-end during the process execution)
start_job(job = job, log = TRUE)
