# Example expected syntax of the R client. (Not expected to be functional yet)

library(devtools)
options(unzip = 'internal'); install_github("Open-EO/openeo-r-backend", ref="develop") # Currently takes a while
install_github("Open-EO/openeo-r-client", ref="develop")
library(openeo)

# Potentially there should be an ability to explore available/connected backends

## Connect to a backend
conn = connect("http://openeo.org/openeo", "username", "password")


## Listing backend capabilities:
# Get a list of functions that the backend natively supports; names and descriptions (list/named vector)
listProcesses(conn)

# Same for listing collections on the backend
listCollections(conn)

# Get in-depth information about a process or collection
describe(conn, "S2_L2A_T32TPS_20M")
# class with Extent, crs, etc.

# Listing existing user jobs
listJobs(conn)

# Listing which UDF capabilities/packages are supported
listUDFCapabilities(conn)


## Simple task: calculating NDVI using a linked list of processes (assumes NDVI function takes band names as parameters)
Task = collection("S2_L2A_T32TPS_20M") %>% process("filter_daterange", start="2016", end="2018") %>% process("NDVI", red="B04", nir="B8A")

## Advanced task: calculating NDVI using processes that have other processes as arguments (assumes NDVI function takes process graphs)
AOICollection = collection("S2_L2A_T32TPS_20M") %>% process("filter_daterange", start="2016", end="2018")
RedSubset = AOICollection %>% process("filter_band", band="red")
NIRSubset = AOICollection %>% process("filter_band", band="nir")
Task = AOICollection %>% process("NDVI", red=RedSubset, nir=NIRSubset)

## Three ways of processing data in OpenEO:
# Run right away and give data as an object (Raster?): synchronised; 'format' optional, should default to native that is then read into a Raster* object
Result = executeTask(conn, Task, format="GTiff")

# Batch: ask the server to prepare it the USGS ESPA way, get a job ID and the URL to the output location
OutputInfo = orderTask(conn, Task)
JobID = OutputInfo$JobID

# Lazy: ask the server to run it when needed (on WCS, but also download etc.)
JobID = queueTask(conn, Task)

## Functions that can be performed with a JobID (lazy and batch only)
followJob(conn, JobID) # cat --follow style updates about the job
JobInfo = queryJob(conn, JobID) # Get current information on a job
cancelJob(conn, JobID) # Pause a job; need a way to restart it also
deleteJob(conn, JobID) # Delete the job from the server entirely
Result = downloadJob(conn, JobID) # Get the result as with the synchronous case
(WCSURL = getWCSLink(conn, JobID)) # Get a URL to the WCS to visualise/download data

## Either way have a result, should be a Raster* object
plot(Result)
spplot(Result, 3)


## Theoretical use case 1: composite example ##

CompositeTask = collection("S2_L2A_T32TPS_20M") %>% process("date_range_filter", start="2016-01-01", end="2016-03-10") %>%
    process("bbox_filter", left=652000, right=672000, top=5161000, bottom=5181000, srs="EPSG:32632") %>%
    process("max_time")

# Lazy
Conn = connect(host="http://saocompute.eurac.edu/openEO_WCPS_Driver", user="nobody", password="nobody")
Job = queueTask(Conn, CompositeTask)
OutPath = file.path("Downloads", "Result.netcdf")
downloadJob(Conn, Job, OutPath, "netcdf") # Processing happens here
Result = brick(OutPath)

# Synchronous
Result = executeTask(Conn, CompositeTask)

plot(Result)

## Complex use case: Land cover classification ##

# UDF definitions: not in API yet!
defineUDF(conn, udf_id="resample", type="aggregate_space", content=file.path("resample.r")) # This could be a server-defined function too
defineUDF(conn, udf_id="temporal_cloud_filter", type="apply_time", content=file.path("temporal_cloud_filter.r")) # No such type in API yet
defineUDF(conn, udf_id="harmonic_analysis", type="reduce_time", content=file.path("harmonic_analysis.r"))
defineUDF(conn, udf_id="ranger_classification", type="apply_pixel", content=file.path("ranger_classification.r"))

DEMCollection = collection("GLSDEM") %>% process("bbox_filter", left=652000, right=672000, top=5161000, bottom=5181000, srs="EPSG:32632") %>%
    process("resample", collection("S2_L2A_T32TPS_20M")) # UDF to resample to Sentinel 2 pixel size

CloudlessNDVI = collection("S2_L2A_T32TPS_20M") %>% # Can be reused in several places
    process("bbox_filter", left=652000, right=672000, top=5161000, bottom=5181000, srs="EPSG:32632") %>%
    process("mask", mask="CM", maskvalue=5) %>% process("ndvi", red="B04", nir="B8A") %>%
    process("temporal_cloud_filter", band="B02", threshold="50") # Temporal cloud filter UDF: for each pixel use blue to detect outliers, replace more than 50 with NA

TSParams = CloudlessNDVI %>% process("harmonic_analysis") # Harmonic parameter extraction UDF: get several layers of harmonic model params

MeanNDVI = CloudlessNDVI %>% process("mean_time") # Also get the mean NDVI as a covariate

# Somehow upload training dataset or have a UDF that downloads it(?)
LandCoverTask = process("ranger_classification", NDVI=MeanNDVI, DEM=DEMCollection, TS=TSParams) # UDF: runs a ranger training and prediction pass, returns an LC map and some (spatial?) statistics

Result = executeTask(conn, LandCoverTask)
spplot(Result, 1)

