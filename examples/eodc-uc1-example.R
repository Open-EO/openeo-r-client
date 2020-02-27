# Rclient -> EODC version: 0.4.2

library(openeo)
library(tibble)

host_url = "https://openeo.eodc.eu"

con = connect(host = host_url, version="0.4.0", login_type = "oidc",external="google",exchange_token = "id_token")


capabilities()
list_file_types()

list_collections()

d= describe_collection(id="s2a_prd_msil1c")

list_processes()

describe_process("reduce")

p = processes()


data = p$load_collection(id = p$data$s2a_prd_msil1c,
                         spatial_extent = list(
                           west = 652000,
                           south = 5161000,
                           north = 5181000,
                           east = 672000,
                           crs = 32632
                         ),
                         temporal_extent = c("2017-01-01T00:00:00Z","2017-01-08T00:00:00Z"),
                         bands = c("B08","B04"))

ndvi = p$ndvi(data = data,name="ndvi")

min_time = p$reduce(data = ndvi, dimension = "temporal", reducer = function(x) {
  min(x,na.rm=TRUE)
})

result = p$save_result(data = min_time,format = "GTiff")


job_id = create_job(graph = result,
                    title = "Min NDVI example",
                    description = "Calculates the minimum NDVI from R client",
                    format = "GTiff") 

describe_job(job = job_id)

start_job(job=job_id)

describe_job(job = job_id)

download_results(job=job_id, folder = "eodc-uc1-test.tif")
