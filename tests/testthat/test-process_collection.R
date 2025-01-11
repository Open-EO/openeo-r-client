test_that("process list parsing works", {
  openeo:::demo_processes()
  
  expect_silent(processes())
})

test_that("'save_result' parsing with Datacube object",{
  process_description = "{
  \"id\": \"save_result\",
  \"summary\": \"Save processed data\",
  \"description\": \"Makes the processed data available in the given file format to the corresponding medium that is relevant for the context this processes is applied in:\\n\\n* For **batch jobs** the data is stored on the back-end. STAC-compatible metadata is usually made available with the processed data.\\n* For **synchronous processing** the data is sent to the client as a direct response to the request.\\n* **Secondary web services** are provided with the processed data so that it can make use of it (e.g., visualize it). Web service may require the data in a certain format. Please refer to the documentation of the individual service types for details.\",
  \"categories\": [
    \"cubes\",
    \"export\"
  ],
  \"parameters\": [
    {
      \"name\": \"data\",
      \"description\": \"The data to deliver in the given file format.\",
      \"schema\": {
        \"type\": \"object\",
        \"subtype\": \"datacube\"
      }
    },
    {
      \"name\": \"format\",
      \"description\": \"The file format to use. It must be one of the values that the server reports as supported output file formats, which usually correspond to the short GDAL/OGR codes. This parameter is *case insensitive*.\\n\\n* If the data cube is empty and the file format can't store empty data cubes, a `DataCubeEmpty` exception is thrown.\\n* If the file format is otherwise not suitable for storing the underlying data structure, a `FormatUnsuitable` exception is thrown.\",
      \"schema\": {
        \"type\": \"string\",
        \"subtype\": \"output-format\"
      }
    },
    {
      \"name\": \"options\",
      \"description\": \"The file format parameters to be used to create the file(s). Must correspond to the parameters that the server reports as supported parameters for the chosen `format`. The parameter names and valid values usually correspond to the GDAL/OGR format options.\",
      \"schema\": {
        \"type\": \"object\",
        \"subtype\": \"output-format-options\"
      },
      \"default\": {},
      \"optional\": true
    }
  ],
  \"returns\": {
    \"description\": \"Always returns `true` as in case of an error an exception is thrown which aborts the execution of the process.\",
    \"schema\": {
      \"type\": \"boolean\",
      \"const\": true
    }
  },
  \"exceptions\": {
    \"FormatUnsuitable\": {
      \"message\": \"Data can't be transformed into the requested output format.\"
    },
    \"DataCubeEmpty\": {
      \"message\": \"The file format doesn't support storing empty data cubes.\"
    }
  },
  \"links\": [
    {
      \"rel\": \"about\",
      \"href\": \"https://gdal.org/drivers/raster/index.html\",
      \"title\": \"GDAL Raster Formats\"
    },
    {
      \"rel\": \"about\",
      \"href\": \"https://gdal.org/drivers/vector/index.html\",
      \"title\": \"OGR Vector Formats\"
    }
  ]
}"
  obj = jsonlite::fromJSON(process_description,simplifyVector = FALSE)
  test = expect_silent(openeo:::processFromJson(obj))
})


test_that("test process parsing with one parameter having no schema",{
  desc = "{\"id\":\"save_result\",\"summary\":\"Save processed data\",\"description\":\"Makes the processed data available in the given file format to the corresponding medium that is relevant for the context this processes is applied in:\\n\\n* For **batch jobs** the data is stored on the back-end. STAC-compatible metadata is usually made available with the processed data.\\n* For **synchronous processing** the data is sent to the client as a direct response to the request.\\n* **Secondary web services** are provided with the processed data so that it can make use of it (e.g., visualize it). Web service may require the data in a certain format. Please refer to the documentation of the individual service types for details.\",\"categories\":[\"cubes\",\"export\"],\"parameters\":[{\"name\":\"data\",\"description\":\"The data to deliver in the given file format.\",\"schema\":{}},{\"name\":\"format\",\"description\":\"The file format to use. It must be one of the values that the server reports as supported output file formats, which usually correspond to the short GDAL/OGR codes. This parameter is *case insensitive*.\\n\\n* If the data cube is empty and the file format can't store empty data cubes, a `DataCubeEmpty` exception is thrown.\\n* If the file format is otherwise not suitable for storing the underlying data structure, a `FormatUnsuitable` exception is thrown.\",\"schema\":{\"type\":\"string\",\"subtype\":\"output-format\"}},{\"name\":\"options\",\"description\":\"The file format parameters to be used to create the file(s). Must correspond to the parameters that the server reports as supported parameters for the chosen `format`. The parameter names and valid values usually correspond to the GDAL/OGR format options.\",\"schema\":{\"type\":\"object\",\"subtype\":\"output-format-options\"},\"default\":{},\"optional\":true}],\"returns\":{\"description\":\"Always returns `true` as in case of an error an exception is thrown which aborts the execution of the process.\",\"schema\":{\"type\":\"boolean\",\"const\":true}},\"exceptions\":{\"FormatUnsuitable\":{\"message\":\"Data can't be transformed into the requested output format.\"},\"DataCubeEmpty\":{\"message\":\"The file format doesn't support storing empty data cubes.\"}},\"links\":[{\"rel\":\"about\",\"href\":\"https://gdal.org/drivers/raster/index.html\",\"title\":\"GDAL Raster Formats\"},{\"rel\":\"about\",\"href\":\"https://gdal.org/drivers/vector/index.html\",\"title\":\"OGR Vector Formats\"}]}"
  obj = jsonlite::fromJSON(desc,simplifyVector = FALSE)
  test = expect_silent(openeo:::processFromJson(obj))
  expect_contains(names(test$parameters), "data")
})
