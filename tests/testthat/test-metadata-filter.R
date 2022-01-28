json = '{
  "default": {},
  "description": "Limits the data by metadata properties to include only data in the data cube which all given conditions return true for (AND operation). Specify key-value-pairs with the key being the name of the metadata property, which can be retrieved with the openEO Data Discovery for Collections. The value must a condition (user-defined process) to be evaluated against the collection metadata, see the example.",
  "name": "properties",
  "optional": true,
  "schema": [
    {
      "additionalProperties": {
        "parameters": [
          {
            "description": "The property value to be checked against.",
            "name": "value",
            "schema": {
              "description": "Any data type."
            }
          }
        ],
        "returns": {
          "description": "true if the data should be loaded into the data cube, otherwise false.",
          "schema": {
            "type": "boolean"
          }
        },
        "subtype": "process-graph",
        "type": "object"
      },
      "description": "A list of filters to check against. Specify key-value-pairs with the key being the name of the metadata property name and the value being a process evaluated against the metadata values.",
      "subtype": "metadata-filter",
      "title": "Filters",
      "type": "object"
    },
    {
      "description": "Do not filter by metadata properties.",
      "title": "No filter",
      "type": "null"
    }
  ]
}'


test_that("metadata filter argument is found", {
  obj = parameterFromJson(fromJSON(json,simplifyDataFrame = FALSE))
  
  expect("metadata-filter" %in% class(obj), failure_message = "object was not correctly interpreted")
})


# other tests need an active connection
# test_that("metadata filter sets value correctly and validate", {
#   skip_if_offline()
#   host = "https://openeo.cloud"
#   con = connect(host,provider = "egi")
#   obj = parameterFromJson(fromJSON(json,simplifyDataFrame = FALSE))
#   
#   filter = list(
#     "eo:cloud_cover" = function(x) x >= 0 & x < 50, # or function(x, builder) builder$between(x, 0, 50)
#     "platform" = function(x) x == "Sentinel-2A"
#   )
#   
#   obj$setValue(filter)
#   err = obj$validate()
#   expect(length(err) == 0, failure_message = "value was not correctly validate")
#   
#   v = obj$getValue()
#   expect(length(v) == 2 && all(sapply(v,function(x)"Process" %in% class(x))), failure_message = "value was not correctly validate")
#   
# })