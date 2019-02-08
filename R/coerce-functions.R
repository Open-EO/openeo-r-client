#' @export
as_tibble.CollectionList = function(x, ...) {
  colls = x$collections
  
  colls = lapply(colls, function(collection) {
    extent = collection$extent
    collection$extent = NULL
    collection$extent.spatial = extent$spatial
    collection$extent.temporal = extent$temporal
    return(collection)
  })
  
  table = .listObjectsToTibble(colls)
  
  return(table)
}

#'@export
as_tibble.BandList = function(x, ...) {
  band_ids = names(x)
  x = unname(x)
  
  table = .listObjectsToTibble(x)
  if (nrow(table) == length(band_ids)) {
    return( table %>% 
              add_column(band_id = band_ids) %>% 
              select(unique(c("band_id",colnames(table))))
    )
  } else {
    return(tibble(band_id=band_ids))
  }
  
}

# x has to be an unnamed list
.listObjectsToTibble = function(x) {
  # extract types
  template = do.call(c ,lapply(x, function(col) {
    lapply(col,function(row) {
      if (is.null(row)) {
        return(NULL)
      }
      
      if (is.list(row)) {
        return(list())
      }
      
      return(do.call(class(row),list(length=0)))
    })
  }))
  template = template[unique(names(template))]
  
  table = do.call("tibble",args=template)

  for (index in seq_along(x)) {
    entry = x[[index]]
    
    if (length(entry) > 0) {
      for (i in seq_along(entry)) {
        val = entry[[i]]
        if (is.list(val)) {
          entry[[i]] = list(val)
        }
      }
      args = append(list(.data = table),entry)
      table=do.call("add_row",args = args)
    }
    
  }
  return(table)
}

asJSON = jsonlite:::asJSON
setMethod("asJSON", "process", function(x, ...) jsonlite:::asJSON(unclass(x), ...))
