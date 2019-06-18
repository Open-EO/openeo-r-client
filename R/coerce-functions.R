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
  x = unname(x)
  
  table = .listObjectsToTibble(x)
  return( table )
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

