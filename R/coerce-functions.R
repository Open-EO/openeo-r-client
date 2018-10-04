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
  
  # extract types
  template = do.call(c ,lapply(colls, function(col) {
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
  
  for (index in seq_along(colls)) {
    collection = colls[[index]]
    
    for (i in seq_along(collection)) {
      val = collection[[i]]
      if (is.list(val)) {
        collection[[i]] = list(val)
      }
    }
    
    args = append(list(.data = table),collection)
    table=do.call("add_row",args = args)
  }
  
  return(table)
}