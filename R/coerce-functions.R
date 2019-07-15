.removeNullEntries = function(list) {
  list[-which(sapply(list, is.null))]
}

# x has to be an unnamed list
.listObjectsToDataFrame = function(x, extract = NULL) {
  if (is.null(extract)) {
    # extract types
    template = do.call(c, lapply(x, function(col) {
      lapply(col, function(row) {
        if (is.null(row)) {
          return(character())
        }
        
        if (is.list(row)) {
          return(character())
        }
        
        return(do.call(class(row), list(length = 0)))
      })
    }))
    template = template[unique(names(template))]
  } else {
    template = extract
    names(template) = extract
  }
    
    table = do.call("data.frame", args = append(template,list(stringsAsFactors=FALSE)))
    
    for (index in seq_along(x)) {
      initial_row = as.list(rep(NA,length(template)))
      names(initial_row) = names(template)
      
      table = rbind(table,initial_row,stringsAsFactors=FALSE)
      
        entry = x[[index]]
        if (length(entry) > 0) {
            for (i in seq_along(entry)) {
                val = entry[[i]]
                if (is.list(val)) {
                  entry[[i]] = list(val)
                }
            }
          entry=entry[names(template)]
          entry[sapply(entry,is.null)] = NA
          names(entry) = names(template)
          
          for (name in names(entry)) {
            val = entry[[name]]
            if (length(val) > 1) val = list(val)
            
            table[nrow(table),name][[1]] = val
          }
        }
        
    }
    return(table)
}

.listObjectsToTibble = function(x) {
  # extract types
  template = do.call(c, lapply(x, function(col) {
    lapply(col, function(row) {
      if (is.null(row)) {
        return(NULL)
      }
      
      if (is.list(row)) {
        return(list())
      }
      
      return(do.call(class(row), list(length = 0)))
    })
  }))
  template = template[unique(names(template))]
  
  table = do.call("tibble", args = template)
  
  for (index in seq_along(x)) {
    entry = x[[index]]
    
    if (length(entry) > 0) {
      for (i in seq_along(entry)) {
        val = entry[[i]]
        if (is.list(val)) {
          entry[[i]] = list(val)
        }
      }
      args = append(list(.data = table), entry)
      table = do.call("add_row", args = args)
    }
    
  }
  return(table)
}

#' Coercions into data.frame objects
#' 
#' The openeo package offers functions to transform list objects obtained from JSON
#' into data.frames. This is mostly applied in list_* functions.
#' 
#' @name as.data.frame
#' @param x the list object that will be coerced
#' @param ... potentially additional parameters to pass on to internal functions like "extract"
#' 
#' @details 
#' The parameter "extract" is used as an additional parameter to extract specific values of the output list
#' / json. The value for the parameters is a vector of character like c("id","title")
#' 
#' @return a data.frame
#' 
#' @export
as.data.frame.JobList = function(x, ...) {
  params=list(...)
  return(.listObjectsToDataFrame(x,extract = params$extract))
}

#' @rdname as.data.frame
#' @export
as.data.frame.BandList = function(x, ...) {
  x = unname(x)
  params = list(...)
  
  table = .listObjectsToDataFrame(x,extract = params$extract)
  return(table)
}

#' @rdname as.data.frame
#' @export
as.data.frame.CollectionList = function(x, ...) {
  colls = x$collections
  
  params = list(...)
  
  colls = lapply(colls, function(collection) {
    extent = collection$extent
    collection$extent = NULL
    collection$extent.spatial = extent$spatial
    collection$extent.temporal = extent$temporal
    return(collection)
  })
  
  table = .listObjectsToDataFrame(colls,extract = params$extract)
  
  return(table)
}

#' @rdname as.data.frame
#' @export
as.data.frame.VersionsList = function(x, ...) {
  versions = x$versions
  params = list(...)
  table = .listObjectsToDataFrame(versions,extract = params$extract)
  return(table[c("api_version", "production", "url")])
}