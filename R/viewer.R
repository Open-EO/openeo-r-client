htmlViewer = function(html) {
    tempfile <- tempfile(fileext = ".html")
    
    cat(html, file = tempfile)
    
    htmlFile = file.path(tempfile)
    viewer <- getOption("viewer")
    
    if (is.null(viewer)) {
      warning(paste0("Cannot show a viewer panel. 'viewer' not available, maybe you are using this package not in RStudio."))
      return(invisible(NULL))
    }
    
    viewer(htmlFile)
}

escaper = function(list) {
    lapply(list, function(elem) {
        if (length(elem) > 1) escaper(elem)
        
        if (is.character(elem) && grepl(x = elem,pattern = "\\",fixed=TRUE)) {
            elem = gsub(x=elem,pattern = "\\",replacement="\\\\",fixed=TRUE)
        }
        
        return(elem)
    })
}

#' Viewer panel for provided openEO processes
#' 
#' Opens up a viewer panel in RStudio and renders one or more processes of the connected 
#' openEO service in HTML. The components of openeo-js-commons / openEO webeditor are reused.
#' 
#' @param x a function from the \code{\link{ProcessCollection}}, a \code{\link{ProcessNode}},
#' \code{\link{Process}} or a character containing the process id.
#' @param con a specific connection (optional), last connected service if omitted.
#' 
#' @export
process_viewer = function(x,con=NULL) {
  tryCatch({
    if (length(con) == 0) con = .assure_connection(con)
    
    api_version = paste0("'",con$api_version(),"'")
    doc_gen_version = "1"
    
    if (is.function(x)) {
      x=do.call(x,args=list())
    } else if ("ProcessCollection" %in% class(x)) {
      x = con$processes
    } else if (is.character(x)) {
      x = describe_process(con=con,process = x)
      
      if (is.null(x)) {
        return(invisible(NULL))
      }
    }
    
    if ("Process" %in% class(x)) {
      pid = x$getId()
      if (!pid %in% names(con$processes)) {
        warning(paste0("Process '",pid,"' is not supported by the current openEO service"))
        return(invisible(NULL))
      }
      x = describe_process(con=con,process = pid)
    }
    
    if (!"ProcessInfo" %in% class(x)) {
        x = unname(escaper(x))
    } else {
        x = list(escaper(x))
    }
    
    navigator = tolower(as.character(is.list(x) && length(x) > 1))
    
    x = jsonlite::toJSON(x,force=TRUE,auto_unbox = TRUE)
    
    template_file = system.file("extdata", "process_viewer_template.html", package = "openeo")
    html = readChar(template_file, nchars = file.info(template_file)$size)
    
    html = gsub(x=html,pattern = "%doc_gen_version%",replacement = doc_gen_version)
    html = gsub(x=html,pattern = "%processes%",replacement=x,fixed=TRUE)
    html = gsub(x=html,pattern = "%navigator%",replacement=navigator)
    html = gsub(x=html,pattern = "%api_version%",replacement=api_version)
    
    htmlViewer(html)
  }, error = .capturedErrorToMessage)
}

#' View openEO collections
#' 
#' The function opens a viewer panel in RStudio which renders the collection information
#' in an HTML. It reuses common components from the openEO webeditor / openeo-js-commons.
#' 
#' @param x character with the name of a collection or the \code{Collection} obtained
#' with \code{\link{describe_collection}}.
#' @param con a specific connection (optional), last connected service if omitted.
#' 
#' @export
collection_viewer = function(x,con=NULL) {
  tryCatch({
    if (length(con) == 0) con = .assure_connection(con)
    
    vue_version = "2"
    
    if (is.character(x)) {
      x = describe_collection(con=con,collection=x)
    }
    
    if (is.null(x)) {
      return(invisible(NULL))
    }
    
    if (!"Collection" %in% class(x)) {
      if (length(x$`cube:dimensions`) > 0) {
        x = unname(escaper(x))
      } else {
        x = unname(escaper(describe_collection(collection = x)))
      }
        
    } else {
      if (length(x$`cube:dimensions`) == 0) {
        x = describe_collection(collection = x)
      }
      x = escaper(x)
    }
    
    x = jsonlite::toJSON(x,force=TRUE,auto_unbox = TRUE,null="null")
    
    template_file = system.file("extdata", "collection_viewer_template.html", package = "openeo")
    html = readChar(template_file, nchars = file.info(template_file)$size)
    
    html = gsub(x=html,pattern = "%vue_version%",replacement = vue_version)
    html = gsub(x=html,pattern = "%collection_info%",replacement=x,fixed=TRUE)
    
    htmlViewer(html)
  }, error = .capturedErrorToMessage)
}
