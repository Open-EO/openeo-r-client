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
#' Opens up a viewer panel in RStudio and renders nicely one or more processes of the connected 
#' openEO service in HTML. The components of openeo-js-commons / openEO webeditor are reused.
#' 
#' @param x a function from the \code{\link{ProcessCollection}}, a \code{\link{ProcessNode}},
#' \code{\link{Process}} or a character containing the process id.
#' @param con a specific connection (optional), last connected service if omitted.
#' 
#' @export
process_viewer = function(x,con=NULL) {
    if (length(con) == 0) con = .assure_connection(con)
    
    api_version = paste0("'",con$api_version(),"'")
    doc_gen_version = "@1.0.0-beta.2"
    
    if (is.function(x)) {
      x=do.call(x,args=list())
    } else if ("ProcessCollection" %in% class(x)) {
      x = con$processes
    } else if ("Process" %in% class(x)) {
      pid = x$getId()
      if (!pid %in% names(con$processes)) {
        warning(paste0("Process '",pid,"' is not supported by the current openEO service"))
        return(invisible(NULL))
      }
      x = describe_process(con=con,id = pid)
    } else if (is.character(x)) {
      x = describe_process(con=con,id = x)
      
      if (is.null(x)) {
        return(invisible(NULL))
      }
    }
    
    if (!"ProcessInfo" %in% class(x)) {
        x = unname(escaper(x))
    } else {
        x = list(escaper(x))
    }
    
    navigator = tolower(as.character(is.list(x) && length(x) > 1))
    
    x = jsonlite::toJSON(x,force=TRUE,auto_unbox = TRUE)
    
    html="<!DOCTYPE html>
  <html>
  
  <head>
  <title>openEO Processes</title>
  <meta http-equiv='X-UA-Compatible' content='IE=edge'>
  <meta charset='UTF-8'>
  <meta name='viewport' content='width=device-width, initial-scale=1'>
  <script src='https://cdn.jsdelivr.net/npm/vue'></script>
  <script src='https://cdn.jsdelivr.net/npm/@openeo/processes-docgen%doc_gen_version%/dist/DocGen.umd.min.js'></script>
  <link rel='stylesheet'' href='https://cdn.jsdelivr.net/npm/@openeo/processes-docgen%doc_gen_version%/dist/DocGen.css'>
  <style>html, body { height: 100%; margin: 0; }</style>
  </head>
  
  <body>
  <div id='app'></div>
  <script>
    new Vue({
      el: '#app',
      render: h => h(DocGen, { 
        props: {
          document: %processes%,
          apiVersion: %api_version%,
          showTableOfContents: %navigator%
        }
      })
    });
  </script>
  <noscript>Sorry, the documentation generator requires JavaScript to be enabled!</noscript>
  </body>
  
  </html>"
    
    html = gsub(x=html,pattern = "%doc_gen_version%",replacement = doc_gen_version)
    html = gsub(x=html,pattern = "%processes%",replacement=x,fixed=TRUE)
    html = gsub(x=html,pattern = "%navigator%",replacement=navigator)
    html = gsub(x=html,pattern = "%api_version%",replacement=api_version)
    
    htmlViewer(html)
}

#' View for openEO collections
#' 
#' The function opens up a viewer panel in RStudio which renders the collection information
#' nicely in an HTML. It reuses common components from the openEO webeditor / openeo-js-commons.
#' 
#' @param x character with the name of a collection or the \code{\link{CollectionInfo}} obtained
#' with \code{\link{describe_collection}}.
#' @param con a specific connection (optional), last connected service if omitted.
#' 
#' @export
collection_viewer = function(x,con=NULL) {
    if (length(con) == 0) con = openeo:::.assure_connection(con)
    
    api_version = paste0("'",con$api_version(),"'")
    doc_gen_version = "@latest"
    
    if (is.character(x)) {
      x = describe_collection(con=con,id=x)
    }
    
    if (is.null(x)) {
      return(invisible(NULL))
    }
    
    if (!"CollectionInfo" %in% class(x)) {
        x = unname(escaper(x))
    } else {
        x = escaper(x)
    }
    
    x = jsonlite::toJSON(x,force=TRUE,auto_unbox = TRUE)
    
    html="<!DOCTYPE html>
<html>

	<head>
		<title>openEO Collection</title>
		<meta http-equiv='X-UA-Compatible' content='IE=edge'>
		<meta charset='UTF-8'>
		<meta name='viewport' content='width=device-width, initial-scale=1'>
		<script src='https://cdn.jsdelivr.net/npm/vue'></script>
		<script src='https://cdn.jsdelivr.net/npm/@openeo/vue-components%doc_gen_version%/assets/openeo-vue.umd.min.js'></script>
		<link rel='stylesheet' href='https://cdn.jsdelivr.net/npm/@openeo/vue-components%doc_gen_version%/assets/openeo-vue.css'>
		<style>html, body { height: 100%; margin: 1em; font-family: sans-serif; }</style>
	</head>

	<body>
		<div id='app'></div>
		<script>
			var { Collection } = window['openeo-vue'];
			new Vue({
				el: '#app',
				render: h => h(Collection, { 
					props: {
						collectionData: %collection_info%,
						version: %api_version%
					}
				})
			});
		</script>
		<noscript>Sorry, the documentation generator requires JavaScript to be enabled!</noscript>
	</body>

</html>"
    
    html = gsub(x=html,pattern = "%doc_gen_version%",replacement = doc_gen_version)
    html = gsub(x=html,pattern = "%collection_info%",replacement=x,fixed=TRUE)
    html = gsub(x=html,pattern = "%api_version%",replacement=api_version)
    
    htmlViewer(html)
}
