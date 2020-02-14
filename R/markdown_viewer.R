htmlViewer = function(html) {
    tempfile <- tempfile(fileext = ".html")
    
    cat(html, file = tempfile)
    
    htmlFile = file.path(tempfile)
    viewer <- getOption("viewer")
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

#' @export
process_viewer = function(x,con=NULL) {
    if (length(con) == 0) con = .assure_connection(con)
    
    api_version = paste0("'",con$api_version(),"'")
    navigator = tolower(as.character(is.list(x) && length(x) > 1))
    doc_gen_version = "@1.0.0-beta.2"
    
    if (!"ProcessInfo" %in% class(x)) {
        x = unname(escaper(x))
    }
    
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



#' Function to use RStudio viewer to view markdown text
#' 
#' It is intended to view data or process description in a human readable way. Therefore markdown will be allowed in the respective `description`
#' fields. This function provides means to visualize the Markdown code as HTML in a RStudio viewer.
#' 
#' @param mdtext markdown text
#' @param isgithub logical parameter to state whether Github markdown was used or not. Default: FALSE
#' 
#' @examples 
#' \dontrun{
#' con = connect(host='http://example.openeo.org/v/0.4.2',
#'               user='user',
#'               password='password',
#'               login_type='basic')
#' 
#' collection = describe_collection(con = con, id = 'some_collection_id')
#' 
#' # the description of a collection is allowed to use markdown
#' markdownViewer(collection$description)
#' }
#' 
#' @importFrom commonmark markdown_html
#' @export
markdownViewer = function(mdtext, isgithub = FALSE) {
    htmlViewer(markdown_html(mdtext, extensions = isgithub))
}
