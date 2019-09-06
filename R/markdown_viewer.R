htmlViewer = function(html) {
    tempfile <- tempfile(fileext = ".html")
    
    cat(html, file = tempfile)
    
    htmlFile = file.path(tempfile)
    viewer <- getOption("viewer")
    viewer(htmlFile)
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
