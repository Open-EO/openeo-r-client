htmlViewer = function(html) {
  tempfile <- tempfile(fileext = ".html")
  
  cat(html, file=tempfile)
  
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
#' @importFrom commonmark markdown_html
#' @export
markdownViewer = function(mdtext,isgithub=FALSE) {
  htmlViewer(markdown_html(mdtext,extensions=isgithub))
}