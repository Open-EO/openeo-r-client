COMPONENT_MAP = list(
    'collection' = 'data',
    'data-table' = 'data',
    'file-format' = 'format',
    'file-formats' = 'formats',
    'item' = 'data',
    'job-estimate' = 'estimate',
    'service-type' = 'service',
    'service-types' = 'services',
    'udf-runtime' = 'runtime',
    'udf-runtimes' = 'runtimes'
)

TABLE_COLUMNS = list(
    'jobs' = list(
        'id' = list(
            'name' = 'ID',
            'primaryKey' = TRUE
        ),
        'title' = list(
            'name' = 'Title'
        ),
        'status' = list(
            'name' = 'Status'
#           'stylable' = True
        ),
        'created' = list(
            'name' = 'Submitted',
            'format' = 'Timestamp',
            'sort' = 'desc'
        ),
        'updated' = list(
            'name' = 'Last update',
            'format' = 'Timestamp'
        )
    ),
    'services' = list(
        'id' = list(
            'name' = 'ID',
            'primaryKey' = TRUE
        ),
        'title' = list(
            'name' = 'Title'
        ),
        'type' = list(
            'name' = 'Type'
#           'format' = value => typeof value === 'string' ? value.toUpperCase()  = value,
        ),
        'enabled' = list(
            'name' = 'Enabled'
        ),
        'created' = list(
            'name' = 'Submitted',
            'format' = 'Timestamp',
            'sort' = 'desc'
        )
    ),
    'files' = list(
        'path' = list(
            'name' = 'Path',
            'primaryKey' = TRUE,
#           'sortFn' = Utils.sortByPath,
            'sort' = 'asc'
        ),
        'size' = list(
            'name' = 'Size',
            'format' = "FileSize",
            'filterable' = FALSE
        ),
        'modified' = list(
            'name' = 'Last modified',
            'format' = 'Timestamp'
        )
    )
)

html_viewer = function(html) {
    tempfile <- tempfile(fileext = ".html")
    
    cat(html, file = tempfile)
    
    htmlFile = file.path(tempfile)
    viewer <- getOption("viewer")
    
    if (is.null(viewer)) {
      warning(paste0("Cannot show a viewer panel. 'viewer' not available, maybe you are using this package outside of RStudio."))
      return(invisible(NULL))
    }
    
    viewer(htmlFile)
}

read_template = function(file, props, component = NULL) {    
    template_file = system.file("extdata", paste(file, ".html", sep = ""), package = "openeo")
    html = readChar(template_file, nchars = file.info(template_file)$size)
    
    json = jsonlite::toJSON(props, force = TRUE, auto_unbox = TRUE, null = "null")
    html = gsub(x = html, pattern = "{props}", replacement = json, fixed = TRUE)
    if (!is.null(component)) {
      html = gsub(x = html, pattern = "{component}", replacement = component, fixed = TRUE)
    }
    
    return (html)
}

add_basemap_from_env = function(props) {
  map = list()

  url = Sys.getenv("OPENEO_JUPYTER_BASEMAP_URL") # Don't distinguish between NA and "" as "" IS NOT a valid value
  if(length(url) > 0) {
    map[["basemap"]] = url
  }

  attribution = Sys.getenv("OPENEO_JUPYTER_BASEMAP_ATTRIBUTION", unset = NA) # Distinguish between NA and "" as "" IS a valid value
  if(!is.na(attribution)) {
    map[["attribution"]] = attribution
  }

  if (length(map) > 0) {
    props[["mapOptions"]] = map
  }

  return (props)
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
process_viewer = function(x, con = NULL) {
  tryCatch({
    if (length(con) == 0) con = .assure_connection(con)
    
    if (is.function(x)) {
      x = do.call(x, args = list())
    } else if ("ProcessCollection" %in% class(x)) {
      x = con$processes
    } else if (is.character(x)) {
      x = describe_process(con = con,process = x)
      
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
      x = describe_process(con = con, process = pid)
    }
    
    if (!"ProcessInfo" %in% class(x)) {
        x = unname(x)
    } else {
        x = list(x)
    }
    
    props = list(
      document = x,
      apiVersion = con$api_version(),
      showTableOfContents = is.list(x) && length(x) > 1,
      provideDownload = FALSE
    )
    
    html = read_template("process_docgen", props)
    html_viewer(html)
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
collection_viewer = function(x, con = NULL) {
  tryCatch({
    if (length(con) == 0) con = .assure_connection(con)
    
    if (is.character(x)) {
      x = describe_collection(con=con,collection=x)
    }
    
    if (is.null(x)) {
      return(invisible(NULL))
    }
    
    if (!"Collection" %in% class(x)) {
      if (length(x$`cube:dimensions`) > 0) {
        x = unname(x)
      } else {
        x = unname(describe_collection(collection = x))
      }
        
    } else {
      if (length(x$`cube:dimensions`) == 0) {
        x = describe_collection(collection = x)
      }
    }
    
    props = add_basemap_from_env(props = list(data = x))

    html = read_template(file = "viewer", props = props, component = "collection")
    html_viewer(html)
  }, error = .capturedErrorToMessage)
}

# Is this in a Jupyter notebook?
is_jupyter = function() {
  return (isTRUE(getOption('jupyter.in_kernel')))
}

# Is this in a RStudio notebook?
is_rstudio_nb = function() {
  return (isTRUE(getOption('rstudio.notebook.executing')))
}

# Is this in a RMarkdown / knitr context?
is_rmd = function() {
  return (isTRUE(getOption('knitr.in.progress')) && knitr::is_html_output() == TRUE)
}

# Is this in a HTML context (any onf the above)?
is_html_context = function() {
  return (is_jupyter() || is_rstudio_nb() || is_rmd());
}

# Print a HTML component for the given data in several contexts (notebooks, markdown, ...)
print_html = function(component, data, props = list()) {
  html = get_component_html(component, data, props);
  if (is_jupyter()) {
    IRdisplay::display_html(html)
    return(invisible(data))
  }
  else if (is_rstudio_nb()) {
    print(htmltools::HTML(html))
    return(invisible(data))
  }
  else if (is_rmd()) {
    knitr::knit_print(htmltools::HTML(html))
    return(data)
  }
  else {
    print.default(data) # todo: does this make sense?
  }
}

# Generate HTML for the given data using a Vue component
get_component_html = function(component, data = NULL, props = list()) {
  # Special handling for batch job results, show either item or collection depending on the data
  if (component == "batch-job-result") {
    if ("type" %in% data && data[["type"]] == "Feature") {
      component = "item"
    } else {
      component = "collection"
    }
  }
  
  if (component == "data-table") {
    props[['columns']] = TABLE_COLUMNS[[props[['columns']]]]
  } else if (component %in% c('collection', 'collections', 'item', 'items')) {
    props = add_basemap_from_env(props)
  }

  # Set the data as the corresponding parameter in the Vue components
  key = COMPONENT_MAP[[component]]
  if (is.null(key)) {
    key = component
  }
  if (!is.null(data)) {
    props[[key]] = data
  }

  # Construct HTML, load Vue Components source files only if the openEO HTML tag is not yet defined
  html = read_template("web_component", props, component = component)

  return (html)
}