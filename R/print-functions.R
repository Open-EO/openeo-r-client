#' Prints a User object
#' 
#' A nice visualization for the user account information obtained by /me
#' 
#' @param x an User object that can be retrieved at \link{describe_account}
#' @param ... additional parameters (not used)
#' 
#' @export
print.User = function(x, ...) {
    cat(paste("ID:", "\t", x$user_id, "\n", sep = ""))
    if (!is.null(x$budget)) {
        cat(paste("Budget:", "\t", x$budget, "\n", sep = ""))
    }
    
    if (!is.null(x$stroage)) {
        cat("File Storage:\n")
        cat(paste("   Quota:", "\t", x$storage$quota, " Bytes", "\n", sep = ""))
        cat(paste("   Free:", "\t", x$storage$free, " Bytes", "\n", sep = ""))
    }
}


#' Pretty print an openeo process
#' 
#' Print function to visualize relevant information about an openeo process
#' 
#' @param x process info that is received on \link{list_processes} and \link{describe_process}
#' @param ... additional parameters (not used)
#' 
#' @export
print.ProcessInfo <- function(x, ...) {
    title = paste("Process:\t", x$id, sep = "")
    summary = paste("Summary:\t", x$summary, sep = "")
    description = paste("Description:\t", x$description, sep = "")
    result = paste("Returns:\t", x$returns$description, sep = "")
    
    if (length(x$parameters) > 0) {
        parameter_names = sapply(x$parameters, function(x)x$name)
        parameter_descriptions = sapply(x$parameters, function(arg) {
            if (length(arg$description) == 1) {
                return(arg$description)
            } else if (length(arg$description) > 1) {
                return(arg$description[[1]])
            } else {
                return("")
            }
            
        })
        parameter_optional = sapply(x$parameters, function(arg) {
            if (!is.null(arg$optional)) {
                return(arg$optional)
            } else {
                return(FALSE)
            }
        })
        
        d = data.frame(Parameter = parameter_names, 
                       Description = parameter_descriptions, 
                       Optional = parameter_optional, 
                       stringsAsFactors = FALSE)
        rownames(d) <- NULL
        
        if (length(x$process_graph) > 0) {
            cat(paste(title, summary, description, result, "", sep = "\n"))
            cat("\n")
            print(d)
            cat("\n")
            cat("Stored process graph:\n")
            print(x$process_graph)
        } else {
            cat(paste(title, summary, description, result, "", sep = "\n"))
            cat("\n")
            print(d)
        }
        
        
        
    }
}

#' @export
print.ServiceType = function(x, ...) {
    service_type = paste(x$service, "\n")
    parameters = paste("Parameters", "(used on service creation)\n")
    
    d1 = data.frame(Parameter = names(x$parameters), Description = sapply(x$parameters, function(arg) arg$description), Type = sapply(x$parameters, function(arg) arg$type), 
        default = sapply(x$parameters, function(arg) arg$default), example = sapply(x$parameters, function(arg) {
            enum = arg$enum
            return(paste("[", paste(enum, sep = "", collapse = ", "), "]", sep = ""))
        }), stringsAsFactors = FALSE)
    row.names(d1) = NULL
    
    attributes = paste("Attributes", "(used during request)\n")
    
    d2 = data.frame(Attributes = names(x$attributes), Description = sapply(x$attributes, function(arg) arg$description), example = sapply(x$attributes, function(arg) {
        example = arg$example
        return(paste("[", paste(example, sep = "", collapse = ", "), "]", sep = ""))
    }), stringsAsFactors = FALSE)
    row.names(d2) = NULL
    
    cat(service_type, parameters)
    print(d1)
    cat(attributes)
    print(d2)
}

#' @export
print.CollectionInfo = function(x, ...) {
    id = paste(x$id)
    if (is.null(x$title)) 
        x$title = "---"
    title = paste("Title:\t\t\t\t", x$title, sep = "")
    
    description = paste0("Description:\t\t\t", x$description)
    deprecated = if(!is.null(x$deprecated) && !as.logical(x$deprecated)) paste0("Deprecated:\t\t\t",x$deprecated) else NULL
    
    if (is.null(x$providers)) 
        x$providers = list(list(name = "---"))
    source = paste("Source:\t\t\t\t", paste(sapply(x$providers, function(p) {
        p$name
    }), sep = "", collapse = ", "), sep = "")
    
    if (!is.null(x$summaries)) {
        if (is.null(x$summaries$`eo:platform`)) 
            x$summaries$`eo:platform` = "---"
        platform = paste("Platform:\t\t\t", x$summaries$`eo:platform`, sep = "")
        if (is.null(x$summaries$`eo:constellation`)) 
            x$summaries$`eo:constellation` = "---"
        constellation = paste("Constellation:\t\t\t", x$summaries$`eo:constellation`, sep = "")
        if (is.null(x$summaries$`eo:instrument`)) 
            x$summaries$`eo:instrument` = "---"
        instrument = paste("Instrument:\t\t\t", x$summaries$`eo:instrument`, sep = "")
        
        crs = paste("Data SRS (EPSG-code):\t\t", x$summaries$`eo:epsg`, sep = "")
    }
    
    
    spatial.extent = paste("(", x$extent$spatial[1], ", ", x$extent$spatial[2], "), (", x$extent$spatial[3], ", ", x$extent$spatial[4], ")", sep = "")
    extent = paste("Spatial extent (lon,lat):\t", spatial.extent, sep = "")
    
    time = tryCatch({
        paste("Temporal extent:\t\t", paste(sapply(x$extent$temporal, function(obj) {
            if (is.null(obj) || is.na(obj) || length(obj) == 0) 
                return(NA) 
            else 
                return(format(as_datetime(obj), format = "%Y-%m-%dT%H:%M:%SZ"))
        }), collapse = "/"), sep = "")
    }, error = function(e) {
        paste("Temporal extent:\t\t***parsing error***")
    })

    
    cat(unlist(list(id, title, description,if (!is.null(deprecated)) deprecated else NULL, source, platform, constellation, instrument, extent, crs, time)), sep = "\n")
    
    if (!is.null(x$summaries$`eo:bands`)) {
        cat("Bands:\n")
        print(as.data.frame(x$summaries$`eo:bands`))
    } else if (!is.null(x$summaries$`sar:bands`)) {
        cat("Bands:\n")
        print(as.data.frame(x$summaries$`sar:bands`))
    }
    
}

#' @export
print.JobInfo = function(x, ...) {
    id = paste("Job ID:\t\t", x$id, "\n", sep = "")
    if (is.null(x$title)) 
        x$title = "---"
    title = paste("Title:\t\t", x$title, "\n", sep = "")
    if (is.null(x$description)) 
        x$description = "---"
    description = paste("Description:\t", x$description, "\n", sep = "")
    status = paste("Status:\t\t", x$status, "\n", sep = "")
    submitted = paste("Submitted:\t", x$submitted, "\n", sep = "")
    updated = paste("Updated:\t", x$updated, "\n", sep = "")
    
    if (is.null(x$progress)) 
        x$progress = "---"
    progress = paste("Progress:\t", x$progress, "\n", sep = "")
    
    if (is.null(x$error))  
        x$error$message = "---"
    error = paste("Error:\t\t", x$error$message, "\n", sep = "")
    
    if (is.null(x$plan)) 
        x$plan = "---"
    plan = paste("Plan:\t\t", x$plan, "\n", sep = "")
    costs = paste("Costs:\t\t", x$costs, "\n", sep = "")
    if (is.null(x$budget)) 
        x$budget = "---"
    budget = paste("Budget:\t\t", x$budget, "\n", sep = "")
    
    cat(id, title, description, status, submitted, updated, progress, error, plan, costs, budget, sep = "")
    
    output = "Output:"
    cat(output)
    if (is.null(x$output)) {
        cat("\t\t---\n")
    } else {
        cat("\n")
        cat(toJSON(x$output, pretty = TRUE, auto_unbox = TRUE))
        cat("\n")
    }
    
    process_graph = "Process graph:\n"
    cat(process_graph)
    print(x$process_graph)
}

#' @export
print.ServiceInfo = function(x, ...) {
    
    id = paste("ID:\t\t", x$id, "\n", sep = "")
    
    type = paste("Type:\t\t", x$type, "\n", sep = "")
    
    enabled = paste("Enabled:\t", x$enabled, "\n", sep = "")
    
    submitted = paste("Submitted:\t", x$submitted, "\n", sep = "")
    
    if (is.null(x$title) || is.na(x$title)) 
        x$title = "---"
    title = paste("Title:\t\t", x$title, "\n", sep = "")
    
    if (is.null(x$description) || is.na(x$description)) 
        x$description = "---"
    description = paste("Description:\t", x$description, "\n", sep = "")
    
    if (is.na(x$url)) 
        x$url = "---"
    url = paste("Endpoint:\t", x$url, "\n", sep = "")
    
    plan = paste("Plan:\t\t", x$plan, "\n", sep = "")
    
    costs = paste("Costs:\t\t", x$costs, "\n", sep = "")
    
    if (length(x$budget) == 0 || is.na(x$budget)) 
        x$budget = "---"
    budget = paste("Budget:\t\t", x$budget, "\n", sep = "")
    
    cat(id, type, enabled, title, submitted, description, url, plan, costs, budget, sep = "")
    
    # parameters, attributes, process_graph
    if (is.null(x$parameters)) {
        x$parameters = "---"
        cat(paste("Parameters:\t", x$parameters, "\n", sep = ""))
    } else {
        x$parameters = toJSON(x$parameters, pretty = TRUE, auto_unbox = TRUE)
        cat(paste("Parameters:\n", x$parameters, "\n", sep = ""))
    }
    
    if (is.null(x$attributes)) {
        x$attributes = "---"
        cat(paste("Attributes:\t", x$attributes, "\n", sep = ""))
    } else {
        x$attributes = toJSON(x$attributes, pretty = TRUE, auto_unbox = TRUE)
        cat(paste("Attributes:\n", x$attributes, "\n", sep = ""))
    }
    
    if (is.null(x$process_graph)) {
        x$process_graph = "---"
        cat(paste("Process graph:\t", x$process_graph, "\n", sep = ""))
    } else {
        x$process_graph = toJSON(x$process_graph, pretty = TRUE, auto_unbox = TRUE)
        cat(paste("Process graph:\n", x$process_graph, "\n", sep = ""))
    }
}

#' @export
print.JobCostsEstimation = function(x, ...) {
    header = "Job costs estimation\n"
    line = "====================\n"
    costs = paste("Costs: \t\t\t\t", x$costs, "\n", sep = "")
    duration = paste("Duration: \t\t\t", x$duration, "\n", sep = "")
    downloads = paste("Downloads for owner included: \t")
    yesno = if (x$downloads_included) 
        "yes" else "no"
    
    cat(header, line, costs, duration, downloads, yesno, sep = "")
}

#' @export
print.CollectionList = function(x, ...) {
    df = as.data.frame(x, extract = c("id", "title", "description","deprecated"))
    if (isNamespaceLoaded("tibble")) 
        print(tibble::as_tibble(df)[, c("id", "title", "description","deprecated")]) else print(df)
}

#' @export
print.Graph = function(x, ...) {
    print(graphToJSON(x))
}

#' @export
print.ProcessNode = function(x, ...) {
    print(toJSON(x$serialize(),auto_unbox = TRUE,pretty = TRUE,force=TRUE))
}

#' @export
print.Json_Graph = function(x, ...) {
    print(toJSON(x, auto_unbox = TRUE, force = TRUE, pretty = TRUE))
}

#' @export
print.OpenEOCapabilities = function(x, ...) {
    capabilities = x
    
    title = capabilities$title
    backend_version = capabilities$backend_version
    description = capabilities$description
    
    version = capabilities$api_version
    stac_version = capabilities$stac_version
    stac_id = capabilities$id
    endpoints = capabilities$endpoints
    billing = capabilities$billing  #not used right now
    
    cat(paste0("Back-end:\t\t", title), 
        paste0("Back-end version: \t", backend_version), 
        paste0("Description:\t\t", description), 
        paste0("API-version:\t\t", version),
        paste0("STAC"),
        if(length(stac_id) > 0 || is.na(stac_id)) paste0("   ID:\t\t\t",stac_id) else "   ID:\t\t\t---",
        paste0("   Version:\t\t",stac_version), sep = "\n")
        
    
    server_offering = .listObjectsToDataFrame(endpoints)
    
    if (isNamespaceLoaded("tibble")) 
        server_offering = tibble::as_tibble(server_offering)
    print(server_offering)
}
