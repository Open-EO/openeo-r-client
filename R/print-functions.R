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
    
    cat(paste(title, summary, description, result, "", sep = "\n"))
    cat("\n")
    
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
        
        print(d)
        cat("\n")
            
    }
    
    if (length(x$process_graph) > 0) {
        cat("Stored process graph:\n")
        print(x$process_graph)
    }
}

#' @export
print.FileFormat = function(x, ...) {
    if (length(x$title) == 0) {
        cat("Format: \t\t\t",x$name,"\n",sep="")
    } else {
        cat("Format: \t\t\t",x$title,"\n",sep="")   
    }
    cat("Format ID: \t\t\t",x$name,"\n",sep="")
    cat("Applicable GIS data types: \t",paste0(x$gis_data_types,collapse=", "),"\n",sep="")
    
    param_names = names(x$parameter)
    
    if (length(param_names) > 0) {
        cat("Creation options:\n")
        params = data.frame(name=param_names,
                            descriptions = sapply(x$parameter, function(p)p$description),
                            stringsAsFactors = FALSE)
        row.names(params) = NULL
        print(params)
    }
}

#' @export
print.ServiceType = function(x, ...) {
    # name is set in list_service_types not in specification
    cat("Service: \t\t",x$name,"\n",sep="")
    if (length(x$configuration) > 0) {
        cat("Configuration parameter:\n")
        config_params = data.frame(name = names(x$configuration),description = sapply(x$configuration, function(cparam) {
            cparam$description
        }),stringsAsFactors = FALSE)
        row.names(config_params) = NULL
        print(config_params)
    }
    
    if (length(x$process_parameters) > 0) {
        cat("Process parameter:\n")
        process_parameters = data.frame(name = sapply(x$process_parameters, function(p)p$name), 
                                        description = sapply(x$process_parameters, function(p)p$description),
                                        stringsAsFactors = FALSE)
        row.names(process_parameters) = NULL
        print(process_parameters)
    }
    
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
    created = paste("Created:\t", x$created, "\n", sep = "")
    updated = paste("Updated:\t", x$updated, "\n", sep = "")
    
    if (is.null(x$progress)) 
        x$progress = "---"
    progress = paste("Progress:\t", x$progress, "\n", sep = "")
    
    if (is.null(x$plan)) 
        x$plan = "---"
    plan = paste("Plan:\t\t", x$plan, "\n", sep = "")
    costs = paste("Costs:\t\t", x$costs, "\n", sep = "")
    if (is.null(x$budget)) 
        x$budget = "---"
    budget = paste("Budget:\t\t", x$budget, "\n", sep = "")
    
    cat(id, title, description, status, created, updated, progress, plan, costs, budget, sep = "")

    process_graph = "User defined process:\n"
    cat(process_graph)
    
    print(x$process)
}

#' @export
print.ServiceList = function(x, ...) {
    if (length(x) > 0) {
        df = as.data.frame(x,extract=c("id","title","description","url","type","enabled","created"))
        row.names(df) = NULL
        
        if (isNamespaceLoaded("tibble"))
            df = tibble::as_tibble(df)
        
        print(df) 
    } else {
        message("No secondary services published.")
    }
    
    
}


#' @export
print.Service = function(x, ...) {
    
    id = paste("ID:\t\t", x$id, "\n", sep = "")
    
    type = paste("Type:\t\t", x$type, "\n", sep = "")
    
    enabled = paste("Enabled:\t", x$enabled, "\n", sep = "")
    
    created = paste("Submitted:\t", x$created, "\n", sep = "")
    
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
    
    cat(id, type, enabled, title, created, description, url, plan, costs, budget, sep = "")
    
    # parameters, attributes, process_graph
    if (length(x$configuration) == 0) {
        x$configuration = "---"
        cat(paste("Configuration:\t", x$configuration, "\n", sep = ""))
    } else {
        x$configuration = toJSON(x$configuration, pretty = TRUE, auto_unbox = TRUE)
        cat(paste("Configuration:\n", x$configuration, "\n", sep = ""))
    }
    
    if (length(x$attributes) > 0) {
        x$attributes = toJSON(x$attributes, pretty = TRUE, auto_unbox = TRUE)
        cat(paste("Attributes:\n", x$attributes, "\n", sep = ""))
    }
    
    if (length(x$process) > 0) {
        if ("Process" %in% class(x$process)) {
            pg = unclass(x$process$serialize())
        } else {
            pg = x$process
        }
        
        pg = toJSON(pg, pretty = TRUE, auto_unbox = TRUE)
        cat(paste("Process graph:\n", pg, "\n", sep = ""))
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

#' @export
print.ResultList = function(x, ...) {
    cat("Results for job: ",x$id,"\n")
    if (length(x$properties$expires) > 0) {
        cat("Links expire on: ",x$properties$expires,"\n")
    }
    
    if (length(x$assets) > 0) {
        cat("\n")
        assets = as.data.frame(x$assets)
        
        if (isNamespaceLoaded("tibble")) {
            assets = tibble::as_tibble(assets)
        }
        
        print(assets)
    } else {
        cat("\n")
        cat("No assests found.\n")
    }
    
    # assets overview
    
}
