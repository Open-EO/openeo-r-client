api.v0.0.2 = function() {
  api = tibble(endpoint="/capabilities",operation="GET",tag="capabilities")
  api = add_row(api,endpoint="/capabilities/output_formats",operation="GET",tag="formats") %>% 
    add_row(endpoint="/capabilities/services",operation="GET",tag="ogc_services") %>%
    add_row(endpoint="/data",operation="GET",tag="data_overview") %>% 
    add_row(endpoint="/data/opensearch",operation="GET",tag="data_search") %>%
    add_row(endpoint="/data/{product_id}",operation="GET",tag="data_details") %>%
    add_row(endpoint="/processes",operation="GET",tag="process_overview") %>%
    add_row(endpoint="/processes/opensearch",operation="GET",tag="processes_search") %>%
    add_row(endpoint="/processes/{processes_id}",operation="GET",tag="processes_details") %>%
    add_row(endpoint="/udf_runtimes",operation="GET",tag="udf_runtimes") %>%
    add_row(endpoint="/udf_runtimes/{lang}/{udf_type}",operation="GET",tag="udf_functions") %>% 
    add_row(endpoint="/users/{user_id}/process_graphs",operation="GET",tag="graph_overview") %>%
    add_row(endpoint="/users/{user_id}/process_graphs",operation="POST",tag="new_graph") %>%
    add_row(endpoint="/users/{user_id}/process_graphs/{process_graph_id}",operation="GET",tag="graph_details") %>%
    add_row(endpoint="/users/{user_id}/process_graphs/{process_graph_id}",operation="PUT",tag="graph_replace") %>%
    add_row(endpoint="/users/{user_id}/process_graphs/{process_graph_id}",operation="DELETE",tag="graph_delete") %>%
    add_row(endpoint="/users/{user_id}/files",operation="GET",tag="user_files") %>% 
    add_row(endpoint="/users/{user_id}/files/{path}",operation="GET",tag="user_file_download") %>%
    add_row(endpoint="/users/{user_id}/files/{path}",operation="PUT",tag="user_file_upload") %>%
    add_row(endpoint="/users/{user_id}/files/{path}",operation="DELETE",tag="user_file_delete") %>%
    add_row(endpoint="/users/{user_id}/jobs",operation="GET",tag="user_jobs") %>%
    add_row(endpoint="/users/{user_id}/services",operation="GET",tag="user_services") %>%
    add_row(endpoint="/users/{user_id}/credits",operation="GET",tag="user_credits") %>%
    add_row(endpoint="/auth/login",operation="GET",tag="login") %>%
    add_row(endpoint="/auth/register",operation="POST",tag="registration") %>%
    add_row(endpoint="/execute",operation="POST",tag="execute_sync") %>%
    add_row(endpoint="/services",operation="POST",tag="service_publish") %>%
    add_row(endpoint="/jobs",operation="POST",tag="jobs_define") %>%
    add_row(endpoint="/jobs/{job_id}", operation="PATCH",tag="jobs_update") %>%
    add_row(endpoint="/jobs/{job_id}",operation="GET",tag="jobs_details") %>%
    add_row(endpoint="/jobs/{job_id}/subscribe",operation="GET",tag="jobs_log") %>%
    add_row(endpoint="/jobs/{job_id}/queue",operation="PATCH",tag="execute_async") %>%
    add_row(endpoint="/jobs/{job_id}/pause",operation="PATCH",tag="jobs_pause") %>%
    add_row(endpoint="/jobs/{job_id}/cancel",operation="PATCH",tag="jobs_cancel") %>%
    add_row(endpoint="/jobs/{job_id}/download",operation="GET",tag="jobs_download") %>%
    add_row(endpoint="/services/{service_id}",operation="GET",tag="services_details") %>%
    add_row(endpoint="/services/{service_id}",operation="PATCH",tag="services_update") %>%
    add_row(endpoint="/services/{service_id}",operation="DELETE",tag="services_delete")
  
  return(api)
}

api.v0.3.1 = function() {
  api = tibble(endpoint="/",operation="GET",tag="capabilities")
  
  api = api %>% 
    add_row(endpoint="/output_formats",operation="GET",tag="formats") %>% 
    add_row(endpoint="/service_types",operation="GET",tag="ogc_services") %>% 
    add_row(endpoint="/udf_runtimes",operation="GET",tag="udf_runtimes") %>%
    add_row(endpoint="/collections",operation="GET",tag="data_overview") %>% 
    add_row(endpoint="/collections/{data_id}",operation="GET",tag="data_details") %>% 
    add_row(endpoint="/processes",operation="GET",tag="process_overview") %>% 
    add_row(endpoint="/process_graphs",operation="GET",tag="graph_overview") %>% 
    add_row(endpoint="/process_graphs",operation="POST",tag="new_graph") %>% 
    add_row(endpoint="/process_graphs/{process_graph_id}",operation="GET",tag="graph_details") %>% 
    add_row(endpoint="/process_graphs/{process_graph_id}",operation="PATCH",tag="graph_replace") %>% 
    add_row(endpoint="/process_graphs/{process_graph_id}",operation="DELETE",tag="graph_delete") %>% 
    add_row(endpoint="/files/{user_id}",operation="GET",tag="user_files") %>% 
    add_row(endpoint="/files/{user_id}/{path}",operation="GET",tag="user_file_download") %>%
    add_row(endpoint="/files/{user_id}/{path}",operation="PUT",tag="user_file_upload") %>%
    add_row(endpoint="/files/{user_id}/{path}",operation="DELETE",tag="user_file_delete") %>%
    add_row(endpoint="/credentials/basic",operation="GET",tag="login") %>%
    add_row(endpoint="/credentials/oidc",operation="GET",tag="oidc_login") %>%
    add_row(endpoint="/preview",operation="POST",tag="execute_sync") %>%
    add_row(endpoint="/jobs",operation="GET",tag="user_jobs") %>%
    add_row(endpoint="/jobs",operation="POST",tag="jobs_define") %>%
    add_row(endpoint="/jobs/{job_id}",operation="PATCH",tag="jobs_update") %>%
    add_row(endpoint="/jobs/{job_id}",operation="GET",tag="jobs_details") %>%
    add_row(endpoint="/jobs/{job_id}",operation="DELETE",tag="jobs_delete") %>%
    add_row(endpoint="/jobs/{job_id}/estimate",operation="GET",tag="jobs_cost_estimation") %>%
    add_row(endpoint="/jobs/{job_id}/results",operation="POST",tag="execute_async") %>%
    add_row(endpoint="/jobs/{job_id}/results",operation="DELETE",tag="jobs_cancel") %>%
    add_row(endpoint="/jobs/{job_id}/results",operation="GET",tag="jobs_download") %>%
    add_row(endpoint="/subscribe",operation="PATCH",tag="jobs_log") %>%
    add_row(endpoint="/services",operation="GET",tag="user_services") %>%
    add_row(endpoint="/services",operation="POST",tag="service_publish") %>%
    add_row(endpoint="/services/{service_id}",operation="GET",tag="services_details") %>%
    add_row(endpoint="/services/{service_id}",operation="PATCH",tag="services_update") %>%
    add_row(endpoint="/services/{service_id}",operation="DELETE",tag="services_delete") %>%
    add_row(endpoint="/me",operation="GET",tag="user_info") %>%
    add_row(endpoint="/validation",operation="POST",tag="process_graph_validate")
    
  return(api)
}

api.v0.4.1 = function() {
  api = tibble(endpoint="/",operation="GET",tag="capabilities")
  
  api = api %>% 
    add_row(endpoint="/output_formats",operation="GET",tag="formats") %>% 
    add_row(endpoint="/service_types",operation="GET",tag="ogc_services") %>% 
    add_row(endpoint="/udf_runtimes",operation="GET",tag="udf_runtimes") %>%
    add_row(endpoint="/collections",operation="GET",tag="data_overview") %>% 
    add_row(endpoint="/collections/{collection_id}",operation="GET",tag="data_details") %>% 
    add_row(endpoint="/processes",operation="GET",tag="process_overview") %>% 
    add_row(endpoint="/process_graphs",operation="GET",tag="graph_overview") %>% 
    add_row(endpoint="/process_graphs",operation="POST",tag="new_graph") %>% 
    add_row(endpoint="/process_graphs/{process_graph_id}",operation="GET",tag="graph_details") %>% 
    add_row(endpoint="/process_graphs/{process_graph_id}",operation="PATCH",tag="graph_replace") %>% 
    add_row(endpoint="/process_graphs/{process_graph_id}",operation="DELETE",tag="graph_delete") %>% 
    add_row(endpoint="/files/{user_id}",operation="GET",tag="user_files") %>% 
    add_row(endpoint="/files/{user_id}/{path}",operation="GET",tag="user_file_download") %>%
    add_row(endpoint="/files/{user_id}/{path}",operation="PUT",tag="user_file_upload") %>%
    add_row(endpoint="/files/{user_id}/{path}",operation="DELETE",tag="user_file_delete") %>%
    add_row(endpoint="/credentials/basic",operation="GET",tag="login") %>%
    add_row(endpoint="/credentials/oidc",operation="GET",tag="oidc_login") %>%
    add_row(endpoint="/result",operation="POST",tag="execute_sync") %>%
    add_row(endpoint="/jobs",operation="GET",tag="user_jobs") %>%
    add_row(endpoint="/jobs",operation="POST",tag="jobs_define") %>%
    add_row(endpoint="/jobs/{job_id}",operation="PATCH",tag="jobs_update") %>%
    add_row(endpoint="/jobs/{job_id}",operation="GET",tag="jobs_details") %>%
    add_row(endpoint="/jobs/{job_id}",operation="DELETE",tag="jobs_delete") %>%
    add_row(endpoint="/jobs/{job_id}/estimate",operation="GET",tag="jobs_cost_estimation") %>%
    add_row(endpoint="/jobs/{job_id}/results",operation="POST",tag="execute_async") %>%
    add_row(endpoint="/jobs/{job_id}/results",operation="DELETE",tag="jobs_cancel") %>%
    add_row(endpoint="/jobs/{job_id}/results",operation="GET",tag="jobs_download") %>%
    add_row(endpoint="/subscription",operation="GET",tag="jobs_log") %>%
    add_row(endpoint="/services",operation="GET",tag="user_services") %>%
    add_row(endpoint="/services",operation="POST",tag="service_publish") %>%
    add_row(endpoint="/services/{service_id}",operation="GET",tag="services_details") %>%
    add_row(endpoint="/services/{service_id}",operation="PATCH",tag="services_update") %>%
    add_row(endpoint="/services/{service_id}",operation="DELETE",tag="services_delete") %>%
    add_row(endpoint="/me",operation="GET",tag="user_info") %>%
    add_row(endpoint="/validation",operation="POST",tag="process_graph_validate")
  
  return(api)
}

endpoints_compare = function(offering,e2,o2) {
  e1 = offering$path
  o1 = offering$method
  
  if (startsWith(e1,"/")) {
    coll1 = unlist(strsplit(e1,split = "/"))[-1]
  } else {
    coll1 = unlist(strsplit(e1,split = "/"))
  }
  
  if (startsWith(e2,"/")) {
    coll2 = unlist(strsplit(e2,split = "/"))[-1]
  } else {
    coll2 = unlist(strsplit(e2,split = "/"))
  }  
  
  if (length(coll1) == 0) coll1=""
  if (length(coll2) == 0) coll2=""
  
  if (length(intersect(coll1,coll2) > 0)) {
    df12 = setdiff(coll1,coll2)
    df21 = setdiff(coll2,coll1)
    
    if (length(df12) != length(df21)) return(FALSE)
    
    varible_pattern = "^[\\{|<|\\[|%].*[\\}|>|\\]|%]$"
    
    df12_var = all(grepl(varible_pattern,df12))
    df21_var = all(grepl(varible_pattern,df21))
    
    return(df12_var && df21_var && o1 == o2)
  }
  
  return(FALSE)
}

endpoint_mapping = function(con) {
  server_offering = con %>% listCapabilities()
  api = api.v0.4.1()
  
  mapping = api %>% rowwise() %>% summarise(endpoint,operation,tag,available = tibble(endpoint,operation) %>% (function(row){
    evaluation = c()
    
    for (i in 1:nrow(server_offering)) {
      evaluation = c(evaluation, endpoints_compare(server_offering[i,],endpoint[[1]], operation[[1]]))
    }
    # evaluation = unname(sapply(server_offering,endpoints_compare,endpoint[[1]],operation[[1]]))
    # add also operation[[1]] = server_offering$method [value]
    if (any(evaluation)) {
      return (list(list(available=TRUE,backend_endpoint=server_offering$path[evaluation])))
    } else {
      return(list(list(available=FALSE,backend_endpoint=NA_character_)))
    }
  }))
  
  mapping = mapping %>%
    rowwise() %>%
    summarise(endpoint,operation,tag,
              backend_endpoint = available
              %>% (function(l)l$backend_endpoint),
              available = available
              %>% (function(l)l$available))
  
  return(mapping)
  
}

replace_endpoint_parameter = function(endpoint, ...) {
  if (startsWith(endpoint,"/")) {
    coll = unlist(strsplit(endpoint,split = "/"))[-1]
  } else {
    coll = unlist(strsplit(endpoint,split = "/"))
  }
  
  endsWithSlash = endsWith(endpoint,"/")
  
  # get parameter
  variable_pattern = "^[\\{|<|\\[|%].*[\\}|>|\\]|%]$"
  
  param_names = grepl(variable_pattern,coll)
  
  params = list(...)
  # replace those parameter by given ... parameter (based on order)
  coll[param_names] = params
  
  if (endsWithSlash) {
    return(paste(coll,collapse="",sep="","/"))
  } else {
    return(paste(coll,collapse = "/",sep=""))
  }
  
}

#' Tag support lookup
#' 
#' Looks up the client tag for a particular endpoint on the back-end and returns whether it is available
#' or not.
#' 
#' @param con backend connection
#' @param tag the endpoints "tag" name as character
#' @return logical - whether the back-end supports the endpoint or not
#' 
#' @export
supports = function(con, tag_name) {
  return(con$api.mapping %>% filter(tag == tag_name) %>% select(available) %>% unlist %>% unname)
}