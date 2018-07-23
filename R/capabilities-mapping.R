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
    add_row(endpoint="/users/{user_id}/files/{path}",operation="DELETE",tag="user_file_upload") %>%
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

endpoints_compare = function(e1,e2) {
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
  
  if (length(intersect(coll1,coll2) > 0)) {
    df12 = setdiff(coll1,coll2)
    df21 = setdiff(coll2,coll1)
    
    if (length(df12) != length(df21)) return(FALSE)
    
    varible_pattern = "^[\\{<%\\[].*[\\}>%\\]$"
    
    df12_var = all(grepl(varible_pattern,df12))
    df21_var = all(grepl(varible_pattern,df21))
    
    return(df12_var && df21_var)
  }
  
  return(FALSE)
}

endpoint_mapping = function(con) {
  capabilities_list = listCapabilities(con)
  api = api.v0.0.2()
  
  mapping = api %>% rowwise() %>% summarise(endpoint,operation,tag,available = tibble(endpoint) %>% (function(endpoint){
    evaluation = unname(sapply(capabilities_list$endpoints,endpoints_compare,endpoint[[1]]))
    if (any(evaluation)) {
      return (list(list(available=TRUE,backend_endpoint=capabilities_list$endpoints[evaluation])))
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