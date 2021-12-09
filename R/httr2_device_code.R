# until we make an official pull request for httr2 (https://github.com/r-lib/httr2) we use this code to extent the device code flow for pkce

oauth_flow_device_request = function (client, auth_url, scope, auth_params) {
  req <- request(auth_url)
  req <- req_body_form(req, list2(scope = scope, !!!auth_params))
  req <- httr2:::oauth_client_req_auth(req, client)
  req <- req_headers(req, Accept = "application/json")
  oauth_flow_fetch(req)
}

oauth_flow_fetch = function (req) {
  req <- req_error(req, is_error = ~FALSE)
  resp <- req_perform(req)
  if (resp_content_type(resp) == "application/json") {
    body <- resp_body_json(resp)
  } else {
    body <- NULL
  }
  
  # the function is also used in during the device code flow which requires to parse the intermediate device_code from the JSON body
  if ((has_name(body, "access_token") ||  has_name(body, "device_code")) && resp_status(resp) == 
      200) {
    return(body)
  } else if (has_name(body, "error")) {
    oauth_flow_abort(body$error, body$error_description, 
                     body$error_uri)
  }
  else { 
    resp_check_status(resp)
    abort("Failed to process response from 'token' endpoint")
  }
}

oauth_flow_device = function (client, auth_url, pkce=FALSE, scope = NULL, auth_params = list(), 
                              token_params = list()) {
  httr2:::oauth_flow_check("device", client, interactive = TRUE)
  
  if (pkce) { # added this
    code <- oauth_flow_auth_code_pkce()
    auth_params$code_challenge <- code$challenge
    auth_params$code_challenge_method <- code$method
    token_params$code_verifier <- code$verifier
  }
  
  request <- oauth_flow_device_request(client, auth_url, scope, 
                                       auth_params)
  
  # verification_uri_complete is optional, it would ship the user code in the uri https://datatracker.ietf.org/doc/html/rfc8628 ch. 3.2
  if (is_interactive() && has_name(request, "verification_uri_complete")) {
    inform(glue("Use code {request$user_code}"))
    utils::browseURL(request$verification_uri_complete)
  } else if (is_interactive() && has_name(request, "verification_uri")) {
    inform(glue("Use code {request$user_code}"))
    utils::browseURL(request$verification_uri)
  } else {
    url <- request$verification_uri %||% request$verification_url
    inform(glue("Visit <{url}> and enter code {request$user_code}"))
  }
  token <- httr2:::oauth_flow_device_poll(client, request, token_params)
  if (is.null(token)) {
    abort("Expired without user confirmation; please try again.")
  }
  exec(oauth_token, !!!token)
}