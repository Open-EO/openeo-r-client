test_that(".get_client works with openeo client_id", {
  default_clients = list(list(id="sh-123wd-ws3fs5gf",
                              grant_types=list("authorization_code+pkce",
                                              "urn:ietf:params:oauth:grant-type:device_code+pkce",
                                              "refresh_token"),
                              redirect_urls=list()),
                         list(id = "some-oidc-openeo-client",
                              grant_types=list("authorization_code+pkce",
                              "urn:ietf:params:oauth:grant-type:device_code+pkce",
                              "refresh_token"),
                              redirect_urls=list()))
  
  c=.get_client(default_clients,grant="urn:ietf:params:oauth:grant-type:device_code+pkce",config=list())
  
  expect_equal(object = c$client_id,expected = "some-oidc-openeo-client",label="openeo client is selected")
})


test_that(".get_client works with compatible first select", {
  default_clients = list(list(id="sh-123wd-ws3fs5gf",
                              grant_types=list("authorization_code+pkce",
                                               "urn:ietf:params:oauth:grant-type:device_code+pkce",
                                               "refresh_token"),
                              redirect_urls=list()),
                         list(id = "some-oidc-client",
                              grant_types=list("authorization_code+pkce",
                                               "urn:ietf:params:oauth:grant-type:device_code+pkce",
                                               "refresh_token"),
                              redirect_urls=list()))
  
  c=.get_client(default_clients,grant="urn:ietf:params:oauth:grant-type:device_code+pkce",config=list())
  
  expect_equal(object = c$client_id,expected = "sh-123wd-ws3fs5gf",label="first client is selected")
})

test_that(".get_client fails with non compatible", {
  default_clients = list(list(id="sh-123wd-ws3fs5gf",
                              grant_types=list("foo+bar",
                                               "refresh_token"),
                              redirect_urls=list()),
                         list(id = "some-oidc-client",
                              grant_types=list("foo+bar",
                                               "refresh_token"),
                              redirect_urls=list()))
  
  c=.get_client(default_clients,grant="urn:ietf:params:oauth:grant-type:device_code+pkce",config=list())
  
  expect_null(object = c$client_id,label="first client is selected")
})
