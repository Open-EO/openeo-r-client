test_that("test against local KeyCloak instance for client credentials", {
  testthat::skip("only for manual use")
  
  # this test requires a lot of setup you need to deploy a authentication server or have one at hand
  # you might have a look at this tutorial to setup a local keycloak instance with docker
  # https://medium.com/@disa2aka/docker-deployments-for-keycloak-and-postgresql-e75707b155e5
  # then setup a realm and create a client and check "Client authentication" and "Service accounts roles"
  
  issuer = ""
  client_id = ""
  secret = ""
  
  provider = list(id=client_id,
                  issuer=issuer,
                  scopes=c("openid","email"),
                  title="Local Keycloak")
  
  class(provider) = "Provider"
  
  expect_silent({
    auth = OIDCClientCredentialsFlow$new(provider=provider, config = list(client_id=client_id,secret=secret))
    auth$login()
    token = auth$access_token
  })
  
  expect(startsWith(token,paste0("oidc/",client_id,"/")),failure_message = "Token does not start with the required prefix")
})