prepare_oidc_providers = function() {
  tmp = jsonlite::fromJSON('{
    "providers": [
        {
            "description": "Identity Provider supported in this back-end.",
            "id": "google",
            "issuer": "https://accounts.google.com",
            "scopes": [
                "openid",
                "email"
            ],
            "title": "Google"
        },
        {
            "default_clients": [
                {
                    "grant_types": [
                        "authorization_code+pkce",
                        "urn:ietf:params:oauth:grant-type:device_code+pkce",
                        "refresh_token"
                    ],
                    "id": "xsdf",
                    "redirect_urls": [
                        "https://some.url.org",
                        "https://another.url.eu/",
                        "http://localhost:9999/",
                        "http://localhost"
                    ]
                }
            ],
            "description": "Federated OIDC",
            "id": "egi",
            "issuer": "https://some.issuer.com/auth/realms/egi",
            "scopes": [
                "openid",
                "email",
                "eduperson_entitlement",
                "eduperson_scoped_affiliation"
            ],
            "title": "EGI Check-in"
        },
        {
            "default_clients": [
                {
                    "grant_types": [
                        "authorization_code+pkce",
                        "urn:ietf:params:oauth:grant-type:device_code+pkce",
                        "refresh_token"
                    ],
                    "id": "xsdf",
                    "redirect_urls": [
                        "https://some.url.org",
                        "http://localhost:9999/",
                        "http://localhost"
                    ]
                }
            ],
            "description": "Federated OIDC",
            "id": "egi-legacy",
            "issuer": "https://old.issuer.com/oidc/",
            "scopes": [
                "openid",
                "email",
                "eduperson_entitlement",
                "eduperson_scoped_affiliation"
            ],
            "title": "EGI Check-in (legacy)"
        }
    ]
}',simplifyDataFrame=FALSE)
  tmp = lapply(tmp$providers, function(provider) {
    class(provider) = "Provider"
    return(provider)
  })
  names = sapply(tmp,function(x)x$id)
  class(tmp) = "ProviderList"
  
  names(tmp) = names
  return(tmp)
}

test_that("default OIDC provider selection with default clients works", {
  provider = NULL
  oidc_providers = prepare_oidc_providers()
  
  provider = .get_oidc_provider(provider=provider, oidc_providers = oidc_providers)
  expect(provider$id == "egi",failure_message = "a different default client was selected")
})


test_that("specific OIDC provider is selected as provider works", {
  
  oidc_providers = prepare_oidc_providers()
  provider = oidc_providers$`egi-legacy`
  
  provider = .get_oidc_provider(provider=provider, oidc_providers = oidc_providers)
  expect(provider$id == "egi-legacy",failure_message = "a different client was selected")
})

test_that("specific OIDC provider is selected as character works", {
  
  oidc_providers = prepare_oidc_providers()
  provider = "google"
  
  provider = .get_oidc_provider(provider=provider, oidc_providers = oidc_providers)
  expect(provider$id == "google",failure_message = "a different client was selected")
})

test_that("specific OIDC provider not available throws error", {
  
  oidc_providers = prepare_oidc_providers()
  provider = "googleR"
  
  expect_error({
    provider = .get_oidc_provider(provider=provider, oidc_providers = oidc_providers)
  }, regexp = "provider '.*' is not supported")
})

test_that("specific OIDC provider as not available Provider throws error", {
  
  oidc_providers = prepare_oidc_providers()
  provider = list(id="googleR")
  class(provider) = "Provider"
  
  expect_error({
    provider = .get_oidc_provider(provider=provider, oidc_providers = oidc_providers)
  }, label = "unsupported Provider does not raise an error")
})

test_that("empty OIDC provider throws error works", {
  
  oidc_providers = list()
  provider = "googleR"
  
  expect_error({
    provider = .get_oidc_provider(provider=provider, oidc_providers = oidc_providers)
  }, label = "empty provider list does not raise an error",regexp = "OIDC provider list .* is empty")
})

test_that("wrong parameter for provider throws error works", {
  
  oidc_providers = prepare_oidc_providers()
  provider = 1
  
  expect_error({
    provider = .get_oidc_provider(provider=provider, oidc_providers = oidc_providers)
  }, label = "unsupported type for provider does not raise an error",regexp = "[U|u]nsupported type")
})

test_that("default selection fails when no default_client is specified", {
  
  oidc_providers = prepare_oidc_providers()
  oidc_providers[[2]]$default_clients =NULL
  oidc_providers[[3]]$default_clients =NULL
  provider = NULL
  
  expect_error({
    provider = .get_oidc_provider(provider=provider, oidc_providers = oidc_providers)
  },regexp = "require additional configuration")
})