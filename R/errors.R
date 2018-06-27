# This script will contain functions that retrieve predefined error messages

.notConnected = function() {
  return("No host selected")
}

.notAuthorized = function() {
  return("REQUEST-ERROR: Not authorized to access URL or file.")
}

.notLoggedInOrExpired = function() {
  return("REQUEST-ERROR: Authentification required or expired. Please login to back-end.")
}