#* Verify incoming requests
#* @filter verify
function(req, res) {
  # Forward requests coming to swagger endpoints
  if (grepl("swagger", tolower(req$PATH_INFO))) return(forward())
  
  # Check for X_SLACK_REQUEST_TIMESTAMP header
  if (is.null(req$HTTP_X_SLACK_REQUEST_TIMESTAMP)) {
    res$status <- 401
  }
  
  # Build base string
  base_string <- paste(
    "v0",
    req$HTTP_X_SLACK_REQUEST_TIMESTAMP,
    req$postBody,
    sep = ":"
  )
  
  # Slack Signing secret is available as environment variable
  # SLACK_SIGNING_SECRET
  computed_request_signature <- paste0(
    "v0=",
    openssl::sha256(base_string, SIGNING_SECRET)
  )
  
  # If the computed request signature doesn't match the signature provided in the
  # request, set status of response to 401
  if (!identical(req$HTTP_X_SLACK_SIGNATURE, computed_request_signature)) {
    res$status <- 401
  } else {
    res$status <- 200
  }
  
  if (res$status == 401) {
    list(
      text = "Error: Invalid request"
    )
  } else {
    forward()
  }
}
