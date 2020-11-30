# make the model
source("make_model.r")
library(plumber)
library(tm)
library(openssl)

SLACK_SIGNING_SECRET <- ""

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
    openssl::sha256(base_string, SLACK_SIGNING_SECRET)
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

#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-",
      req$REQUEST_METHOD, req$PATH_INFO, "-",
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

#' return chatbot payments response
#' @serializer unboxedJSON
#' @param text
#' @post /payments
#' @response 200
function(text=" "){
  my_text <- text
  my_text <- gsub("[[:punct:]]", " ",my_text)
  answer <- paymentspred(my_text)
  return(
    list(
      # response type - ephemeral indicates the response will only be seen by the
      # user who invoked the slash command as opposed to the entire channel
      response_type = "ephemeral",
      type = "mrkdwn",
      text = paste0(":graetan: :rowboat: \n You asked: ","*",my_text,"*","\n",":dart: I found this question that sounds similar: ",answer,"\n","\n",
                    ":robot_face: *If my answer sucks, give me a better one for next time by filling in the form at the top of this channel* :arrow_up: :arrow_up:")))
}

#' return chatbot ebay response
#' @serializer unboxedJSON
#' @param text
#' @post /ebay
#' @response 200
function(text=" ") {
  my_text <- text
  my_text <- gsub("[[:punct:]]", " ",my_text)
  answer <- ebaypred(my_text)
  return(
    list(
      # response type - ephemeral indicates the response will only be seen by the
      # user who invoked the slash command as opposed to the entire channel
      response_type = "ephemeral",
      type = "mrkdwn",
      text = paste0(":graetan: :rowboat: \n You asked: ","*",my_text,"*","\n",":dart: I found this question that sounds similar: ",answer,"\n","\n",
                    ":robot_face: *If my answer sucks, give me a better one for next time by filling in the form at the top of this channel* :arrow_up: :arrow_up:")))
}

#' return chatbot design response
#' @serializer unboxedJSON
#' @param text
#' @post /design
#' @response 200
function(text=" ") {
  my_text <- text
  my_text <- gsub("[[:punct:]]", " ",my_text)
  answer <- designpred(my_text)
  return(
    list(
      # response type - ephemeral indicates the response will only be seen by the
      # user who invoked the slash command as opposed to the entire channel
      response_type = "ephemeral",
      type = "mrkdwn",
      text = paste0(":graetan: :rowboat: \n You asked: ","*",my_text,"*","\n",":dart: I found this question that sounds similar: ",answer,"\n","\n",
                    ":robot_face: *If my answer sucks, give me a better one for next time by filling in the form at the top of this channel* :arrow_up: :arrow_up:")))
}

#' return chatbot amazon response
#' @serializer unboxedJSON
#' @param text
#' @post /amazon
#' @response 200
function(text=" ") {
  my_text <- text
  my_text <- gsub("[[:punct:]]", " ",my_text)
  answer <- amazonpred(my_text)
  return(
    list(
      # response type - ephemeral indicates the response will only be seen by the
      # user who invoked the slash command as opposed to the entire channel
      response_type = "ephemeral",
      type = "mrkdwn",
      text = paste0(":graetan: :rowboat: \n You asked: ","*",my_text,"*","\n",":dart: I found this question that sounds similar: ",answer,"\n","\n",
                    ":robot_face: *If my answer sucks, give me a better one for next time by filling in the form at the top of this channel* :arrow_up: :arrow_up:")))
}

#' return chatbot kogan response
#' @serializer unboxedJSON
#' @param text
#' @post /kogan
#' @response 200
function(text=" ") {
  my_text <- text
  my_text <- gsub("[[:punct:]]", " ",my_text)
  answer <- koganpred(my_text)
  return(
    list(
      # response type - ephemeral indicates the response will only be seen by the
      # user who invoked the slash command as opposed to the entire channel
      response_type = "ephemeral",
      type = "mrkdwn",
      text = paste0(":graetan: :rowboat: \n You asked: ","*",my_text,"*","\n",":dart: I found this question that sounds similar: ",answer,"\n","\n",
                    ":robot_face: *If my answer sucks, give me a better one for next time by filling in the form at the top of this channel* :arrow_up: :arrow_up:")))
}

#' return chatbot xero response
#' @serializer unboxedJSON
#' @param text
#' @post /xero
#' @response 200
function(text=" ") {
  my_text <- text
  my_text <- gsub("[[:punct:]]", " ",my_text)
  answer <- xeropred(my_text)
  return(
    list(
      # response type - ephemeral indicates the response will only be seen by the
      # user who invoked the slash command as opposed to the entire channel
      response_type = "ephemeral",
      type = "mrkdwn",
      text = paste0(":graetan: :rowboat: \n You asked: ","*",my_text,"*","\n",":dart: I found this question that sounds similar: ",answer,"\n","\n",
                    ":robot_face: *If my answer sucks, give me a better one for next time by filling in the form at the top of this channel* :arrow_up: :arrow_up:")))
}

#' return chatbot myob response
#' @serializer unboxedJSON
#' @param text
#' @post /myob
#' @response 200
function(text=" ") {
  my_text <- text
  my_text <- gsub("[[:punct:]]", " ",my_text)
  answer <- myobpred(my_text)
  return(
    list(
      # response type - ephemeral indicates the response will only be seen by the
      # user who invoked the slash command as opposed to the entire channel
      response_type = "ephemeral",
      type = "mrkdwn",
      text = paste0(":graetan: :rowboat: \n You asked: ","*",my_text,"*","\n",":dart: I found this question that sounds similar: ",answer,"\n","\n",
                    ":robot_face: *If my answer sucks, give me a better one for next time by filling in the form at the top of this channel* :arrow_up: :arrow_up:")))
}

#' return chatbot data response
#' @serializer unboxedJSON
#' @param text
#' @post /data
#' @response 200
function(text=" ") {
  my_text <- text
  my_text <- gsub("[[:punct:]]", " ",my_text)
  answer <- datapred(my_text)
  return(
    list(
      # response type - ephemeral indicates the response will only be seen by the
      # user who invoked the slash command as opposed to the entire channel
      response_type = "ephemeral",
      type = "mrkdwn",
      text = paste0(":graetan: :rowboat: \n You asked: ","*",my_text,"*","\n",":dart: I found this question that sounds similar: ",answer,"\n","\n",
                    ":robot_face: *If my answer sucks, give me a better one for next time by filling in the form at the top of this channel* :arrow_up: :arrow_up:")))
}

#' return chatbot catch response
#' @serializer unboxedJSON
#' @param text
#' @post /catch
#' @response 200
function(text=" ") {
  my_text <- text
  my_text <- gsub("[[:punct:]]", " ",my_text)
  answer <- catchpred(my_text)
  return(
    list(
      # response type - ephemeral indicates the response will only be seen by the
      # user who invoked the slash command as opposed to the entire channel
      response_type = "ephemeral",
      type = "mrkdwn",
      text = paste0(":graetan: :rowboat: \n You asked: ","*",my_text,"*","\n",":dart: I found this question that sounds similar: ",answer,"\n","\n",
                    ":robot_face: *If my answer sucks, give me a better one for next time by filling in the form at the top of this channel* :arrow_up: :arrow_up:")))
}
