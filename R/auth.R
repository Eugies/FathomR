# ————————————————————————————————
# Authentication
# ————————————————————————————————

#' Authenticate to Fathom API
#'
#' Logs into the Fathom Cognito authentication system and returns an access token.
#'
#' @param username Fathom account email
#' @param password Fathom password
#' @param client_id AWS Cognito client ID
#'
#' @return Access token string
#' @export
fathom_authenticate <- function(username, password, client_id) {

  url <- "https://cognito-idp.us-east-1.amazonaws.com/"

  body <- list(
    AuthParameters = list(
      USERNAME = username,
      PASSWORD = password
    ),
    AuthFlow = "USER_PASSWORD_AUTH",
    ClientId = client_id
  )

  res <- httr::POST(
    url,
    httr::add_headers(
      `Content-Type` = "application/x-amz-json-1.1",
      `X-Amz-Target` = "AWSCognitoIdentityProviderService.InitiateAuth"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )

  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  j <- jsonlite::fromJSON(txt)

  if (!is.null(j$AuthenticationResult$AccessToken)) {
    return(j$AuthenticationResult$AccessToken)
  }

  if (!is.null(j$ChallengeName)) {

    challenge_body <- list(
      ChallengeName = j$ChallengeName,
      ClientId = client_id,
      Session = j$Session,
      ChallengeResponses = list(
        USERNAME = username,
        PASSWORD = password
      )
    )

    res2 <- httr::POST(
      url,
      httr::add_headers(
        `Content-Type` = "application/x-amz-json-1.1",
        `X-Amz-Target` = "AWSCognitoIdentityProviderService.RespondToAuthChallenge"
      ),
      body = jsonlite::toJSON(challenge_body, auto_unbox = TRUE)
    )

    j2 <- jsonlite::fromJSON(httr::content(res2, as = "text", encoding = "UTF-8"))

    if (!is.null(j2$AuthenticationResult$AccessToken)) {
      return(j2$AuthenticationResult$AccessToken)
    }
  }

  stop("Authentication failed in Cognito")
}


#' Interactive authentication wrapper
#'
#' Prompts user for credentials and returns token + workspace ID.
#'
#' @export
authenticate_wrapper <- function() {

  username <- readline("Fathom email: ")

  use_default <- readline("Use default client ID? (y/n): ")

  client_id <- if (tolower(use_default) == "y") {
    "52mpprcnrect5cpkbs0pb86f4f"
  } else {
    readline("Client ID: ")
  }

  password <- rstudioapi::askForPassword("Fathom password:")

  token <- fathom_authenticate(username, password, client_id)

  ws <- get_workspaces(token)

  print(ws)

  name <- readline("Workspace name (exact): ")

  ws_id <- ws$id[ws$name == name]

  list(
    token = token,
    ws_id = ws_id
  )
}
