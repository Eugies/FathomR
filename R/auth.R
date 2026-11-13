
# ============================================================
# FathomR Authentication
# ============================================================


#' Authenticate with Fathom
#'
#' Authenticates a user through AWS Cognito and returns an
#' access token.
#'
#' @param username Fathom account email.
#' @param password Fathom account password.
#' @param client_id Cognito application client ID.
#'
#' @return Cognito access token.
#' @export

fathom_authenticate <- function(
    username,
    password,
    client_id = "52mpprcnrect5cpkbs0pb86f4f"
) {

  url <- "https://cognito-idp.us-east-1.amazonaws.com/"

  body1 <- list(
    AuthParameters = list(
      USERNAME = username,
      PASSWORD = password
    ),
    AuthFlow = "USER_PASSWORD_AUTH",
    ClientId = client_id
  )

  res1 <- httr::POST(
    url,
    httr::add_headers(
      `Content-Type` = "application/x-amz-json-1.1",
      `X-Amz-Target` =
        "AWSCognitoIdentityProviderService.InitiateAuth"
    ),
    body = jsonlite::toJSON(
      body1,
      auto_unbox = TRUE
    )
  )

  httr::stop_for_status(res1)

  content1 <- httr::content(
    res1,
    as = "text",
    encoding = "UTF-8"
  )

  j1 <- jsonlite::fromJSON(content1)

  # Successful authentication
  if (!is.null(j1$AuthenticationResult$AccessToken)) {
    return(j1$AuthenticationResult$AccessToken)
  }

  # Handle Cognito authentication challenge
  if (!is.null(j1$ChallengeName)) {

    cr <- list(
      USERNAME = username,
      PASSWORD = password
    )

    body2 <- list(
      ChallengeName = j1$ChallengeName,
      ClientId = client_id,
      Session = j1$Session,
      ChallengeResponses = cr
    )

    res2 <- httr::POST(
      url,
      httr::add_headers(
        `Content-Type` = "application/x-amz-json-1.1",
        `X-Amz-Target` =
          "AWSCognitoIdentityProviderService.RespondToAuthChallenge"
      ),
      body = jsonlite::toJSON(
        body2,
        auto_unbox = TRUE
      )
    )

    httr::stop_for_status(res2)

    content2 <- httr::content(
      res2,
      as = "text",
      encoding = "UTF-8"
    )

    j2 <- jsonlite::fromJSON(content2)

    if (!is.null(j2$AuthenticationResult$AccessToken)) {
      return(j2$AuthenticationResult$AccessToken)
    }

    stop("Cognito authentication challenge was not completed.")
  }

  if (!is.null(j1$message)) {
    stop("Authentication failed: ", j1$message)
  }

  stop("Authentication failed in Cognito.")
}


# ============================================================
# Authentication Wrapper
# ============================================================


#' Authenticate with Fathom and select workspace and study
#'
#' Prompts for Fathom credentials, authenticates through
#' Cognito, and allows selection of a workspace and study.
#'
#' @return A list containing:
#' \describe{
#'   \item{token}{Cognito access token}
#'   \item{ws_id}{Selected workspace ID}
#'   \item{study_id}{Selected study ID}
#' }
#' @export

authenticate_wrapper <- function() {

  # ----------------------------------------------------------
  # USER CREDENTIALS
  # ----------------------------------------------------------

  username <- readline("Fathom email: ")

  password <- rstudioapi::askForPassword(
    "Fathom password:"
  )

  # Default Cognito client ID
  client_id <- "52mpprcnrect5cpkbs0pb86f4f"


  # ----------------------------------------------------------
  # AUTHENTICATE
  # ----------------------------------------------------------

  message("Authenticating with Fathom...")

  token <- fathom_authenticate(
    username = username,
    password = password,
    client_id = client_id
  )

  message("Authentication successful.")


  # ----------------------------------------------------------
  # SELECT WORKSPACE
  # ----------------------------------------------------------

  ws <- get_workspaces(token)

  if (is.null(ws) || nrow(ws) == 0) {
    stop("No workspaces were returned for this account.")
  }

  cat("\nAvailable workspaces:\n\n")

  print(ws)

  workspace_name <- readline(
    "\nWorkspace name (exact): "
  )

  ws_match <- which(ws$name == workspace_name)

  if (length(ws_match) != 1) {
    stop(
      paste0(
        "Workspace '",
        workspace_name,
        "' not found or is not unique."
      ),
      call. = FALSE
    )
  }

  ws_id <- ws$id[ws_match]

  message(
    "Workspace selected: ",
    workspace_name
  )


  # ----------------------------------------------------------
  # SELECT STUDY
  # ----------------------------------------------------------

  # IMPORTANT:
  # The selected workspace ID is passed to get_studies().
  # get_studies() uses the workspace-id HTTP header.

  studies <- get_studies(
    token = token,
    ws_id = ws_id
  )

  if (is.null(studies) || nrow(studies) == 0) {
    stop(
      "No studies were returned for workspace '",
      workspace_name,
      "'.",
      call. = FALSE
    )
  }

  cat("\nAvailable studies:\n\n")

  print(studies)

  study_name <- readline(
    "\nStudy name (exact): "
  )

  study_match <- which(studies$name == study_name)

  if (length(study_match) != 1) {
    stop(
      paste0(
        "Study '",
        study_name,
        "' not found or is not unique."
      ),
      call. = FALSE
    )
  }

  study_id <- studies$id[study_match]

  message(
    "Study selected: ",
    study_name
  )


  # ----------------------------------------------------------
  # RETURN AUTHENTICATION DETAILS
  # ----------------------------------------------------------

  auth <- list(
    token = token,
    ws_id = ws_id,
    study_id = study_id
  )

  return(auth)
}
