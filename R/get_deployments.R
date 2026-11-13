#' Download receiver deployments from Fathom Central
#'
#' Retrieves receiver deployment records for a selected study.
#'
#' @param token Fathom API access token.
#' @param ws_id Workspace ID.
#' @param study_id Study ID.
#'
#' @return A data frame with Receiver, Station.name, Start, and Stop.
#' @export

get_deployments <- function(
    token,
    ws_id,
    study_id
) {

  # ------------------------------------------------------------
  # Validate inputs
  # ------------------------------------------------------------

  if (missing(token) || is.null(token) || !nzchar(token)) {
    stop("A valid authentication token must be provided.")
  }

  if (missing(ws_id) || is.null(ws_id) ||
      !nzchar(as.character(ws_id))) {
    stop("A valid workspace ID must be provided.")
  }

  if (missing(study_id) || is.null(study_id) ||
      !nzchar(as.character(study_id))) {
    stop("A valid study ID must be provided.")
  }


  # ------------------------------------------------------------
  # Helper function for extracting single values
  # ------------------------------------------------------------

  get_value <- function(x, name) {

    value <- x[[name]]

    if (is.null(value) || length(value) == 0) {
      return(NA_character_)
    }

    as.character(value[[1]])

  }


  # ------------------------------------------------------------
  # GraphQL query
  # ------------------------------------------------------------

  deployment_query <- paste(
    "query listDeployments($studyId: ID) {",
    "  deployments(studyId: $studyId) {",
    "    id",
    "    start",
    "    end",
    "    station {",
    "      name",
    "      id",
    "    }",
    "    deviceAttachments {",
    "      id",
    "      device {",
    "        id",
    "        serial",
    "        model",
    "        deviceClasses",
    "      }",
    "      start",
    "      end",
    "    }",
    "  }",
    "}"
  )


  # ------------------------------------------------------------
  # Send GraphQL request
  # ------------------------------------------------------------

  request <- list(
    query = deployment_query,
    variables = list(
      studyId = as.character(study_id)
    )
  )

  response <- httr::POST(
    url = "https://graph.fathomcentral.com/graphql",

    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", token),
      "workspace-id" = as.character(ws_id)
    ),

    body = jsonlite::toJSON(
      request,
      auto_unbox = TRUE,
      null = "null",
      na = "null"
    ),

    encode = "raw",
    httr::timeout(120)
  )


  # ------------------------------------------------------------
  # Check response
  # ------------------------------------------------------------

  if (httr::http_error(response)) {

    stop(
      "Fathom API request failed: HTTP ",
      httr::status_code(response),
      "\n",
      httr::content(
        response,
        as = "text",
        encoding = "UTF-8"
      )
    )

  }

  result <- jsonlite::fromJSON(
    httr::content(
      response,
      as = "text",
      encoding = "UTF-8"
    ),
    simplifyVector = FALSE
  )


  if (!is.null(result$errors)) {

    error_messages <- vapply(
      result$errors,
      function(x) x$message,
      character(1)
    )

    stop(
      "GraphQL error: ",
      paste(error_messages, collapse = "; ")
    )

  }

  if (is.null(result$data$deployments)) {
    stop("No deployment data was returned by the Fathom API.")
  }


  # ------------------------------------------------------------
  # Extract receiver deployments
  # ------------------------------------------------------------

  deployments_raw <- result$data$deployments

  deployment_rows <- list()

  row_index <- 0

  for (i in seq_along(deployments_raw)) {

    deployment <- deployments_raw[[i]]

    station_name <- get_value(
      deployment$station,
      "name"
    )

    attachments <- deployment$deviceAttachments

    if (length(attachments) == 0) {
      next
    }

    for (j in seq_along(attachments)) {

      attachment <- attachments[[j]]

      device <- attachment$device

      receiver <- get_value(
        device,
        "serial"
      )

      row_index <- row_index + 1

      deployment_rows[[row_index]] <- data.frame(

        Receiver = receiver,

        Station.name = station_name,

        Start = get_value(deployment, "start"),

        Stop = get_value(deployment, "end"),

        stringsAsFactors = FALSE

      )

    }

  }


  # ------------------------------------------------------------
  # Combine deployment records
  # ------------------------------------------------------------

  if (length(deployment_rows) == 0) {

    message("No receiver deployments were returned.")

    return(
      data.frame(
        Receiver = character(),
        Station.name = character(),
        Start = as.POSIXct(character(), tz = "UTC"),
        Stop = as.POSIXct(character(), tz = "UTC")
      )
    )

  }

  deployments <- do.call(
    rbind,
    deployment_rows
  )

  rownames(deployments) <- NULL


  # ------------------------------------------------------------
  # Format deployment dates
  # ------------------------------------------------------------

  deployments$Start <- as.POSIXct(
    deployments$Start,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )

  deployments$Stop <- as.POSIXct(
    deployments$Stop,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )


  # ------------------------------------------------------------
  # Return results
  # ------------------------------------------------------------

  message(
    "Deployment download complete: ",
    nrow(deployments),
    " receiver deployments retrieved."
  )

  return(deployments)

}
