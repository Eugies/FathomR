#' Get spatial station locations from Fathom
#'
#' Retrieves station coordinates from deployment positions and returns
#' a dataframe of unique hydrophone locations. An optional spatial
#' extent can be used to filter the returned locations.
#'
#' @param token Fathom API access token.
#' @param ws_id Fathom workspace ID.
#' @param study_id Fathom study ID.
#' @param lon_min Minimum longitude.
#' @param lon_max Maximum longitude.
#' @param lat_min Minimum latitude.
#' @param lat_max Maximum latitude.
#'
#' @return A dataframe containing Station.name, Latitude, Longitude,
#'   and Type.
#'
#' @export

get_spatial <- function(
    token,
    ws_id,
    study_id,
    lon_min = NULL,
    lon_max = NULL,
    lat_min = NULL,
    lat_max = NULL
) {

  # Check spatial extent arguments
  extent <- c(lon_min, lon_max, lat_min, lat_max)

  if (any(!vapply(extent, is.null, logical(1))) &&
      any(vapply(extent, is.null, logical(1)))) {
    stop(
      "Please provide all four spatial extent arguments: ",
      "lon_min, lon_max, lat_min, and lat_max."
    )
  }

  if (!all(vapply(extent, is.null, logical(1)))) {

    if (!all(vapply(extent, is.numeric, logical(1)))) {
      stop("Spatial extent arguments must be numeric.")
    }

    if (lon_min >= lon_max || lat_min >= lat_max) {
      stop("Minimum coordinates must be smaller than maximum coordinates.")
    }

    if (lon_min < -180 || lon_max > 180 ||
        lat_min < -90 || lat_max > 90) {
      stop("Longitude or latitude values are outside valid coordinate ranges.")
    }
  }

  # GraphQL endpoint
  url <- "https://graph.fathomcentral.com/graphql"

  # GraphQL query
  query <- '
  query listDeployments($studyId: ID) {
    deployments(studyId: $studyId) {
      station {
        name
      }
      positions {
        latLon {
          latitude
          longitude
        }
      }
    }
  }'

  # Request body
  body <- list(
    query = query,
    variables = list(
      studyId = as.character(study_id)
    )
  )

  # Send request
  response <- httr::POST(
    url,
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", token),
      "workspace-id" = as.character(ws_id)
    ),
    body = body,
    encode = "json"
  )

  # Check HTTP status
  httr::stop_for_status(response)

  # Parse response
  result <- jsonlite::fromJSON(
    httr::content(response, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  # Check GraphQL errors
  if (!is.null(result$errors)) {
    stop(
      "GraphQL error: ",
      paste(
        vapply(result$errors, function(x) x$message, character(1)),
        collapse = "; "
      )
    )
  }

  # Extract deployments
  deployments <- result$data$deployments

  # Extract station coordinates
  spatial_list <- list()

  for (deployment in deployments) {

    station_name <- deployment$station$name

    # Skip deployments without station names or positions
    if (is.null(station_name) || is.null(deployment$positions)) {
      next
    }

    for (position in deployment$positions) {

      lat_lon <- position$latLon

      if (is.null(lat_lon) ||
          is.null(lat_lon$latitude) ||
          is.null(lat_lon$longitude)) {
        next
      }

      spatial_list[[length(spatial_list) + 1]] <- data.frame(
        Station.name = as.character(station_name),
        Latitude = as.numeric(lat_lon$latitude),
        Longitude = as.numeric(lat_lon$longitude),
        Type = "Hydrophone",
        stringsAsFactors = FALSE
      )
    }
  }

  # Combine results
  if (length(spatial_list) == 0) {
    spatial <- data.frame(
      Station.name = character(),
      Latitude = numeric(),
      Longitude = numeric(),
      Type = character()
    )
  } else {
    spatial <- do.call(rbind, spatial_list)
  }

  # Remove duplicate station-coordinate combinations
  spatial <- unique(spatial)

  # Filter by spatial extent if supplied
  if (!is.null(lon_min)) {

    spatial <- spatial[
      spatial$Longitude >= lon_min &
        spatial$Longitude <= lon_max &
        spatial$Latitude >= lat_min &
        spatial$Latitude <= lat_max,
    ]
  }

  # Reset row names
  rownames(spatial) <- NULL

  # Message
  message(
    "Spatial download complete: ",
    nrow(spatial),
    " hydrophone locations retrieved."
  )

  return(spatial)
}
