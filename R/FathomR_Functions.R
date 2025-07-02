#' @importFrom httr POST add_headers content stop_for_status
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom rstudioapi askForPassword
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate group_by n_distinct transmute
#' @importFrom tibble tibble
#' @importFrom purrr map map_chr map_dfr
#' @importFrom stringr str_detect str_extract
#' @importFrom lubridate ymd_hms
#' @importFrom stats na.omit
#' @importFrom rlang %||%
#' @importFrom utils head
NULL

# ————————————————————————————————
# 1. Authentication via Cognito
# ————————————————————————————————
#' @export
fathom_authenticate <- function(username, password, client_id) {
  url <- "https://cognito-idp.us-east-1.amazonaws.com/"

  body1 <- list(
    AuthParameters = list(USERNAME = username, PASSWORD = password),
    AuthFlow = "USER_PASSWORD_AUTH",
    ClientId = client_id
  )

  res1 <- POST(
    url,
    add_headers(
      `Content-Type` = "application/x-amz-json-1.1",
      `X-Amz-Target` = "AWSCognitoIdentityProviderService.InitiateAuth"
    ),
    body = toJSON(body1, auto_unbox = TRUE)
  )
  content1 <- content(res1, as = "text", encoding = "UTF-8")
  j1 <- fromJSON(content1)

  if (!is.null(j1$AuthenticationResult$AccessToken)) {
    return(j1$AuthenticationResult$AccessToken)
  } else if (!is.null(j1$ChallengeName)) {
    cr <- list(USERNAME = username, PASSWORD = password)
    body2 <- list(
      ChallengeName = j1$ChallengeName,
      ClientId = client_id,
      Session = j1$Session,
      ChallengeResponses = cr
    )
    res2 <- POST(
      url,
      add_headers(
        `Content-Type` = "application/x-amz-json-1.1",
        `X-Amz-Target` = "AWSCognitoIdentityProviderService.RespondToAuthChallenge"
      ),
      body = toJSON(body2, auto_unbox = TRUE)
    )
    j2 <- fromJSON(content(res2, as = "text", encoding = "UTF-8"))
    if (!is.null(j2$AuthenticationResult$AccessToken)) {
      return(j2$AuthenticationResult$AccessToken)
    }
  }

  stop("Authentication failed in Cognito")
}

# Wrapper prompting user
#' @export
authenticate_wrapper <- function() {
  username <- readline("Fathom email: ")
  use_default <- readline("Use default client ID? (y/n): ")
  client_id <- if (tolower(use_default) == "y") "52mpprcnrect5cpkbs0pb86f4f" else readline("Client ID: ")
  password <- rstudioapi::askForPassword("Fathom password:")
  token <- fathom_authenticate(username, password, client_id)
  ws <- get_workspaces(token)
  print(ws)
  name <- readline("Workspace name (exact): ")
  ws_id <- ws$id[ws$name == name]
  list(token = token, ws_id = ws_id)
}

# ————————————————————————————————
# 2. Fetching workspace list
# ————————————————————————————————
#' @export
get_workspaces <- function(token) {
  res <- POST(
    "https://graph.fathomcentral.com/graphql",
    add_headers(
      "Content-Type" = "application/json",
      Authorization = paste("Bearer", token)
    ),
    body = toJSON(list(query = "query { workspaces { id name } }"), auto_unbox = TRUE)
  )
  stop_for_status(res)
  fromJSON(content(res, "text"))$data$workspaces
}

# ————————————————————————————————
# 3. Fetch RAW Biometrics listed
# ————————————————————————————————
#' @export
get_RAW_biometrics <- function(token = NULL, ws_id = NULL) {
  if (is.null(token) || is.null(ws_id)) {
    auth <- authenticate_wrapper()
    token <- auth$token
    ws_id <- auth$ws_id
  }

  query <- '
    query {
      animals {
        name
        speciesCommonName
        speciesScientificName
        devices {
          serial
          model
          transmitters {
            displayId
          }
        }
        events {
          __typename
          time
          locationName
          researcherName
          latLon {
            latitude
            longitude
          }
        }
        measurementSets {
          totalLength { value unit }
          forkLength { value unit }
          mass { value unit }
          sex
          lifeStage
          time
        }
      }
    }
  '

  res <- POST(
    "https://graph.fathomcentral.com/graphql",
    add_headers(
      "Content-Type" = "application/json",
      Authorization = paste("Bearer", token),
      `workspace-id` = ws_id
    ),
    body = toJSON(list(query = query), auto_unbox = TRUE)
  )
  stop_for_status(res)

  animals <- fromJSON(content(res, "text"), flatten = TRUE)$data$animals
  if (is.data.frame(animals)) animals <- split(animals, seq_len(nrow(animals)))

  tibble(
    Nickname = map_chr(animals, ~ .x$name %||% NA_character_),
    CommonName = map_chr(animals, ~ .x$speciesCommonName %||% NA_character_),
    Species = map_chr(animals, ~ .x$speciesScientificName %||% NA_character_),
    Devices = map(animals, ~ .x$devices),
    MeasurementSets = map(animals, ~ .x$measurementSets),
    Events = map(animals, ~ .x$events)
  )
}
# —————————————————————————————————————————
# get_biometrics (unlisted)
# —————————————————————————————————————————
#' Get Biometrics for Tagged Animals
#'
#' Retrieves biometric metadata for all animals and their associated tags in the Fathom workspace.
#'
#' @param token Optional. Fathom API token. If `NULL`, `authenticate_wrapper()` is called.
#' @param ws_id Optional. Workspace ID. If `NULL`, `authenticate_wrapper()` is called.
#'
#' @return A tibble with biometric metadata for each tagged animal. Includes fields like:
#' \describe{
#'   \item{CommonName}{Species common name.}
#'   \item{AnimalId}{Unique ID of the animal.}
#'   \item{Sex}{Sex of the animal (if recorded).}
#'   \item{Length}{Length of the animal (if recorded).}
#'   \item{Weight}{Weight of the animal (if recorded).}
#'   \item{Devices}{List-column with device and tag information.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' auth <- authenticate_wrapper()
#' biometrics <- get_biometrics(token = auth$token, ws_id = auth$ws_id)
#' head(biometrics)
#' }
get_biometrics <- function(token = NULL, ws_id = NULL) {
  if (is.null(token) || is.null(ws_id)) {
    auth <- authenticate_wrapper()
    token <- auth$token
    ws_id <- auth$ws_id
  }

  query <- '
    query {
      animals {
        name
        speciesCommonName
        speciesScientificName
        devices {
          serial
          model
          transmitters {
            displayId
          }
        }
        events {
          __typename
          time
          locationName
          researcherName
          latLon {
            latitude
            longitude
          }
        }
        measurementSets {
          totalLength { value unit }
          forkLength { value unit }
          mass { value unit }
          sex
          lifeStage
          time
        }
      }
    }
  '

  res <- POST(
    "https://graph.fathomcentral.com/graphql",
    add_headers(
      "Content-Type" = "application/json",
      Authorization = paste("Bearer", token),
      `workspace-id` = ws_id
    ),
    body = toJSON(list(query = query), auto_unbox = TRUE)
  )
  stop_for_status(res)

  animals <- fromJSON(content(res, "text"), flatten = TRUE)$data$animals
  if (is.data.frame(animals)) animals <- split(animals, seq_len(nrow(animals)))

  #' @export
  flatten_transmitters_with_biometrics <- function(row) {
    nickname <- row$Nickname
    common_name <- row$CommonName
    species <- row$Species

    devices_list <- row$Devices[[1]]

    measurement_set <- NULL
    if (is.list(row$MeasurementSets) && length(row$MeasurementSets) > 0) {
      inner_list <- row$MeasurementSets[[1]]
      if (is.list(inner_list) && length(inner_list) > 0 && is.data.frame(inner_list[[1]])) {
        measurement_set <- inner_list[[1]]
      }
    }

    tagging_events_list <- row$Events[[1]]
    tagging_events <- NULL
    if (length(tagging_events_list) > 0) {
      tagging_events <- tagging_events_list[[1]]
    }

    if (!is.list(devices_list) || length(devices_list) == 0) return(tibble())
    if (!is.data.frame(tagging_events)) tagging_events <- tibble()

    tagging_event <- tagging_events %>%
      filter(`__typename` == "AnimalEventTagging") %>%
      dplyr::slice_head(n = 1)

    tagging_time_str <- tagging_event$time %||% NA_character_
    tagging_time <- if (!is.na(tagging_time_str)) ymd_hms(tagging_time_str, tz = "UTC") else NA
    tagging_location <- tagging_event$locationName %||% NA_character_
    tagging_lat <- tagging_event$latLon.latitude %||% NA_real_
    tagging_lon <- tagging_event$latLon.longitude %||% NA_real_
    tagged_by <- tagging_event$researcherName %||% NA_character_

    release_event <- tagging_events %>%
      dplyr::filter(`__typename` == "AnimalEventRelease") %>%
      dplyr::slice_head(n = 1)

    release_time_str <- release_event$time %||% NA_character_
    release_time <- if (!is.na(release_time_str)) ymd_hms(release_time_str, tz = "UTC") else NA
    release_location <- release_event$locationName %||% NA_character_
    release_lat <- release_event$latLon.latitude %||% NA_real_
    release_lon <- release_event$latLon.longitude %||% NA_real_

    release_length <- release_event$totalLength.value %||% NA_real_
    release_length_unit <- release_event$totalLength.unit %||% NA_character_

    release_fork_length <- release_event$forkLength.value %||% release_event$forkLength %||% NA_real_
    release_fork_length_unit <- release_event$forkLength.unit %||% release_event$forkLength.unit %||% NA_character_

    release_weight <- release_event$mass.value %||% release_event$mass %||% NA_real_
    release_weight_unit <- release_event$mass.unit %||% release_event$mass.unit %||% NA_character_

    length_total <- if (!is.null(measurement_set) && nrow(measurement_set) > 0) {
      measurement_set$totalLength.value[1] %||% measurement_set$totalLength[1] %||% NA_real_
    } else NA_real_
    length_total_unit <- if (!is.null(measurement_set) && nrow(measurement_set) > 0) {
      measurement_set$totalLength.unit[1] %||% NA_character_
    } else NA_character_

    length_fork <- if (!is.null(measurement_set) && nrow(measurement_set) > 0) {
      measurement_set$forkLength.value[1] %||% measurement_set$forkLength[1] %||% NA_real_
    } else NA_real_
    length_fork_unit <- if (!is.null(measurement_set) && nrow(measurement_set) > 0) {
      measurement_set$forkLength.unit[1] %||% NA_character_
    } else NA_character_

    weight <- if (!is.null(measurement_set) && nrow(measurement_set) > 0) {
      measurement_set$mass.value[1] %||% measurement_set$mass[1] %||% NA_real_
    } else NA_real_
    weight_unit <- if (!is.null(measurement_set) && nrow(measurement_set) > 0) {
      measurement_set$mass.unit[1] %||% NA_character_
    } else NA_character_

    map_dfr(devices_list, function(device) {
      transmitters_list <- device$transmitters[[1]]

      if (!is.data.frame(transmitters_list) || nrow(transmitters_list) == 0) return(tibble())

      map_dfr(1:nrow(transmitters_list), function(j) {
        tibble(
          Nickname = nickname,
          CommonName = common_name,
          Species = species,
          Transmitter = transmitters_list$displayId[j],
          Transmitter.model = device$model %||% NA_character_,
          Transmitter.serial = device$serial %||% NA_character_,
          Tagging.date = tagging_time,
          Tagging.site = tagging_location,
          Tagging.latitude = tagging_lat,
          Tagging.longitude = tagging_lon,
          Tagged.by = tagged_by,
          Tagging.total_length = length_total,
          Tagging.total_length_unit = length_total_unit,
          Tagging.fork_length = length_fork,
          Tagging.fork_length_unit = length_fork_unit,
          Tagging.weight = weight,
          Tagging.weight_unit = weight_unit,
          Release.date = release_time,
          Release.site = release_location,
          Release.latitude = release_lat,
          Release.longitude = release_lon,
          Release.length = release_length,
          Release.length_unit = release_length_unit,
          Release.fork_length = release_fork_length,
          Release.fork_length_unit = release_fork_length_unit,
          Release.weight = release_weight,
          Release.weight_unit = release_weight_unit
        )
      })
    })
  }

  biometrics_tbl <- tibble(
    Nickname = map_chr(animals, ~ .x$name %||% NA_character_),
    CommonName = map_chr(animals, ~ .x$speciesCommonName %||% NA_character_),
    Species = map_chr(animals, ~ .x$speciesScientificName %||% NA_character_),
    Devices = map(animals, ~ .x$devices),
    MeasurementSets = map(animals, ~ .x$measurementSets),
    Events = map(animals, ~ .x$events)
  )

  biometrics_all_flat <- biometrics_tbl %>%
    split(seq_len(nrow(.))) %>%
    map_dfr(flatten_transmitters_with_biometrics)

  # Add CodeSpace and Signal columns
  biometrics_all_flat <- biometrics_all_flat %>%
    dplyr::mutate(
      CodeSpace = sub("-[^-]*$", "", Transmitter)
      ) %>%   # everything before last hyphen
    dplyr::group_by(Nickname, Tagging.date) %>%
    #filter(n_distinct(Transmitter) > 1) %>%
    dplyr::mutate(
      #n_transmitters = n_distinct(Transmitter),
      Transmitters = paste(unique(Transmitter), collapse = ", "),
      Signal = paste(
        unique(str_extract(Transmitter, "(?<=-)[0-9]+$")),
        collapse = "|"))%>%
    select(!Transmitter)%>%
    unique()

  return(biometrics_all_flat)
}
# —————————————————————————————————————————
# Helper function to retrieve detections
# —————————————————————————————————————————
#' @export
fetch_detections <- function(tx_ids, start_date = NULL, end_date = NULL, token, ws_id, page_size = 100000) {
  query <- '
    query allDet($start: Int!, $pageSize: Int!, $filters: DetectionFilterInput) {
      allDetections(start: $start, pageSize: $pageSize, filters: $filters) {
        data nextPageStart
      }
    }'

  filters <- list(includeTransmitterIDs = tx_ids)

  # Flexibly handle date filtering
  if (!is.null(start_date) && tolower(start_date) != "all") {
    filters$includeStartTime <- format(as.POSIXct(start_date, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  }
  if (!is.null(end_date) && tolower(end_date) != "all") {
    filters$includeEndTime <- format(as.POSIXct(end_date, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  }

  start <- 0
  pages <- list()

  repeat {
    res <- POST(
      "https://graph.fathomcentral.com/graphql",
      add_headers(
        "Content-Type" = "application/json",
        Authorization = paste("Bearer", token),
        `workspace-id` = ws_id
      ),
      body = toJSON(list(query = query, variables = list(start = start, pageSize = page_size, filters = filters)), auto_unbox = TRUE)
    )
    stop_for_status(res)
    j <- fromJSON(content(res, "text"))
    str <- j$data$allDetections$data
    if (nzchar(str)) pages[[length(pages) + 1]] <- read.csv(text = str, stringsAsFactors = FALSE)
    nxt <- j$data$allDetections$nextPageStart
    if (is.null(nxt)) break else start <- nxt
  }

  if (length(pages) == 0) {
    message("No detections found.")
    return(tibble())
  }

  bind_rows(pages)
}

# ————————————————————————————————
# get_detections function
# ————————————————————————————————
#' Get Detection Data
#'
#' Retrieves acoustic detection data from the Fathom database using filters for
#' common names, transmitter IDs, transmitter types, and date ranges.
#'
#' @param common_names A vector of common names to filter by (default: "all").
#' @param transmitters A vector of transmitter IDs to filter by (default: "all").
#' @param transmitterTypes A vector of transmitter model types (e.g., "V13") (default: "all").
#' @param start_date Optional start date (e.g., "2021-01-01").
#' @param end_date Optional end date (e.g., "2022-01-01").
#' @param token API token for authentication.
#' @param ws_id Workspace ID for the query.
#'
#' @return A tibble of cleaned detection data.
#' @export
#'
#' @examples
#' \dontrun{
#' auth <- authenticate_wrapper()
#' get_detections(common_names = c("Cownose","Gulf Sturgeon"), token = auth$token, ws_id = auth$ws_id)
#' }
get_detections <- function(common_names = "all",
                           transmitters = "all",
                           transmitterTypes = "all",  # NEW parameter for device models
                           start_date = NULL,
                           end_date = NULL,
                           token = NULL,
                           ws_id = NULL) {

  if (is.null(token) || is.null(ws_id)) {
    auth <- authenticate_wrapper()
    token <- auth$token
    ws_id <- auth$ws_id
  }

  bm <- get_RAW_biometrics(token, ws_id)

  # Filter by common name if not "all"
  if (!identical(tolower(common_names[1]), "all")) {
    bm <- bm %>% filter(CommonName %in% common_names)
  }

  # Unnest devices into a dataframe
  device_info <- bm %>%
    dplyr::transmute(Devices = map(Devices, ~
                              if (length(.x) && is.data.frame(.x[[1]])) bind_rows(.x) else tibble()
    )) %>%
    tidyr::unnest(Devices, keep_empty = TRUE)

  # Filter by transmitter type (device model) if not "all"
  if (!identical(tolower(transmitterTypes[1]), "all")) {
    pattern <- paste(transmitterTypes, collapse = "|")  # e.g., "V13|V16"
    device_info <- device_info %>% filter(str_detect(model, pattern))
  }

  # Unnest transmitters from devices
  device_info <- device_info %>%
    tidyr::unnest(transmitters, keep_empty = TRUE)

  # Filter by specific transmitters if not "all"
  if (!identical(tolower(transmitters[1]), "all")) {
    device_info <- device_info %>% filter(displayId %in% transmitters)
  }

  tx <- unique(device_info$displayId)
  if (length(tx) == 0) stop("No transmitters matched filters.")

  # Call the fetch function with filtered transmitters and dates
  raw_detections <- fetch_detections(tx, start_date, end_date, token, ws_id)

  # Clean detections
  cleaned_detections <- raw_detections %>%
    dplyr::rename(
      Transmitter = full_id,
      Receiver = serial,
      Sensor.Value = sensor_value,
      Sensor.Unit = sensor_type
    ) %>%
    dplyr::mutate(
      CodeSpace = sub("-[^-]*$", "", Transmitter),
      Signal = str_extract(Transmitter, "(?<=-)[0-9]+$"),
      Timestamp = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    ) %>%
    dplyr::select(-files, -time) %>%
    dplyr::filter(
      !is.na(Transmitter) & Transmitter != "",
      !is.na(Receiver) & Receiver != "",
      !is.na(Timestamp)
    )

  return(cleaned_detections)
}
# —————————————————————————————————————————
# Get Deployments from Fathom
# —————————————————————————————————————————
#' Get Deployment Metadata
#'
#' Retrieves receiver deployment metadata from the Fathom database,
#' including receiver serial numbers, associated station names, and
#' deployment start and stop dates.
#'
#' This function queries the GraphQL API using a bearer token and workspace ID.
#'
#' @param token Optional. Fathom API token. If `NULL`, `authenticate_wrapper()` will be called to retrieve it.
#' @param ws_id Optional. Workspace ID. If `NULL`, `authenticate_wrapper()` will be called to retrieve it.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{Receiver}{Receiver serial number.}
#'   \item{Station.name}{Name of the deployment station.}
#'   \item{Start}{Start datetime of the deployment.}
#'   \item{Stop}{End datetime of the deployment.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' auth <- authenticate_wrapper()
#' deployments <- get_deployments(token = auth$token, ws_id = auth$ws_id)
#' head(deployments)
#' }
get_deployments <- function(token = NULL, ws_id = NULL) {
  if (is.null(token) || is.null(ws_id)) {
    auth <- authenticate_wrapper()
    token <- auth$token
    ws_id <- auth$ws_id
  }

  query <- '
    query listDeployments {
      deployments {
        id
        start
        end
        station {
          name
          id
        }
        deviceAttachments {
          device {
            serial
          }
        }
      }
    }
  '

  res <- httr::POST(
    "https://graph.fathomcentral.com/graphql",
    httr::add_headers(
      "Content-Type" = "application/json",
      Authorization = paste("Bearer", token),
      `workspace-id` = ws_id
    ),
    body = jsonlite::toJSON(list(query = query), auto_unbox = TRUE)
  )

  httr::stop_for_status(res)
  dat <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), flatten = TRUE)

  deployments_df <- dat$data$deployments

  # Safely extract receiver serials
  deployments_df$Receiver <- purrr::map_chr(deployments_df$deviceAttachments, function(x) {
    # x should be a list of length ≥ 1 with $device$serial
    if (is.null(x) || length(x) == 0) {
      return(NA_character_)
    } else if (is.data.frame(x)) {
      return(x$device.serial[1])
    } else if (is.list(x) && !is.null(x[[1]]$device$serial)) {
      return(x[[1]]$device$serial)
    } else {
      return(NA_character_)
    }
  })

  # Select and rename final columns
  deployments_clean <- deployments_df %>%
    dplyr::select(
      Receiver,
      Station.name = station.name,
      Start = start,
      Stop = end
    )

  return(deployments_clean)
}

# —————————————————————————————————————————
# Get Spatial
# —————————————————————————————————————————
#' Get Spatial Metadata for Receivers
#'
#' Retrieves spatial metadata for receiver deployments, including latitude, longitude, depth,
#' and station names. You can filter results by receiver serial numbers, station names, or a bounding box.
#'
#' @param token Optional. Fathom API token. If `NULL`, `authenticate_wrapper()` is called.
#' @param ws_id Optional. Workspace ID. If `NULL`, `authenticate_wrapper()` is called.
#' @param StationNames Character vector of station name substrings to filter by (default is "all").
#' @param Receivers Character vector of receiver serial number substrings to filter by (default is "all").
#' @param Area Optional bounding box filter. A list with two elements: `lat = c(minLat, maxLat)` and `lon = c(minLon, maxLon)`.
#'
#' @return A tibble with receiver spatial metadata, including:
#' \describe{
#'   \item{Receiver}{Receiver serial number.}
#'   \item{Station.name}{Name of the deployment station.}
#'   \item{station.id}{Internal station ID.}
#'   \item{Latitude}{Latitude of the receiver position.}
#'   \item{Longitude}{Longitude of the receiver position.}
#'   \item{Depth}{Deployment depth in meters, if available.}
#'   \item{Type}{Receiver type (default is "Hydrophone").}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' auth <- authenticate_wrapper()
#' spatial <- get_spatial(token = auth$token, ws_id = auth$ws_id)
#' head(spatial)
#' }
get_spatial <- function(token = NULL, ws_id = NULL,
                        StationNames = "all",   # renamed argument
                        Receivers = "all",
                        Area = NULL) {
  if (is.null(token) || is.null(ws_id)) {
    auth <- authenticate_wrapper()
    token <- auth$token
    ws_id <- auth$ws_id
  }

  # GraphQL query
  query <- 'query listDeployments {
    deployments {
      id
      start
      end
      station {
        name
        id
      }
      deviceAttachments {
        id
        device {
          id
          capabilities
          serial
          model
          transmitters {
            displayId
          }
          deviceClasses
        }
        start
        end
        height {
          value
          unit
        }
        conflict {
          conflictingDeploymentIds
        }
        studyConflicts {
          studyId
          conflict {
            conflictingDeploymentIds
          }
        }
      }
      positions {
        id
        start
        end
        latLon {
          latitude
          longitude
        }
        depth {
          value
          unit
        }
        hasConflict
      }
    }
    stations {
      id
      name
    }
  }'

  res <- httr::POST(
    "https://graph.fathomcentral.com/graphql",
    httr::add_headers(
      "Content-Type" = "application/json",
      Authorization = paste("Bearer", token),
      `workspace-id` = ws_id
    ),
    body = jsonlite::toJSON(list(query = query), auto_unbox = TRUE)
  )

  httr::stop_for_status(res)
  dat <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), flatten = TRUE)

  spatial_df <- dat$data$deployments

  # Extract receiver serials safely
  spatial_df$Receiver <- purrr::map_chr(spatial_df$deviceAttachments, function(x) {
    if (is.null(x) || length(x) == 0) {
      NA_character_
    } else if (is.data.frame(x)) {
      x$device.serial[1]
    } else if (is.list(x) && !is.null(x[[1]]$device$serial)) {
      x[[1]]$device$serial
    } else {
      NA_character_
    }
  })

  # Flatten positions nested list-column
  positions_expanded <- spatial_df %>%
    dplyr::select(Receiver, station.name, station.id, start, end, positions) %>%
    dplyr::mutate(positions = ifelse(lengths(positions) == 0, NA, positions)) %>%
    tidyr::unnest(positions, names_sep = "_") %>%
    dplyr::mutate(
      Latitude = positions_latLon.latitude,
      Longitude = positions_latLon.longitude,
      Depth = if ("positions_depth.value" %in% colnames(.)) positions_depth.value else NA_real_,
      Type = "Hydrophone"
    ) %>%
    dplyr::rename(Station.name = station.name) %>%
    dplyr::select(
      Receiver, Station.name, station.id,
      Latitude, Longitude, Depth, Type
    )

  # Filter by StationNames (substring match)
  if (!identical(StationNames, "all")) {
    positions_expanded <- positions_expanded %>%
      dplyr::filter(
        purrr::reduce(
          StationNames,
          ~ .x | grepl(.y, Station.name, ignore.case = TRUE),
          .init = FALSE
        )
      )
  }

  # Filter by Receivers (substring match)
  if (!identical(Receivers, "all")) {
    positions_expanded <- positions_expanded %>%
      dplyr::filter(
        purrr::reduce(
          Receivers,
          ~ .x | grepl(.y, Receiver, ignore.case = FALSE),
          .init = FALSE
        )
      )
  }

  # Filter by bounding box Area (if provided)
  if (!is.null(Area) && all(c("lon", "lat") %in% names(Area))) {
    lon_range <- sort(Area$lon)
    lat_range <- sort(Area$lat)
    positions_expanded <- positions_expanded %>%
      dplyr::filter(
        !is.na(Longitude), !is.na(Latitude),
        Longitude >= lon_range[1], Longitude <= lon_range[2],
        Latitude  >= lat_range[1], Latitude  <= lat_range[2]
      )
  }

  return(positions_expanded)
}
