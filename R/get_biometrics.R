#' Get animal biometrics and event history from Fathom Central
#'
#' Retrieves animal records, event history, device information, and all
#' available measurement sets for a selected study.
#'
#' Returns one row per animal event. Measurement sets are matched to events
#' for the same animal on the same UTC calendar date. Multiple measurement
#' sets on the same date are combined into the corresponding event row.
#'
#' Original date and time values returned by Fathom are preserved without
#' timezone conversion.
#'
#' @param token Fathom API access token.
#' @param ws_id Fathom workspace ID.
#' @param study_id Fathom study ID.
#' @param Event Character vector specifying which event types to include.
#'   Accepted values are "Capture", "Tagging", "Release", and
#'   "DeviceRecovery". Full Fathom event names are also accepted.
#'   Defaults to NULL, which returns all event types.
#'
#' @return A data frame containing one row per animal event.
#' @export

get_biometrics <- function(token,
                           ws_id,
                           study_id,
                           Event = NULL) {

  # Check inputs -----------------------------------------------------------

  if (missing(token) || is.null(token) || !nzchar(token)) {
    stop("Please provide a valid Fathom access token.")
  }

  if (missing(ws_id) || is.null(ws_id)) {
    stop("Please provide a workspace ID.")
  }

  if (missing(study_id) || is.null(study_id)) {
    stop("Please provide a study ID.")
  }

  # Accepted event types and their corresponding GraphQL names.
  event_map <- c(
    Capture = "AnimalEventCapture",
    Tagging = "AnimalEventTagging",
    Release = "AnimalEventRelease",
    DeviceRecovery = "AnimalEventDeviceRecovery"
  )

  # Validate and translate the Event argument.
  if (!is.null(Event)) {

    if (!is.character(Event) ||
        length(Event) == 0 ||
        anyNA(Event) ||
        any(!nzchar(Event))) {
      stop(
        "Event must be NULL or a character vector containing valid event types."
      )
    }

    # Accept friendly names or full Fathom event names.
    event_lookup <- c(
      event_map,
      setNames(
        unname(event_map),
        unname(event_map)
      )
    )

    if (any(!Event %in% names(event_lookup))) {

      stop(
        "Invalid Event value. Accepted values are: ",
        paste(
          c(names(event_map), unname(event_map)),
          collapse = ", "
        ),
        "."
      )
    }

    Event <- unique(
      unname(event_lookup[Event])
    )
  }


  # GraphQL query ----------------------------------------------------------

  query <- '
  query listAnimals($studyId: ID) {
    animals(studyId: $studyId) {
      id
      name
      creationTime
      lastModified
      speciesCommonName
      speciesScientificName
      origin
      stock
      notes

      devices {
        id
        serial
        model
        transmitters {
          displayId
        }
      }

      events {
        id
        time
        lastModified
        __typename

        latLon {
          latitude
          longitude
        }

        locationName
        researcherName
        notes

        ... on AnimalEventTagging {
          devices {
            id
            serial
            model
            deviceClasses
            transmitters {
              displayId
            }
          }
          taggingMethod
          anaesthetic
          sedative
          buffer
          anatomicLocation
          pitTagId
          floyTagId
        }

        ... on AnimalEventCapture {
          captureMethod
        }

        ... on AnimalEventDeviceRecovery {
          devices {
            id
            serial
            model
            deviceClasses
            transmitters {
              displayId
            }
          }
        }
      }

      measurementSets {
        id

        totalLength {
          value
          unit
        }

        forkLength {
          value
          unit
        }

        standardLength {
          value
          unit
        }

        hoodLength {
          value
          unit
        }

        width {
          value
          unit
        }

        girth {
          value
          unit
        }

        mass {
          value
          unit
        }

        age {
          value
          unit
        }

        sex
        lifeStage
        measurerName
        time
      }

      conflicts {
        conflictingAnimalIds
        __typename

        ... on AnimalDeviceConflict {
          deviceId
        }
      }

      studyConflicts {
        studyId
        conflict {
          conflictingAnimalIds
          __typename

          ... on AnimalDeviceConflict {
            deviceId
          }
        }
      }
    }
  }'


  # Submit GraphQL request -------------------------------------------------

  response <- httr::POST(
    url = "https://graph.fathomcentral.com/graphql",

    httr::add_headers(
      Authorization = paste("Bearer", token),
      `Content-Type` = "application/json",
      `workspace-id` = as.character(ws_id)
    ),

    body = list(
      query = query,
      variables = list(
        studyId = as.character(study_id)
      )
    ),

    encode = "json"
  )

  response_text <- httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )

  result <- jsonlite::fromJSON(
    response_text,
    simplifyVector = FALSE
  )

  if (httr::http_error(response)) {
    stop(
      "Fathom API request failed (HTTP ",
      httr::status_code(response),
      "):\n",
      response_text
    )
  }

  if (!is.null(result$errors)) {

    error_messages <- vapply(
      result$errors,
      function(x) {
        if (!is.null(x$message)) {
          x$message
        } else {
          "Unknown GraphQL error"
        }
      },
      character(1)
    )

    stop(
      "GraphQL returned an error:\n",
      paste(error_messages, collapse = "\n")
    )
  }

  animals <- result$data$animals

  if (is.null(animals) || length(animals) == 0) {
    message("No animals were returned for this study.")
    return(data.frame())
  }


  # Helper functions -------------------------------------------------------

  # Safely retrieve a field from a list.
  get_field <- function(x, field, default = NA) {

    if (is.null(x) ||
        is.null(x[[field]]) ||
        length(x[[field]]) == 0) {
      return(default)
    }

    x[[field]]
  }


  # Preserve the original timestamp returned by Fathom.
  # Remove the timezone suffix and milliseconds without converting time.
  format_datetime <- function(x) {

    if (is.null(x) ||
        length(x) == 0 ||
        is.na(x) ||
        !nzchar(x)) {
      return(NA_character_)
    }

    x <- sub("Z$", "", x)

    x <- sub("\\.\\d+$", "", x)

    gsub("T", " ", x, fixed = TRUE)
  }


  # Extract the original UTC calendar date from an ISO timestamp.
  get_utc_date <- function(x) {

    if (is.null(x) ||
        length(x) == 0 ||
        is.na(x) ||
        !nzchar(x)) {
      return(NA_character_)
    }

    substr(x, 1, 10)
  }


  # Collapse multiple values, removing missing and duplicate values.
  collapse_values <- function(x) {

    x <- unlist(x, recursive = TRUE, use.names = FALSE)

    x <- x[!is.na(x)]
    x <- as.character(x)
    x <- x[nzchar(x)]

    x <- unique(x)

    if (length(x) == 0) {
      return(NA_character_)
    }

    paste(x, collapse = "; ")
  }


  # Extract the value and unit from a measurement object.
  measurement_value <- function(measurement_sets, field) {

    values <- lapply(
      measurement_sets,
      function(x) {

        measurement <- x[[field]]

        if (is.null(measurement)) {
          return(NULL)
        }

        measurement$value
      }
    )

    collapse_values(values)
  }


  # Extract measurement units.
  measurement_unit <- function(measurement_sets, field) {

    units <- lapply(
      measurement_sets,
      function(x) {

        measurement <- x[[field]]

        if (is.null(measurement)) {
          return(NULL)
        }

        measurement$unit
      }
    )

    collapse_values(units)
  }


  # Extract simple measurement fields such as sex and life stage.
  measurement_metadata <- function(measurement_sets, field) {

    values <- lapply(
      measurement_sets,
      function(x) {
        x[[field]]
      }
    )

    collapse_values(values)
  }


  # Extract complete transmitter display IDs from device records.
  get_transmitter_ids <- function(devices) {

    if (is.null(devices) || length(devices) == 0) {
      return(NA_character_)
    }

    transmitter_ids <- unlist(
      lapply(
        devices,
        function(device) {

          transmitters <- device$transmitters

          if (is.null(transmitters)) {
            return(NULL)
          }

          lapply(
            transmitters,
            function(transmitter) {
              transmitter$displayId
            }
          )
        }
      ),
      recursive = TRUE,
      use.names = FALSE
    )

    transmitter_ids <- collapse_values(transmitter_ids)

    if (is.na(transmitter_ids)) {
      return(NA_character_)
    }

    # Use | as the separator for multiple transmitter IDs.
    gsub("; ", "|", transmitter_ids, fixed = TRUE)
  }


  # Extract signal numbers from complete transmitter IDs.
  get_signal <- function(transmitter_ids) {

    if (is.null(transmitter_ids) ||
        length(transmitter_ids) == 0 ||
        is.na(transmitter_ids) ||
        !nzchar(transmitter_ids)) {
      return(NA_character_)
    }

    ids <- strsplit(
      transmitter_ids,
      "|",
      fixed = TRUE
    )[[1]]

    # Extract the digits following the final hyphen.
    signals <- sub("^.*-", "", ids)

    signals <- unique(signals[nzchar(signals)])

    if (length(signals) == 0) {
      return(NA_character_)
    }

    paste(signals, collapse = "|")
  }


  # Extract device information.
  get_device_info <- function(devices, field) {

    if (is.null(devices) || length(devices) == 0) {
      return(NA_character_)
    }

    values <- lapply(
      devices,
      function(device) {
        device[[field]]
      }
    )

    collapse_values(values)
  }


  # Extract coordinates.
  get_coordinate <- function(event, coordinate) {

    if (is.null(event$latLon)) {
      return(NA_real_)
    }

    value <- event$latLon[[coordinate]]

    if (is.null(value)) {
      return(NA_real_)
    }

    as.numeric(value)
  }


  # Process animals --------------------------------------------------------

  output <- list()

  for (animal in animals) {

    animal_id <- get_field(animal, "id")
    animal_name <- get_field(animal, "name")

    events <- animal$events
    measurement_sets <- animal$measurementSets
    animal_devices <- animal$devices

    if (is.null(events) || length(events) == 0) {
      next
    }

    if (is.null(measurement_sets)) {
      measurement_sets <- list()
    }


    # Identify recapture events -------------------------------------------

    event_times <- vapply(
      events,
      function(event) {

        x <- event$time

        if (is.null(x)) {
          return(NA_character_)
        }

        as.character(x)
      },
      character(1)
    )

    event_types <- vapply(
      events,
      function(event) {

        x <- event[["__typename"]]

        if (is.null(x)) {
          return(NA_character_)
        }

        as.character(x)
      },
      character(1)
    )

    capture_indices <- which(
      event_types == "AnimalEventCapture"
    )

    is_recapture <- rep(FALSE, length(events))

    if (length(capture_indices) > 0) {

      capture_order <- capture_indices[
        order(
          is.na(event_times[capture_indices]),
          event_times[capture_indices],
          capture_indices
        )
      ]

      if (length(capture_order) > 1) {
        is_recapture[capture_order[-1]] <- TRUE
      }
    }


    # Build one row per event ----------------------------------------------

    for (i in seq_along(events)) {

      event <- events[[i]]

      event_type <- event[["__typename"]]
      event_time <- event$time

      # Apply the requested event filter.
      if (!is.null(Event) && !event_type %in% Event) {
        next
      }

      event_date <- get_utc_date(event_time)


      # Match measurement sets to the event by animal and UTC calendar date.
      # Event and measurement timestamps remain separate.
      matched_measurements <- Filter(
        function(measurement) {

          measurement_date <- get_utc_date(
            measurement$time
          )

          !is.na(event_date) &&
            !is.na(measurement_date) &&
            event_date == measurement_date
        },
        measurement_sets
      )


      # Device information ------------------------------------------------

      event_devices <- event$devices

      # Use event-level devices when available; otherwise use animal devices.
      if (is.null(event_devices) || length(event_devices) == 0) {
        event_devices <- animal_devices
      }

      # Keep both complete transmitter IDs and actel-compatible signals.
      transmitter_ids <- get_transmitter_ids(event_devices)

      signal <- get_signal(transmitter_ids)


      # Measurement information -------------------------------------------

      measurement_dates <- vapply(
        matched_measurements,
        function(x) {
          format_datetime(x$time)
        },
        character(1)
      )

      measurement_ids <- lapply(
        matched_measurements,
        function(x) {
          x$id
        }
      )


      # Construct event row ------------------------------------------------

      row <- data.frame(

        Animal.ID = as.character(animal_id),

        Animal.name = as.character(animal_name),

        Event.ID = as.character(
          get_field(event, "id")
        ),

        Event.type = as.character(event_type),

        Event.date = format_datetime(event_time),

        # Release.date is populated only for release events.
        Release.date = if (
          identical(event_type, "AnimalEventRelease")
        ) {
          format_datetime(event_time)
        } else {
          NA_character_
        },

        Is.recapture = is_recapture[i],

        Signal = signal,

        Transmitter.IDs = transmitter_ids,

        Group = as.character(
          get_field(animal, "speciesCommonName")
        ),

        Species.scientific = as.character(
          get_field(animal, "speciesScientificName")
        ),

        Release.site = if (
          identical(event_type, "AnimalEventRelease")
        ) {
          as.character(get_field(event, "locationName"))
        } else {
          NA_character_
        },

        Location = as.character(
          get_field(event, "locationName")
        ),

        Latitude = get_coordinate(event, "latitude"),

        Longitude = get_coordinate(event, "longitude"),

        Researcher = as.character(
          get_field(event, "researcherName")
        ),

        Event.notes = as.character(
          get_field(event, "notes")
        ),

        Animal.notes = as.character(
          get_field(animal, "notes")
        ),

        Capture.method = as.character(
          get_field(event, "captureMethod")
        ),

        Tagging.method = as.character(
          get_field(event, "taggingMethod")
        ),

        Anaesthetic = as.character(
          get_field(event, "anaesthetic")
        ),

        Sedative = as.character(
          get_field(event, "sedative")
        ),

        Buffer = as.character(
          get_field(event, "buffer")
        ),

        Anatomic.location = as.character(
          get_field(event, "anatomicLocation")
        ),

        PIT.tag.ID = as.character(
          get_field(event, "pitTagId")
        ),

        Floy.tag.ID = as.character(
          get_field(event, "floyTagId")
        ),

        Device.ID = get_device_info(
          event_devices,
          "id"
        ),

        Device.serial = get_device_info(
          event_devices,
          "serial"
        ),

        Device.model = get_device_info(
          event_devices,
          "model"
        ),

        Device.classes = get_device_info(
          event_devices,
          "deviceClasses"
        ),

        Measurement.date = collapse_values(
          as.list(measurement_dates)
        ),

        Measurement.ID = collapse_values(
          measurement_ids
        ),

        stringsAsFactors = FALSE
      )


      # Add all measurement fields ----------------------------------------

      measurement_fields <- c(
        "totalLength",
        "forkLength",
        "standardLength",
        "hoodLength",
        "width",
        "girth",
        "mass",
        "age"
      )

      measurement_names <- c(
        "Total.length",
        "Fork.length",
        "Standard.length",
        "Hood.length",
        "Width",
        "Girth",
        "Mass",
        "Age"
      )

      for (j in seq_along(measurement_fields)) {

        field <- measurement_fields[j]
        output_name <- measurement_names[j]

        row[[output_name]] <- measurement_value(
          matched_measurements,
          field
        )

        row[[paste0(output_name, ".unit")]] <- measurement_unit(
          matched_measurements,
          field
        )
      }


      # Add biological and measurement metadata ---------------------------

      row$Sex <- measurement_metadata(
        matched_measurements,
        "sex"
      )

      row$Life.stage <- measurement_metadata(
        matched_measurements,
        "lifeStage"
      )

      row$Measurer <- measurement_metadata(
        matched_measurements,
        "measurerName"
      )


      # Add animal origin and stock information ----------------------------

      row$Origin <- as.character(
        get_field(animal, "origin")
      )

      row$Stock <- as.character(
        get_field(animal, "stock")
      )


      # Append row ---------------------------------------------------------

      output[[length(output) + 1]] <- row
    }
  }


  # Combine event rows -----------------------------------------------------

  if (length(output) == 0) {
    message("No animal events were returned for the selected event types.")
    return(data.frame())
  }

  biometrics <- do.call(
    rbind,
    output
  )

  rownames(biometrics) <- NULL


  # Convert measurement columns to numeric where possible -----------------

  measurement_columns <- c(
    "Total.length",
    "Fork.length",
    "Standard.length",
    "Hood.length",
    "Width",
    "Girth",
    "Mass",
    "Age"
  )

  for (column in measurement_columns) {

    biometrics[[column]] <- type.convert(
      biometrics[[column]],
      as.is = TRUE
    )
  }


  # Return data ------------------------------------------------------------

  return(biometrics)
}
