
#' Download detections from Fathom Central
#'
#' Downloads detection records for a selected study, with optional filters,
#' a progress bar, and an estimated time remaining (ETA).
#'
#' @param token Fathom API access token.
#' @param ws_id Workspace ID.
#' @param study_id Study ID.
#' @param start_date Optional detection start date/time.
#' @param end_date Optional detection end date/time.
#' @param receivers Optional receiver serial numbers.
#' @param receiver_models Optional receiver models.
#' @param stations Optional station names or IDs.
#' @param transmitter_models Optional transmitter models.
#' @param full_ids Optional transmitter full IDs.
#' @param animals Optional animal IDs.
#' @param species Optional species.
#' @param corrected_time Logical; whether to use corrected detection times.
#'
#' @return A data frame containing the detection records.
#' @export

get_detections <- function(
    token,
    ws_id,
    study_id,
    start_date = NULL,
    end_date = NULL,
    receivers = NULL,
    receiver_models = NULL,
    stations = NULL,
    transmitter_models = NULL,
    full_ids = NULL,
    animals = NULL,
    species = NULL,
    corrected_time = TRUE
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

  if (!is.logical(corrected_time) ||
      length(corrected_time) != 1 ||
      is.na(corrected_time)) {
    stop("'corrected_time' must be TRUE or FALSE.")
  }


  # ------------------------------------------------------------
  # Convert dates to UTC ISO timestamps
  # ------------------------------------------------------------

  format_date <- function(x) {

    if (is.null(x)) {
      return(NULL)
    }

    if (length(x) != 1 || is.na(x)) {
      stop("Dates must be single, non-NA values.")
    }

    if (inherits(x, "Date")) {

      x <- as.POSIXct(x, tz = "UTC")

    } else if (inherits(x, "POSIXt")) {

      x <- as.POSIXct(x, tz = "UTC")

    } else if (is.character(x)) {

      x <- as.POSIXct(x, tz = "UTC")

      if (is.na(x)) {
        stop("Could not interpret the supplied date.")
      }

    } else {

      stop("Dates must be Date, POSIXct, or character values.")

    }

    format(
      x,
      format = "%Y-%m-%dT%H:%M:%OS3Z",
      tz = "UTC",
      usetz = FALSE
    )
  }

  start_date <- format_date(start_date)
  end_date <- format_date(end_date)


  # ------------------------------------------------------------
  # Construct detection filters
  # ------------------------------------------------------------

  filters <- list()

  # Selection filters must be arrays, including when only one
  # selection is supplied.

  add_filter <- function(filter_name, value) {

    if (!is.null(value)) {
      filters[[filter_name]] <<- as.list(as.character(value))
    }

  }

  add_filter("includeReceiverSerials", receivers)
  add_filter("includeReceiverModels", receiver_models)
  add_filter("includeStations", stations)
  add_filter("includeTransmitterModels", transmitter_models)
  add_filter("includeTransmitterIDs", full_ids)
  add_filter("includeAnimals", animals)
  add_filter("includeSpecies", species)

  # Date filters are individual timestamps, not arrays.

  if (!is.null(start_date)) {
    filters$includeStartTime <- start_date
  }

  if (!is.null(end_date)) {
    filters$includeEndTime <- end_date
  }

  if (length(filters) == 0) {
    filters <- NULL
  }


  # ------------------------------------------------------------
  # GraphQL request helper
  # ------------------------------------------------------------

  post_graphql <- function(request) {

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

    if (is.null(result$data)) {
      stop("The Fathom API returned no data.")
    }

    result$data
  }


  # ------------------------------------------------------------
  # Retrieve expected detection count
  # ------------------------------------------------------------

  count_query <- paste(
    "query detCounts(",
    "$studyId: ID,",
    "$filters: DetectionFilterInput,",
    "$useTimeCorrection: Boolean",
    ") {",
    "  detectionCounts(",
    "    studyId: $studyId,",
    "    filters: $filters,",
    "    correctedTime: $useTimeCorrection",
    "  ) {",
    "    totalCount",
    "    timeCorrectedCount",
    "  }",
    "}"
  )

  count_request <- list(
    query = count_query,

    variables = list(
      studyId = as.character(study_id),
      filters = filters,
      useTimeCorrection = corrected_time
    )
  )

  total_count <- tryCatch({

    count_data <- post_graphql(count_request)

    counts <- count_data$detectionCounts

    if (is.null(counts)) {
      stop("Detection count was not returned.")
    }

    count <- if (corrected_time) {
      counts$timeCorrectedCount
    } else {
      counts$totalCount
    }

    count <- as.numeric(count)

    if (length(count) != 1 ||
        is.na(count) ||
        !is.finite(count) ||
        count < 0) {
      stop("Invalid detection count returned by API.")
    }

    count

  }, error = function(e) {

    warning(
      "Could not retrieve expected detection count: ",
      conditionMessage(e),
      "\nContinuing download without a progress bar."
    )

    NA_real_

  })


  # ------------------------------------------------------------
  # Detection query
  # ------------------------------------------------------------

  detection_query <- paste(
    "query allDet(",
    "$start: Int!",
    "$pageSize: Int!",
    "$studyId: ID",
    "$filters: DetectionFilterInput",
    "$orderBy: OrderBy!",
    "$correctedTime: Boolean",
    ") {",
    "  allDetections(",
    "    start: $start",
    "    pageSize: $pageSize",
    "    studyId: $studyId",
    "    filters: $filters",
    "    orderBy: $orderBy",
    "    correctedTime: $correctedTime",
    "  ) {",
    "    data",
    "  }",
    "}"
  )


  # ------------------------------------------------------------
  # Initialize pagination and progress bar
  # ------------------------------------------------------------

  page_size <- 1000
  start <- 0

  downloaded <- 0
  all_data <- list()

  # Start timer for ETA calculation
  download_start <- Sys.time()

  pb <- NULL

  if (!is.na(total_count) && total_count > 0) {

    message(
      "Expected detections: ",
      format(total_count, big.mark = ",")
    )

    pb <- utils::txtProgressBar(
      min = 0,
      max = total_count,
      initial = 0,
      style = 3
    )

  } else if (!is.na(total_count) && total_count == 0) {

    message("Expected detections: 0")

  }

  # Always close the progress bar, including if an error occurs.

  on.exit({

    if (!is.null(pb)) {
      close(pb)
    }

  }, add = TRUE)


  # ------------------------------------------------------------
  # Download detections in batches
  # ------------------------------------------------------------

  repeat {

    detection_request <- list(

      query = detection_query,

      variables = list(
        start = start,
        pageSize = page_size,
        studyId = as.character(study_id),
        filters = filters,

        orderBy = list(
          column = "time_utc",
          order = "ASC"
        ),

        correctedTime = corrected_time
      )
    )


    # Send request

    detection_data <- post_graphql(detection_request)

    csv_data <- detection_data$allDetections$data


    # Stop if no CSV data is returned

    if (is.null(csv_data) ||
        length(csv_data) == 0 ||
        !nzchar(trimws(csv_data))) {
      break
    }


    # Parse CSV response

    batch <- utils::read.csv(
      text = csv_data,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )


    # Stop if the response contains no records

    if (nrow(batch) == 0) {
      break
    }


    # Store batch and update download count

    all_data[[length(all_data) + 1]] <- batch

    downloaded <- downloaded + nrow(batch)


    # ----------------------------------------------------------
    # Update progress bar and calculate ETA
    # ----------------------------------------------------------

    elapsed <- as.numeric(
      difftime(
        Sys.time(),
        download_start,
        units = "secs"
      )
    )

    if (!is.null(pb)) {

      utils::setTxtProgressBar(
        pb,
        value = min(downloaded, total_count)
      )

      if (downloaded > 0 &&
          downloaded < total_count &&
          elapsed > 0) {

        # Average download rate (detections per second)
        download_rate <- downloaded / elapsed

        # Estimated remaining time (seconds)
        remaining <- (total_count - downloaded) / download_rate

        # Format ETA
        eta <- if (remaining < 60) {

          paste0(round(remaining), " seconds")

        } else if (remaining < 3600) {

          paste0(round(remaining / 60, 1), " minutes")

        } else {

          paste0(round(remaining / 3600, 1), " hours")

        }

        message(
          "\nDownloaded ",
          format(downloaded, big.mark = ","),
          " / ",
          format(total_count, big.mark = ","),
          " detections | ETA: ",
          eta
        )

      }

    } else {

      message(
        "Downloaded ",
        format(downloaded, big.mark = ","),
        " detections... (",
        round(elapsed, 1),
        " seconds elapsed)"
      )

    }


    # Advance pagination

    start <- start + page_size


    # Stop when the final, incomplete batch is reached

    if (nrow(batch) < page_size) {
      break
    }

  }


  # ------------------------------------------------------------
  # Combine downloaded batches
  # ------------------------------------------------------------

  if (length(all_data) == 0) {

    message("No detections were returned.")

    return(data.frame())

  }

  detections <- do.call(
    rbind,
    all_data
  )

  rownames(detections) <- NULL


  # ------------------------------------------------------------
  # Report download results
  # ------------------------------------------------------------

  elapsed <- as.numeric(
    difftime(
      Sys.time(),
      download_start,
      units = "secs"
    )
  )

  message(
    "\nDownload complete: ",
    format(downloaded, big.mark = ","),
    " detections retrieved."
  )

  message(
    "Total download time: ",
    if (elapsed < 60) {
      paste0(round(elapsed, 1), " seconds")
    } else if (elapsed < 3600) {
      paste0(round(elapsed / 60, 1), " minutes")
    } else {
      paste0(round(elapsed / 3600, 2), " hours")
    }
  )

  if (!is.na(total_count)) {

    message(
      "Expected count: ",
      format(total_count, big.mark = ","),
      ". Retrieved count: ",
      format(downloaded, big.mark = ","),
      "."
    )

    if (downloaded != total_count) {

      warning(
        "The retrieved detection count does not match the expected ",
        "detection count returned by the API."
      )

    }

  }

  return(detections)

}
