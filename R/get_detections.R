#' Download Detection Data
#'
#' Downloads detection data from Fathom Central using the server-side CSV
#' export. Filters are applied by Fathom before the CSV is generated.
#'
#' @param token Fathom API access token.
#' @param ws_id Fathom workspace ID.
#' @param study_id Fathom study ID.
#' @param start_date Start date for detections.
#' @param end_date End date for detections.
#' @param receivers Optional vector of receiver serial numbers.
#' @param receiver_models Optional vector of receiver models.
#' @param stations Optional vector of station IDs.
#' @param transmitter_models Optional vector of transmitter models.
#' @param full_ids Optional vector of transmitter full IDs.
#' @param animals Optional vector of animal IDs.
#' @param species Optional vector of species names.
#' @param corrected_time Logical. Include corrected detection times.
#' @param download_path Optional CSV destination. If NULL, a save dialog
#'   is opened.
#'
#' @return A data frame containing the downloaded detections.
#' @export
get_detections <- function(
    token,
    ws_id,
    study_id,
    start_date,
    end_date,
    receivers = NULL,
    receiver_models = NULL,
    stations = NULL,
    transmitter_models = NULL,
    full_ids = NULL,
    animals = NULL,
    species = NULL,
    corrected_time = TRUE,
    download_path = NULL
) {

  # ------------------------------------------------------------
  # 1. Validate inputs
  # ------------------------------------------------------------

  if (missing(token) || is.null(token) || !nzchar(token)) {
    stop("A valid Fathom access token is required.")
  }

  if (missing(ws_id) || is.null(ws_id)) {
    stop("A workspace ID is required.")
  }

  if (missing(study_id) || is.null(study_id)) {
    stop("A study ID is required.")
  }

  if (missing(start_date) || missing(end_date)) {
    stop("Both start_date and end_date must be supplied.")
  }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  if (is.na(start_date) || is.na(end_date)) {
    stop("start_date and end_date must be valid dates.")
  }

  if (start_date > end_date) {
    stop("start_date cannot be later than end_date.")
  }

  if (!is.logical(corrected_time) ||
      length(corrected_time) != 1 ||
      is.na(corrected_time)) {
    stop("corrected_time must be TRUE or FALSE.")
  }

  # ------------------------------------------------------------
  # 2. Select CSV destination
  # ------------------------------------------------------------

  if (is.null(download_path)) {

    if (!requireNamespace("tcltk", quietly = TRUE)) {
      stop(
        "The tcltk package is required to open the save dialog. ",
        "Alternatively, supply download_path."
      )
    }

    download_path <- tcltk::tclvalue(
      tcltk::tkgetSaveFile(
        defaultextension = ".csv",
        filetypes = "{{CSV Files} {.csv}} {{All Files} *}"
      )
    )

    if (is.null(download_path) || !nzchar(download_path)) {
      stop("CSV download cancelled.")
    }
  }

  if (!grepl("\\.csv$", download_path, ignore.case = TRUE)) {
    download_path <- paste0(download_path, ".csv")
  }

  # ------------------------------------------------------------
  # 3. Build GraphQL filters
  # ------------------------------------------------------------

  filters <- list(
    includeStartTime = paste0(
      format(start_date, "%Y-%m-%d"),
      "T00:00:00.000Z"
    ),
    includeEndTime = paste0(
      format(end_date, "%Y-%m-%d"),
      "T00:00:00.000Z"
    )
  )

  if (!is.null(species)) {
    filters$includeSpecies <- as.list(species)
  }

  if (!is.null(animals)) {
    filters$includeAnimals <- as.list(animals)
  }

  if (!is.null(receivers)) {
    filters$includeReceiverSerials <- as.list(receivers)
  }

  if (!is.null(receiver_models)) {
    filters$includeReceiverModels <- as.list(receiver_models)
  }

  if (!is.null(stations)) {
    filters$includeStations <- as.list(stations)
  }

  if (!is.null(transmitter_models)) {
    filters$includeTransmitterModels <- as.list(transmitter_models)
  }

  if (!is.null(full_ids)) {
    filters$includeTransmitterIDs <- as.list(full_ids)
  }

  # ------------------------------------------------------------
  # 4. Select CSV columns
  # ------------------------------------------------------------

  columns <- c(
    "device_time_utc",
    "full_id",
    "receiver_serial",
    "station",
    "sensor_value",
    "sensor_type",
    "signal_strength_db",
    "noise_db",
    "animal_name",
    "species",
    "file_names",
    "transmitter_model",
    "receiver_model",
    "sensor_adc",
    "latitude",
    "longitude"
  )

  if (corrected_time) {
    columns <- c(
      "time_utc",
      "time_correction_sec",
      columns
    )
  }

  # ------------------------------------------------------------
  # 5. Start server-side CSV export
  # ------------------------------------------------------------

  mutation <- paste(
    "mutation startDownloadDetections(",
    "$studyId: ID,",
    "$filters: DetectionFilterInput,",
    "$includeFiles: Boolean,",
    "$correctedTime: Boolean,",
    "$columns: [DetectionCsvColumn!]",
    ") {",
    "startDownloadDetections(",
    "studyId: $studyId",
    "filters: $filters",
    "includeFiles: $includeFiles",
    "correctedTime: $correctedTime",
    "columns: $columns",
    ") {",
    "id",
    "percentComplete",
    "complete",
    "apiPath",
    "error",
    "errorMsg",
    "creationTime",
    "}",
    "}"
  )

  variables <- list(
    studyId = as.character(study_id),
    filters = filters,
    includeFiles = TRUE,
    correctedTime = corrected_time,
    columns = as.list(columns)
  )

  message("Submitting detection export to Fathom...")

  response <- httr::POST(
    url = "https://graph.fathomcentral.com/graphql",
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", token),
      "workspace-id" = as.character(ws_id)
    ),
    body = list(
      query = mutation,
      variables = variables
    ),
    encode = "json"
  )

  httr::stop_for_status(response)

  response_content <- jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )

  if (!is.null(response_content$errors)) {
    error_messages <- vapply(
      response_content$errors,
      function(x) x$message,
      character(1)
    )

    stop(
      "Fathom GraphQL error: ",
      paste(error_messages, collapse = "; ")
    )
  }

  export <- response_content$data$startDownloadDetections

  if (is.null(export)) {
    stop("Fathom did not return a detection export.")
  }

  if (isTRUE(export$error)) {
    stop(
      "Fathom export error: ",
      ifelse(
        is.null(export$errorMsg),
        "Unknown error.",
        export$errorMsg
      )
    )
  }

  # ------------------------------------------------------------
  # 6. Check export status
  # ------------------------------------------------------------

  if (!isTRUE(export$complete)) {
    stop(
      "The Fathom export has not completed yet. ",
      "Export ID: ", export$id,
      ". Completion: ",
      ifelse(
        is.null(export$percentComplete),
        "unknown",
        paste0(export$percentComplete, "%")
      ),
      ". No download was attempted. Please retry the export."
    )
  }

  if (is.null(export$apiPath) || !nzchar(export$apiPath)) {
    stop("Fathom completed the export but did not return a download path.")
  }

  # ------------------------------------------------------------
  # 7. Download CSV
  # ------------------------------------------------------------

  download_url <- paste0(
    "https://graph.fathomcentral.com",
    export$apiPath
  )

  message("Export complete. Downloading CSV...")

  download_response <- httr::GET(
    url = download_url,
    httr::add_headers(
      "Authorization" = paste("Bearer", token),
      "workspace-id" = as.character(ws_id)
    ),
    httr::write_disk(
      path = download_path,
      overwrite = TRUE
    )
  )

  httr::stop_for_status(download_response)

  # ------------------------------------------------------------
  # 8. Import CSV into R
  # ------------------------------------------------------------

  if (!file.exists(download_path)) {
    stop("The CSV download failed: output file was not created.")
  }

  if (file.info(download_path)$size == 0) {
    stop("The downloaded CSV is empty.")
  }

  detections <- utils::read.csv(
    download_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  message(
    "Downloaded ",
    format(nrow(detections), big.mark = ","),
    " detections."
  )

  message("CSV saved to: ", normalizePath(download_path))

  return(detections)
}
