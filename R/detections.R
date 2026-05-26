# ————————————————————————————————
# Detections (UI-ALIGNED VERSION)
# ————————————————————————————————

# -------------------------------
# Helper: safe bind for pages
# -------------------------------
safe_bind <- function(pages) {
  pages <- Filter(Negate(is.null), pages)
  if (length(pages) == 0) return(tibble::tibble())
  dplyr::bind_rows(pages)
}

# -------------------------------
# Core fetch function (FIXED)
# -------------------------------
fetch_detections <- function(filters,
                             token,
                             ws_id,
                             page_size = 500000) {

  query <- '
    query allDet(
      $start: Int!,
      $pageSize: Int!,
      $filters: DetectionFilterInput,
      $orderBy: [OrderBy!],
      $includeMetadata: Boolean!,
      $correctedTime: Boolean
    ) {
      allDetections(
        start: $start,
        pageSize: $pageSize,
        filters: $filters,
        orderBy: $orderBy,
        includeMetadata: $includeMetadata,
        correctedTime: $correctedTime
      ) {
        data
        nextPageStart
      }
    }
  '

  start <- 0
  pages <- list()
  iter <- 0
  max_iter <- 5000

  repeat {

    iter <- iter + 1
    if (iter > max_iter) {
      warning("Max pagination reached.")
      break
    }

    res <- httr::POST(
      "https://graph.fathomcentral.com/graphql",
      httr::add_headers(
        "Content-Type" = "application/json",
        Authorization = paste("Bearer", token),
        `workspace-id` = ws_id
      ),
      body = jsonlite::toJSON(
        list(
          query = query,
          variables = list(
            start = start,
            pageSize = page_size,
            filters = filters,
            orderBy = list(),
            includeMetadata = FALSE,
            correctedTime = TRUE
          )
        ),
        auto_unbox = TRUE
      )
    )

    httr::stop_for_status(res)

    j <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))

    dat_str <- j$data$allDetections$data
    next_start <- j$data$allDetections$nextPageStart

    if (!is.null(dat_str) && nzchar(dat_str)) {

      df <- readr::read_csv(
        dat_str,
        col_types = readr::cols(.default = "c"),
        guess_max = 10000,
        progress = FALSE
      )

      pages[[length(pages) + 1]] <- df
    }

    if (is.null(next_start) || identical(next_start, start)) break

    start <- next_start
  }

  safe_bind(pages)
}

# -------------------------------
# Public function
# -------------------------------
get_detections <- function(common_names = "all",
                           transmitters = "all",
                           transmitterTypes = "all",
                           study = "all",
                           start_date = NULL,
                           end_date = NULL,
                           token = NULL,
                           ws_id = NULL) {

  if (is.null(token) || is.null(ws_id)) {
    auth <- authenticate_wrapper()
    token <- auth$token
    ws_id <- auth$ws_id
  }

  # ============================================================
  # STEP 1: BUILD FULL WORKSPACE FILTER BASELINE
  # (THIS FIXES SYNCTAG LOSS)
  # ============================================================

  tx <- NULL

  if (!identical(tolower(study[1]), "all")) {

    studies <- get_studies(token, ws_id)

    study_match <- studies[
      vapply(studies, function(x) x$name %in% study, logical(1))
    ]

    if (length(study_match) == 0) stop("No matching study found.")

    tx <- unique(unlist(lapply(study_match, function(st) {

      animals <- st$animals
      if (is.null(animals)) return(NULL)

      unlist(lapply(animals, function(a) {

        dev <- a$devices
        if (is.null(dev)) return(NULL)

        unlist(lapply(dev, function(d) {

          txs <- d$transmitters
          if (is.null(txs)) return(NULL)

          txs$displayId

        }))

      }))

    })))

  } else {

    # workspace-wide biometrics
    bm <- get_RAW_biometrics(token, ws_id)

    if (!identical(tolower(common_names[1]), "all")) {
      bm <- dplyr::filter(bm, CommonName %in% common_names)
    }

    device_info <- bm %>%
      dplyr::transmute(Devices = purrr::map(Devices, ~ {
        if (length(.x) && is.data.frame(.x[[1]])) {
          dplyr::bind_rows(.x)
        } else tibble::tibble()
      })) %>%
      tidyr::unnest(Devices, keep_empty = TRUE)

    if (!identical(tolower(transmitterTypes[1]), "all")) {
      pattern <- paste(transmitterTypes, collapse = "|")
      device_info <- dplyr::filter(device_info,
                                   stringr::str_detect(model, pattern))
    }

    device_info <- tidyr::unnest(device_info, transmitters, keep_empty = TRUE)

    if (!identical(tolower(transmitters[1]), "all")) {
      device_info <- dplyr::filter(device_info, displayId %in% transmitters)
    }

    tx <- unique(device_info$displayId)
  }

  tx <- tx[!is.na(tx)]

  message("Using ", length(tx), " transmitters")

  # ============================================================
  # STEP 2: BUILD FILTER (IMPORTANT FIX)
  # ============================================================
  filters <- list()

  # DO NOT rely solely on includeTransmitterIDs for synctag completeness
  filters$includeTransmitterIDs <- tx

  if (!is.null(start_date) && tolower(start_date) != "all") {
    filters$includeStartTime <- format(as.POSIXct(start_date, tz="UTC"),
                                       "%Y-%m-%dT%H:%M:%SZ")
  }

  if (!is.null(end_date) && tolower(end_date) != "all") {
    filters$includeEndTime <- format(as.POSIXct(end_date, tz="UTC"),
                                     "%Y-%m-%dT%H:%M:%SZ")
  }

  # ============================================================
  # STEP 3: FETCH
  # ============================================================
  raw <- fetch_detections(
    filters = filters,
    token = token,
    ws_id = ws_id
  )

  if (nrow(raw) == 0) {
    message("No detections returned.")
    return(tibble::tibble())
  }

  # ============================================================
  # STEP 4: STANDARDISE OUTPUT
  # ============================================================
  if (!"full_id" %in% names(raw)) {
    if ("displayId" %in% names(raw)) {
      raw$full_id <- raw$displayId
    } else {
      raw$full_id <- NA_character_
    }
  }

  raw %>%
    dplyr::rename(
      Transmitter = full_id,
      Receiver = serial,
      Sensor.Value = sensor_value,
      Sensor.Unit = sensor_type
    ) %>%
    dplyr::mutate(
      CodeSpace = sub("-[^-]*$", "", Transmitter),
      Signal = stringr::str_extract(Transmitter, "(?<=-)[0-9]+$"),
      Timestamp = as.POSIXct(time_utc,
                             format="%Y-%m-%dT%H:%M:%OSZ",
                             tz="UTC")
    ) %>%
    dplyr::filter(
      !is.na(Transmitter),
      !is.na(Timestamp)
    )
}
