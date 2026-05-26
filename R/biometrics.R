# ————————————————————————————————
# Biometrics
# ————————————————————————————————

#' Get event-level biometrics dataset
#'
#' Returns one row per event with associated measurements and transmitter info.
#'
#' @param token Authentication token
#' @param ws_id Workspace ID
#' @param species Optional species filter ("all" = no filter)
#'
#' @export
get_event_biometrics <- function(token = NULL, ws_id = NULL, species = "all") {

  if (is.null(token) || is.null(ws_id)) {
    auth <- authenticate_wrapper()
    token <- auth$token
    ws_id <- auth$ws_id
  }

  raw <- get_RAW_biometrics(token, ws_id)

  # ---- SPECIES FILTER ----
  if (!is.null(species) && !("all" %in% tolower(species))) {

    species <- tolower(unlist(strsplit(species, ",")))
    species <- trimws(species)

    keep <- tolower(raw$CommonName) %in% species |
      tolower(raw$Species) %in% species

    raw <- raw[keep, ]
  }

  purrr::map_dfr(seq_along(raw$Nickname), function(i) {

    nickname <- raw$Nickname[[i]]
    cname    <- raw$CommonName[[i]]
    species  <- raw$Species[[i]]

    # ---- EVENTS ----
    events_df <- safe_df(raw$Events[[i]])
    if (is.null(events_df) || nrow(events_df) == 0) return(NULL)

    events_df$EventTime <- parse_iso_utc(events_df$time)

    # ---- MEASUREMENTS ----
    meas_df <- safe_df(raw$MeasurementSets[[i]])
    if (!is.null(meas_df) && nrow(meas_df) > 0) {
      meas_df$MeasurementTime <- parse_iso_utc(meas_df$time)
    }

    # ---- DEVICE FALLBACK ----
    animal_devices <- safe_df(raw$Devices[[i]])

    purrr::map_dfr(seq_len(nrow(events_df)), function(j) {

      event <- events_df[j, ]

      # ---- closest measurement ----
      meas_match <- NULL

      if (!is.null(meas_df) && nrow(meas_df) > 0) {
        meas_match <- meas_df %>%
          dplyr::mutate(
            timediff = abs(as.numeric(difftime(
              MeasurementTime,
              event$EventTime,
              units = "secs"
            )))
          ) %>%
          dplyr::slice_min(timediff, n = 1)
      }

      # ---- EVENT-LEVEL DEVICES ----
      event_devices <- safe_df(event$devices)

      tx <- NA_character_
      tx_model <- NA_character_
      tx_serial <- NA_character_

      extract_tx <- function(dev_df) {
        if (is.null(dev_df) || nrow(dev_df) == 0) return(NULL)

        tx_df <- safe_df(dev_df$transmitters)

        if (!is.null(tx_df) && nrow(tx_df) > 0) {
          paste(unique(tx_df$displayId), collapse = "|")
        } else {
          NA_character_
        }
      }

      if (!is.null(event_devices) && nrow(event_devices) > 0) {

        tx <- extract_tx(event_devices)
        tx_model <- event_devices$model[1]
        tx_serial <- event_devices$serial[1]

      } else if (!is.null(animal_devices) && nrow(animal_devices) > 0) {

        tx <- extract_tx(animal_devices)
        tx_model <- animal_devices$model[1]
        tx_serial <- animal_devices$serial[1]
      }

      # ---- SAFE TIME ----
      meas_time <- if (!is.null(meas_match) &&
                       nrow(meas_match) > 0 &&
                       "MeasurementTime" %in% names(meas_match)) {
        meas_match$MeasurementTime[1]
      } else {
        as.POSIXct(NA, tz = "UTC")
      }

      # ---- OUTPUT ROW ----
      tibble::tibble(

        Nickname = nickname,
        CommonName = cname,
        Species = species,

        EventType = event$`__typename`,
        EventTime = event$EventTime,
        EventLocation = safe_val(event, "locationName"),
        Latitude = safe_val(event$latLon, "latitude", "num"),
        Longitude = safe_val(event$latLon, "longitude", "num"),

        # ---- TRANSMITTER (ACTEL FORMAT) ----
        Transmitter = tx,
        Transmitter.model = tx_model,
        Transmitter.serial = tx_serial,

        # ---- LENGTHS ----
        TotalLength = safe_val(meas_match, "totalLength.value", "num"),
        TotalLengthUnit = safe_val(meas_match, "totalLength.unit"),

        ForkLength = safe_val(meas_match, "forkLength.value", "num"),
        ForkLengthUnit = safe_val(meas_match, "forkLength.unit"),

        StandardLength = safe_val(meas_match, "standardLength.value", "num"),
        StandardLengthUnit = safe_val(meas_match, "standardLength.unit"),

        HoodLength = safe_val(meas_match, "hoodLength.value", "num"),
        HoodLengthUnit = safe_val(meas_match, "hoodLength.unit"),

        Width = safe_val(meas_match, "width.value", "num"),
        WidthUnit = safe_val(meas_match, "width.unit"),

        Girth = safe_val(meas_match, "girth.value", "num"),
        GirthUnit = safe_val(meas_match, "girth.unit"),

        Weight = safe_val(meas_match, "mass.value", "num"),
        WeightUnit = safe_val(meas_match, "mass.unit"),

        Age = safe_val(meas_match, "age.value", "num"),
        AgeUnit = safe_val(meas_match, "age.unit"),

        Sex = safe_val(meas_match, "sex"),
        LifeStage = safe_val(meas_match, "lifeStage"),

        MeasuredBy = safe_val(meas_match, "measurerName"),
        MeasurementTime = meas_time
      )
    })
  })
}
