# ————————————————————————————————
# Spatial metadata
# ————————————————————————————————

#' Get receiver spatial metadata
#'
#' Retrieves deployment positions, receiver locations, and depth info.
#'
#' @param token Authentication token
#' @param ws_id Workspace ID
#' @param StationNames Station filters ("all" or vector)
#' @param Receivers Receiver filters ("all" or vector)
#' @param Area Optional bounding box list: list(lon=c(), lat=c())
#'
#' @return Tibble of spatial deployment metadata
#' @export
get_spatial <- function(token = NULL,
                        ws_id = NULL,
                        StationNames = "all",
                        Receivers = "all",
                        Area = NULL) {

  if (is.null(token) || is.null(ws_id)) {
    auth <- authenticate_wrapper()
    token <- auth$token
    ws_id <- auth$ws_id
  }

  query <- '
    query listDeployments {
      deployments {
        station {
          name
          id
        }
        deviceAttachments {
          device {
            serial
          }
        }
        positions {
          latLon {
            latitude
            longitude
          }
          depth {
            value
          }
        }
      }
    }
  '

  dat <- fathom_post(query, token, ws_id)

  dat$Receiver <- purrr::map_chr(dat$deviceAttachments, function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    if (is.data.frame(x)) return(x$device.serial[1])
    if (is.list(x)) return(x[[1]]$device$serial)
    NA_character_
  })

  spatial <- dat %>%
    dplyr::select(Receiver, station.name, positions) %>%
    tidyr::unnest(positions, keep_empty = TRUE) %>%
    dplyr::mutate(
      Latitude = positions_latLon.latitude,
      Longitude = positions_latLon.longitude,
      Depth = positions_depth.value,
      Type = "Hydrophone"
    ) %>%
    dplyr::rename(Station.name = station.name) %>%
    dplyr::select(Receiver, Station.name, Latitude, Longitude, Depth, Type)

  if (!identical(StationNames, "all")) {
    spatial <- spatial %>%
      dplyr::filter(
        Reduce(`|`, lapply(StationNames,
                           \(x) grepl(x, Station.name, ignore.case = TRUE)))
      )
  }

  if (!identical(Receivers, "all")) {
    spatial <- dplyr::filter(
      spatial,
      Reduce(`|`, lapply(Receivers,
                         \(x) grepl(x, Receiver)))
    )
  }

  if (!is.null(Area)) {
    lon <- sort(Area$lon)
    lat <- sort(Area$lat)

    spatial <- spatial %>%
      dplyr::filter(
        Longitude >= lon[1], Longitude <= lon[2],
        Latitude >= lat[1], Latitude <= lat[2]
      )
  }

  spatial
}
