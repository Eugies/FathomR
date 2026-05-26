# —————————————————————————————————————————
# Deployments (Fathom API)
# —————————————————————————————————————————

#' Get Deployment Metadata
#'
#' Retrieves receiver deployment metadata from the Fathom database,
#' including receiver serial numbers, station names, and deployment dates.
#'
#' @param token Optional Fathom API token
#' @param ws_id Optional workspace ID
#'
#' @return Tibble with deployment metadata
#' @export
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

  dat <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8"),
    flatten = TRUE
  )$data$deployments

  # ————————————————————————————————
  # SAFE receiver extraction (ALL serials)
  # ————————————————————————————————
  extract_receivers <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_character_)

    serials <- tryCatch({
      unlist(lapply(x, function(z) {
        if (!is.null(z$device$serial)) return(z$device$serial)
        NA_character_
      }))
    }, error = function(e) NA_character_)

    serials <- serials[!is.na(serials)]

    if (length(serials) == 0) {
      NA_character_
    } else {
      paste(unique(serials), collapse = "|")
    }
  }

  tibble::tibble(
    Receiver = purrr::map_chr(dat$deviceAttachments, extract_receivers),
    Station.name = dat$station.name,
    Station.id = dat$station.id,
    Start = as.POSIXct(dat$start, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    Stop  = as.POSIXct(dat$end,   format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  ) %>%
    dplyr::filter(!is.na(Receiver))
}
