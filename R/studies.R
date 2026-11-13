
# ============================================================
# Studies
# ============================================================


#' Get available studies
#'
#' Returns a lightweight list of studies in a workspace.
#'
#' @param token Authentication token.
#' @param ws_id Workspace ID.
#'
#' @return Data frame containing study IDs and names.
#' @export
get_studies <- function(token, ws_id) {

  query <- list(
    query = "
      query {
        studies {
          id
          name
        }
      }
    "
  )

  res <- httr::POST(
    "https://graph.fathomcentral.com/graphql",
    httr::add_headers(
      "Content-Type" = "application/json",
      Authorization = paste("Bearer", token),
      `workspace-id` = as.character(ws_id)
    ),
    body = jsonlite::toJSON(query, auto_unbox = TRUE)
  )

  httr::stop_for_status(res)

  result <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8")
  )

  if (!is.null(result$errors)) {
    stop(result$errors[[1]]$message)
  }

  studies <- result$data$studies

  if (is.null(studies) || length(studies) == 0) {
    return(data.frame(
      id = character(),
      name = character()
    ))
  }

  return(studies)
}


# ============================================================
# GET FULL STUDY DETAIL
# ============================================================

#' Get full study structure including animals and devices
#'
#' @param token Authentication token.
#' @param ws_id Workspace ID.
#' @param study_id Study ID.
#'
#' @return Nested study object.
#' @keywords internal
get_study_detail <- function(token, ws_id, study_id) {

  query <- list(
    query = "
      query ($id: ID!) {
        study(id: $id) {
          id
          name
          animals {
            id
            devices {
              transmitters {
                displayId
              }
            }
          }
        }
      }
    ",
    variables = list(id = study_id)
  )

  res <- httr::POST(
    "https://graph.fathomcentral.com/graphql",
    httr::add_headers(
      "Content-Type" = "application/json",
      Authorization = paste("Bearer", token),
      `workspace-id` = as.character(ws_id)
    ),
    body = jsonlite::toJSON(query, auto_unbox = TRUE)
  )

  httr::stop_for_status(res)

  result <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8")
  )

  if (!is.null(result$errors)) {
    stop(result$errors[[1]]$message)
  }

  result$data$study
}


# ============================================================
# RESOLVE STUDY
# ============================================================

#' Resolve a study by name
#'
#' Returns the full study object.
#'
#' @param token Authentication token.
#' @param ws_id Workspace ID.
#' @param study Study name.
#'
#' @return Full study object.
#' @keywords internal
resolve_study <- function(token, ws_id, study = NULL) {

  studies <- get_studies(token, ws_id)

  if (is.null(study)) {

    cat("\nAvailable studies:\n\n")
    print(studies)

    study <- readline("\nStudy name (exact): ")
  }

  idx <- which(studies$name == study)

  if (length(idx) != 1) {
    stop(
      paste0("Study '", study, "' not found or is not unique."),
      call. = FALSE
    )
  }

  study_id <- studies$id[idx]

  get_study_detail(token, ws_id, study_id)
}


# ============================================================
# EXTRACT TRANSMITTERS
# ============================================================

#' Extract transmitter IDs from full study object
#'
#' @param study Full study object.
#'
#' @return Character vector of transmitter IDs.
#' @keywords internal
extract_study_transmitters <- function(study) {

  animals <- study$animals

  if (is.null(animals) || !is.data.frame(animals)) {
    return(character(0))
  }

  if (!"devices" %in% names(animals)) {
    return(character(0))
  }

  tx_raw <- animals$devices

  tx_raw <- tx_raw[!is.na(tx_raw) & tx_raw != ""]

  tx_split <- strsplit(tx_raw, "\\s*,\\s*")

  tx <- unlist(tx_split, use.names = FALSE)

  tx <- trimws(tx)

  tx <- tx[tx != "" & !is.na(tx)]

  unique(tx)
}
