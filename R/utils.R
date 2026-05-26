# ————————————————————————————————
# Internal utility functions
# ————————————————————————————————

# Safely extract a data frame from nested API responses
safe_df <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  if (is.data.frame(x)) return(x)
  if (is.list(x) && length(x) > 0 && is.data.frame(x[[1]])) return(x[[1]])
  return(NULL)
}

# Safely extract a value from a data frame column
safe_val <- function(df, col, type = "char") {
  if (!is.null(df) && !is.null(col) && col %in% names(df) && nrow(df) > 0) {
    val <- df[[col]][1]
    return(val)
  }

  switch(type,
         char = NA_character_,
         num  = NA_real_,
         NA)
}
# Safely extract events (IMPORTANT for biometrics)
safe_events_df <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)

  if (is.data.frame(x)) return(x)

  if (is.list(x) && length(x) > 0 && is.data.frame(x[[1]])) {
    return(x[[1]])
  }

  return(NULL)
}
# Standard GraphQL POST wrapper (optional but VERY useful)
fathom_post <- function(query, token, ws_id, variables = NULL) {
  res <- httr::POST(
    "https://graph.fathomcentral.com/graphql",
    httr::add_headers(
      "Content-Type" = "application/json",
      Authorization = paste("Bearer", token),
      `workspace-id` = ws_id
    ),
    body = jsonlite::toJSON(
      list(query = query, variables = variables),
      auto_unbox = TRUE
    )
  )

  httr::stop_for_status(res)
  jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), flatten = TRUE)
}

# Standard POSIX parsing helper
parse_iso_utc <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}

# —————————————————————————————————————————
# Get Studies (FULL STRUCTURE)
# —————————————————————————————————————————
#' @export
get_studies <- function(token = NULL, ws_id = NULL) {

  if (is.null(token) || is.null(ws_id)) {
    auth <- authenticate_wrapper()
    token <- auth$token
    ws_id <- auth$ws_id
  }

  query <- '
    query {
      studies {
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

  txt <- httr::content(res, "text", encoding = "UTF-8")
  raw <- jsonlite::fromJSON(txt, flatten = FALSE)

  studies <- raw$data$studies

  if (is.null(studies) || length(studies) == 0) {
    stop("No studies found in workspace.")
  }

  # Ensure list format (robust)
  if (is.data.frame(studies)) {
    studies <- split(studies, seq_len(nrow(studies)))
  }

  return(studies)
}
