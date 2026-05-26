# ————————————————————————————————
# Workspaces
# ————————————————————————————————

#' Get available workspaces
#'
#' Queries Fathom GraphQL API and returns accessible workspaces.
#'
#' @param token Authentication token
#'
#' @return Tibble of workspace IDs and names
#' @export
get_workspaces <- function(token) {

  query <- list(
    query = "query { workspaces { id name } }"
  )

  res <- httr::POST(
    "https://graph.fathomcentral.com/graphql",
    httr::add_headers(
      "Content-Type" = "application/json",
      Authorization = paste("Bearer", token)
    ),
    body = jsonlite::toJSON(query, auto_unbox = TRUE)
  )

  httr::stop_for_status(res)

  txt <- httr::content(res, as = "text", encoding = "UTF-8")

  jsonlite::fromJSON(txt)$data$workspaces
}
