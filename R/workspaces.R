
# ============================================================
# Workspaces
# ============================================================

#' Get available workspaces
#'
#' Queries the Fathom GraphQL API and returns the workspaces
#' available to the authenticated user.
#'
#' @param token Authentication token.
#'
#' @return A data frame containing workspace IDs and names.
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


#' Resolve a workspace name to its ID
#'
#' @param token Authentication token.
#' @param workspace Workspace name. If NULL, the user is prompted.
#'
#' @return Workspace ID.
#' @keywords internal
resolve_workspace <- function(token, workspace = NULL) {

  ws <- get_workspaces(token)

  if (is.null(workspace)) {

    cat("\nAvailable workspaces:\n\n")
    print(ws)

    workspace <- readline("\nWorkspace name (exact): ")
  }

  ws_id <- ws$id[ws$name == workspace]

  if (length(ws_id) != 1) {
    stop(
      paste0("Workspace '", workspace, "' not found."),
      call. = FALSE
    )
  }

  ws_id
}
