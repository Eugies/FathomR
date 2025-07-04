% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FathomR_Functions.R
\name{get_biometrics}
\alias{get_biometrics}
\title{Get Biometrics for Tagged Animals}
\usage{
get_biometrics()
}
\arguments{
\item{token}{Optional. Fathom API token. If \code{NULL}, \code{authenticate_wrapper()} is called.}

\item{ws_id}{Optional. Workspace ID. If \code{NULL}, \code{authenticate_wrapper()} is called.}
}
\value{
A tibble with biometric metadata for each tagged animal. Includes fields like:
\describe{
\item{CommonName}{Species common name.}
\item{AnimalId}{Unique ID of the animal.}
\item{Sex}{Sex of the animal (if recorded).}
\item{Length}{Length of the animal (if recorded).}
\item{Weight}{Weight of the animal (if recorded).}
\item{Devices}{List-column with device and tag information.}
}
}
\description{
Retrieves biometric metadata for all animals and their associated tags in the Fathom workspace.
}
\examples{
\dontrun{
auth <- authenticate_wrapper()
biometrics <- get_biometrics(token = auth$token, ws_id = auth$ws_id)
head(biometrics)
}
}
