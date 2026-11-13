

# FathomR <img src="https://raw.githubusercontent.com/Eugies/FathomR/main/man/figures/logo.png" align="right" height="140"/>

**FathomR** is an R package designed to interface with the [Fathom Central](https://fathomcentral.com/learn) API. It allows users to authenticate and retrieve telemetry, receiver deployment, spatial, and biometric data from acoustic telemetry studies. Detection data can also be filtered and exported directly from Fathom. The resulting data frames are designed to support acoustic telemetry analyses, including workflows using the [actel](https://github.com/hugomflavio/actel) package.

## Installation

Install FathomR directly from GitHub using `devtools`:

```r
install.packages("devtools")
devtools::install_github("Eugies/FathomR")
```

Load the package:

```r
library(FathomR)
```

## Authentication

Authenticate with your Fathom account:

```r
auth <- authenticate_wrapper()
```

The authentication wrapper prompts for your email address and password, then retrieves the available workspaces and studies associated with your account.

The returned object contains:

- `auth$token`: Authentication token.
- `auth$ws_id`: Selected workspace ID.
- `auth$study_id`: Selected study ID.

These values are used by the data retrieval functions throughout the package.

## Receiver deployments

Retrieve receiver deployment information for the selected study:

```r
deployments <- get_deployments(
  token = auth$token,
  ws_id = auth$ws_id,
  study_id = auth$study_id
)
```

The returned data frame contains receiver deployment information, including receiver serial number, station name, deployment start time, and deployment end time. Datetimes are returned in UTC.

## Spatial data

Retrieve receiver and station location information:

```r
spatial <- get_spatial(
  StationName = "all",
  # StationName = c("BB", "Creole"),
  # Receivers = c("1361", "800"),
  Receivers = "all",
  # Area = list(lon = c(-89, -88), lat = c(30.2, 30.6)),
  token = auth$token,
  ws_id = auth$ws_id
)
```

The function allows spatial data to be selected using station names, receiver serial numbers, or a geographic area.

## Biometrics

Retrieve biometric information associated with animals in the selected study:

```r
biometrics <- get_biometrics(
  token = auth$token,
  ws_id = auth$ws_id,
  study_id = auth$study_id,
  include_recaptures = TRUE,
  tz = "UTC"
)
```

Arguments:

- `include_recaptures`: Whether to include recapture events. Defaults to `TRUE`.
- `tz`: Time zone used for returned datetime values. Defaults to `"UTC"`.

The returned data frame contains animal biometric and capture-event information.

## Detection data

Retrieve acoustic detection data using server-side filtering and CSV export:

```r
detections <- get_detections(
  token = auth$token,
  ws_id = auth$ws_id,
  study_id = auth$study_id,
  start_date = as.Date("2025-01-01"),
  end_date = as.Date("2025-01-05"),
  corrected_time = TRUE,
  species = "Sheepshead (MDMR)",
  receiver_models = c("VR2Tx-69", "VR2W-69")
)
```

The function submits the requested filters to Fathom and downloads the resulting CSV export. The CSV is automatically imported into R and returned as a data frame. If no download path is supplied, the function prompts the user to select where the CSV file should be saved.

### Available filters

| Argument | Description |
|---|---|
| `start_date` | Start date for the detection query |
| `end_date` | End date for the detection query |
| `receivers` | Receiver serial numbers |
| `receiver_models` | Receiver models |
| `stations` | Station IDs |
| `transmitter_models` | Transmitter models |
| `full_ids` | Transmitter full IDs |
| `animals` | Animal IDs |
| `species` | Species names |
| `corrected_time` | Whether to use corrected detection times |
| `download_path` | File path where the CSV export will be saved |

Filters can be combined to retrieve a specific subset of detection data. Leave optional filters as `NULL` to avoid filtering by those fields.

For example, retrieve all detections for a date range:

```r
detections <- get_detections(
  token = auth$token,
  ws_id = auth$ws_id,
  study_id = auth$study_id,
  start_date = as.Date("2025-01-01"),
  end_date = as.Date("2025-01-31")
)
```

To use uncorrected detection times:

```r
detections <- get_detections(
  token = auth$token,
  ws_id = auth$ws_id,
  study_id = auth$study_id,
  start_date = as.Date("2025-01-01"),
  end_date = as.Date("2025-01-31"),
  corrected_time = FALSE
)
```

## Dependencies

FathomR uses the following R packages:

- `httr`
- `jsonlite`
- `rstudioapi`

The detection export workflow also uses `tcltk` for the interactive file-save dialog when `download_path` is not supplied.

## Issues and contributions

For bug reports, feature requests, or contributions, please visit the [FathomR GitHub repository](https://github.com/Eugies/FathomR).



