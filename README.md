# fathomR <img src="https://raw.githubusercontent.com/Eugies/FathomR/main/man/figures/logo.png" align="right" height="140"/>

**fathomR** is an R package designed to interface with the [Fathom](https://fathomcentral.com/learn) API. It allows users to authenticate, retrieve, and filter telemetry and deployment data from acoustic telemetry studies with ease. The resulting dataframes are in a format compatable with the [actel](https://github.com/hugomflavio/actel) package.

## 📦 Installation

To install the package directly from GitHub, use the following commands in R:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install the package from GitHub
devtools::install_github("Eugies/FathomR")

# Load the package
library(fathomR)
```

## 📦Authentication

To authenticate user login details and select workspace, use the following commands in R:

```r
# Authenticate using Fathom username and password
auth <- authenticate_wrapper()
# This will launch a browser window to log in and return a token and workspace ID
# Use default client ID if asked
```

## 📦Receiver deployments

To fetch all receiver deployment data, use the following commands in R:

```r
# deployments
deployments<-get_deployments(token = auth$token,  # if not specified then authentication will prompt a new login
                             ws_id = auth$ws_id) # if not specified then authentication will prompt a new login
```

## 📦Spatial

To fetch Spatial data of receivers, use the following commands in R:

```r
# spatial
spatial <- get_spatial(
   StationName = "all",
  #StationName = c("BB", "Creole"),     # For example: all stations containing BB or Creole (case sensative)
  #Receivers = c("1361", "800"),        # For example: all receivers containing these substrings
  Receivers = "all",   
  #Area = list(lon = c(30.2, 30.6), lat = c(-89, -88)), #For example: all receivers inside the specified longitude and latitude
  token = auth$token,  # if not specified then authentication will prompt a new login
  ws_id = auth$ws_id   # if not specified then authentication will prompt a new login
)
```

## 📦Biometrics

To fetch biometrics data of tagged individuals, use the following commands in R:

```r
# biometrics
biometrics <- get_biometrics(token = auth$token,  # if not specified then authentication will prompt a new login
                             ws_id = auth$ws_id)  # if not specified then authentication will prompt a new login
```

## 📦Detections

To fetch biometrics data of tagged individuals, use the following commands in R:

```r
# detections
detections <- get_detections(
  common_names = "all", # all common names
  #common_names = c("Cownose',"Gulf Sturgeon), # specify common name
  transmitters = "all",
  #transmitters = c("A69-1604-60495","A69-1303-46570","A69-9001-16605"), # For example: To Specify specific Transmitters
  transmitterTypes = "all",      # for all transmitter types
  #transmitterTypes = c("V13","V16"),  # For example: to only get V13 and V16 transmitter types
  start_date = "all", # use "all" or leave unspecified for all dates, or specify a start date in yyyy-mm-dd
  end_date = "all", # use "all" or leave unspecified for all dates, or specify an end date in yyyy-mm-dd
  token = auth$token,  # if not specified then authentication will prompt a new login
  ws_id = auth$ws_id  # if not specified then authentication will prompt a new login
)
```


