### github version ###
remove.packages("FathomR")
devtools::install_github("eugies/FathomR", force = TRUE)
library(FathomR)
auth <- authenticate_wrapper()
bio_old <- get_biometrics(auth$token, auth$ws_id)
View(bio_old)
# local github version ###
# set branch as "main"
system("git checkout main")
# laod local
devtools::load_all()
auth <- authenticate_wrapper()
biom <- get_biometrics(auth$token, auth$ws_id)
View(biom)

### lead test version ####

# test
devtools::load_all()
auth <- authenticate_wrapper()
#
# ev_biom <- get_event_biometrics(auth$token, auth$ws_id)
# ev_biom <- get_event_biometrics(auth$token, auth$ws_id, species = "Gulf Sturgeon")
#
#
# det <- get_detections(
#   token = auth$token,
#   ws_id = auth$ws_id
# )
# View(det)
# nrow(det) # 900 000
# head(det)
#
# det_2020 <- get_detections(
#   token = auth$token,
#   ws_id = auth$ws_id,
#   start_date = "2020-01-01",
#   end_date   = "2020-12-31",
#   study = "EMEL"
# )
# nrow(det_2020) # 801051 or # 790104 for only EMEL study
#
# det_2021 <- get_detections(
#   token = auth$token,
#   ws_id = auth$ws_id,
#   start_date = "2021-01-01",
#   end_date   = "2021-12-31"
# )
# nrow(det_2021) # 2 155 210
#
# det_2022 <- get_detections(
#   token = auth$token,
#   ws_id = auth$ws_id,
#   start_date = "2022-01-01",
#   end_date   = "2022-12-31"
# )
# nrow(det_2022) # 6 936 304
#
# det_2023 <- get_detections(
#   token = auth$token,
#   ws_id = auth$ws_id,
#   start_date = "2023-01-01",
#   end_date   = "2023-12-31"
# )
# nrow(det_2023) # 7 555 818

emel_det_2024 <- get_detections(
  token = auth$token,
  ws_id = auth$ws_id,
  study = "EMEL",
  #transmitters = "A69-1602-65015",
  start_date = "2024-01-01",
  end_date   = "2025-01-01"
)
nrow(emel_det_2024) # 6 517 762

all_det_2024 <- get_detections(
  token = auth$token,
  ws_id = auth$ws_id,
  #study = "EMEL",
  #transmitters = "A69-1602-65015",
  start_date = "2024-01-01",
  end_date   = "2025-01-01"
)
nrow(all_det_2024) # 11 665 477



det_2024%>%
  #filter(Transmitter == "A69-9004-14370")
  filter(Transmitter == "A69-1602-65015")

det_2025 <- get_detections(
  token = auth$token,
  ws_id = auth$ws_id,
  start_date = "2025-01-01",
  end_date   = "2025-12-31"
)
nrow(det_2025) # 6 780 141

# compare
library(readr)
detections_2026_04_20_15_58_44 <- read_csv("E:/Fathom Dets/detections_2026-04-20_15-58-44.csv")
View(detections_2026_04_20_15_58_44)

detections_2026_04_20_15_58_44%>%
  #filter(Transmitter == "A69-9004-14370")
  filter(`Full ID` == "A69-1602-65015")
rm(detections_2026_04_20_15_58_44)
# start: 2024-01-01 00:00:03
# end: 2026-04-15 17:18:54
nrow(detections_2026_04_20_15_58_44) #24 196 825
head(detections_2026_04_20_15_58_44)
# only 2024:
detections_2024 <- detections_2026_04_20_15_58_44 %>%
  filter(`Device Time (UTC)` >= as.POSIXct("2024-01-01 00:00:00") &
           `Device Time (UTC)` <  as.POSIXct("2025-01-01 00:00:00"))
nrow(detections_2024) #10 743 149
detections_2024 %>%
  summarise(n_transmitters = n_distinct(`Full ID`))
# 2885
detections_2024 %>%
  summarise(n = n_distinct(`Receiver Serial`))
#225


# my dets
dets <- get_detections(
  token = auth$token,
  ws_id = auth$ws_id,
  start_date = "2024-01-01",
  end_date   = "2026-04-15",
  study = "EMEL"
)
nrow(dets) #1826 transmitters and #12 755 277 detections which is way less than what is on Fathom central
head(dets)
library(dplyr)
dets %>%
  summarise(n_transmitters = n_distinct(Transmitter))
# 832
dets %>%
  summarise(n = n_distinct(Receiver))
# 334


# old (github)
remove.packages("FathomR")
devtools::install_github("eugies/FathomR", force = TRUE)
library(FathomR)
auth <- authenticate_wrapper()
dets_old <- get_detections(
  token = auth$token,
  ws_id = auth$ws_id,
  start_date = "2024-01-01",
  end_date   = "2026-04-15"
)
nrow(dets_old)   # 14 915 179   way less than is on Fathom Central
dets_old %>%
  summarise(n_transmitters = n_distinct(Transmitter))
#
dets_old %>%
  summarise(n = n_distinct(Receiver))
#


# local github version ###
# set branch as "main"
system("git checkout main")
# laod local
devtools::load_all()
auth <- authenticate_wrapper()
biom <- get_biometrics(auth$token, auth$ws_id)
View(biom)




detections_all <- get_detections(
  #common_names = "all", # all common names
  #common_names = c("Cownose","Gulf Sturgeon"), # specify common name
  #transmitters = "all",
  #transmitters = c("A69-1604-60495","A69-1303-46570","A69-9001-16605"), # For example: To Specify specific Transmitters
  #transmitterTypes = "all",      # for all transmitter types
  #transmitterTypes = c("V13","V16"),  # For example: to only get V13 and V16 transmitter types
  #transmitterTypes = c("V9"),
  #start_date = "2025-01-01", # use "all" or leave unspecified for all dates, or specify a start date in yyyy-mm-dd
  #end_date = "all", # use "all" or leave unspecified for all dates, or specify an end date in yyyy-mm-dd
  token = auth$token,  # if not specified then authentication will prompt a new login
  ws_id = auth$ws_id  # if not specified then authentication will prompt a new login
)
View(detections_all) # getting 18,843,946, but should be 45,147,345 detections in total
# 15 pages with errors:
# I get 5 pages with: "Could not parse CSV on page 2: duplicate 'row.names' are not allowed"
# and 10 pages with: "Could not parse CSV on page 16: more columns than column names"


dep <- get_deployments(
  token = auth$token,
  ws_id = auth$ws_id)
View(dep)
nrow(dep)

space <- get_spatial(
  token = auth$token,
  ws_id = auth$ws_id)
View(space)

