schedules <- list(
  `MM461` = data.table::fread("data-raw/netmath461.csv")
  , `MM461EGR` = data.table::fread("data-raw/netmath461EGR.csv")
)

status <- data.table::data.table(
  low = c(-999, 1, 3, 4)
  , high = c(0, 2, 3, 999)
  , ProgressStatus = c("Green", "Yellow", "Orange", "Red")
)

api_endpoint <- "https://nexus.netmath.illinois.edu/api"

devtools::use_data(api_endpoint, status, schedules, internal = TRUE, overwrite = TRUE)
