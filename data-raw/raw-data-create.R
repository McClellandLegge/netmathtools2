schedules <- list(
    `MM461` = data.table::fread("data-raw/netmath461.csv")
  , `MM461EXT` = data.table::fread("data-raw/netmath461EXT.csv")
  , `MM461EGR` = data.table::fread("data-raw/netmath461EGR.csv")
  , `MM461HS` = data.table::fread("data-raw/netmath461HS.csv")
)

status <- data.table::data.table(
  low = c(-999, 1, 3, 4)
  , high = c(0, 2, 3, 999)
  , ProgressStatus = c("Green", "Yellow", "Orange", "Red")
)

api_endpoint <- "https://nexus.netmath.illinois.edu/api"

mathable_hname <- "courseware.illinois.edu"
mathable_url <- paste0("https://", mathable_hname, "/")

mathable_endpoint <- paste0(mathable_url, "WSAPI.asmx")

usethis::use_data(api_endpoint, mathable_hname, mathable_endpoint, mathable_url, status, schedules, internal = TRUE, overwrite = TRUE)
