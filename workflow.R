library("netmathtools2")
library("data.table")
library("lubridate")
library("purrr")
library("shiny")
library("ggplot2")

year_start <- floor_date(Sys.Date(), "year")
bw_ending  <- floor_date(Sys.Date(), "week") - 1L
bw_tp      <- ceiling((bw_ending - min(year_start)) / dweeks(1L) / 2L) + 1

tries <- 0L
students <- NULL
while (is.null(students)) {

  h <- composeNexusHandle("mkemp6")

  # students <- getStudents(h, "mkemp6")
  students <- tryCatch(getStudents(h, "mkemp6"), error = function(e) NULL)

  tries <- tries + 1
  cat(paste("\rTried", tries, "times"))
}

active_xgr_ids <- c("koleske2", "turner22")
active_xgr_start <- as.Date("2018-08-27")
active_xgr_end   <- as.Date("2018-12-12")
students[student_netid %in% active_xgr_ids, `:=`(
    start_date = active_xgr_start
  , end_date   = active_xgr_end
  , start_days = as.integer(Sys.Date() - active_xgr_start)
  , end_days   = as.integer(active_xgr_end - Sys.Date())
)]

# exclude finished students not being handled by the automated process
finished_xgr_ids <- c("jkim619", "konicek2", "mmbeasl2", "msalis2", "mabusch2")
active_students <- students[!student_netid %in% finished_xgr_ids & end_days > -30]



tries <- 0L
student_prog <- NULL
while (is.null(student_prog)) {

  h <- composeNexusHandle("mkemp6")

  # student_prog <- getStudentsProgress(h, active_students)
  student_prog <- tryCatch(getStudentsProgress(h, active_students), error = function(e) NULL)

  tries <- tries + 1
  cat(paste("\rTried", tries, "times"))
}

chartProgress(student_prog)
