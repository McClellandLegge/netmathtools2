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

# Mentor Activity ---------------------------------------------------------

h          <- composeNexusHandle("mkemp6")
emails     <- getMentorActivity(h, students)
all_emails <- unlist(emails, use.names = FALSE)
all_dates  <- data.table(date = anytime::anydate(all_emails))
date_sum   <- all_dates[, .(emails = .N), by = date][order(date)]

# Course ------------------------------------------------------------------

all_graded_assignments <- lapply(students$mathable_course_id, getAsssignmentDetail, netid = netid)

aga_dt <- rbindlist(all_graded_assignments)
aga_dt[, `:=`(date = anytime::anydate(graded))]

aga_sum   <- aga_dt[, .(mathable = .N), by = date]
plot_dat  <- merge(date_sum, aga_sum, by = "date", all = TRUE)
plot_dat[, student_progress_check := 15]

bw_period <- data.table(date = seq(bw_ending - 13, bw_ending, by = "day"))
bw_plot   <- merge(plot_dat, bw_period, by = "date", all.y = TRUE)

bw_plot[is.na(bw_plot)] <- 0
bw_plot[, `:=`(total = 15 * emails + 7 * mathable + student_progress_check)]

bw_totals <- unique(bw_plot[, .(date, total)])

m_plot_dat <- melt(bw_plot, id.vars = c("date", "total"), variable.factor = TRUE)



p <- ggplot(m_plot_dat, aes(x = date, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_date(date_breaks = "day", date_labels = "%a, %b %d") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_text(data = bw_totals, aes(x = date, y = 0, label = total), inherit.aes = FALSE)

sum(bw_totals$total)

p

bw_totals[, total := plyr::round_any(total, 30, f = ceiling) / 60]

write.table(bw_totals, file = "clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)
