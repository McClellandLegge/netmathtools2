library("netmathtools2")
library("data.table")
library("plotly")

netid <- "mkemp6"
dobw <- FALSE

tries <- 0L
students <- NULL
while (is.null(students)) {

  h <- composeNexusHandle(netid)

  # students <- getStudents(h, netid)
  students <- tryCatch(getStudents(h, netid), error = function(e) NULL)

  tries <- tries + 1
  cat(paste("\rTried", tries, "times"))
}

tries <- 0L
student_prog <- NULL
while (is.null(student_prog)) {

  h <- composeNexusHandle(netid)

  # student_prog <- getStudentsProgress(h, students)
  student_prog <- tryCatch(getStudentsProgress(h, students), error = function(e) NULL)

  tries <- tries + 1
  cat(paste("\rTried", tries, "times"))
}

customized_student_prog <- student_prog[, .(
  full
  , end_days
  , orientation_date
  , timeline
  , has_proctor
  , days_last_mentor_email
  , days_last_student_email
  , days_behind
  , tryits_behind
  , lessons_behind
  , current_pace_interp
  , needed_pace_interp
  , completed_assignments
  , exams
  , end_date
)]

chartProgress(customized_student_prog)


if (dobw == TRUE) {

  # Mentor Activity ---------------------------------------------------------

  emails     <- getMentorActivity(h, students)
  all_emails <- unlist(emails, use.names = FALSE)
  all_dates  <- data.table(date = anytime::anydate(all_emails))
  date_sum   <- all_dates[, .(emails = .N), by = date][order(date)]

  # Course ------------------------------------------------------------------

  all_graded_assignments <- lapply(students$mathable_course_id, getAsssignmentDetail, netid = netid)

  aga_dt <- rbindlist(all_graded_assignments)
  aga_dt[, `:=`(date = anytime::anydate(graded))]

  aga_sum <- aga_dt[, .(mathable = .N), by = date]

  plot_dat <- merge(date_sum, aga_sum, by = "date", all = TRUE)

  plot_dat[is.na(plot_dat)] <- 0

  plot_dat[, `:=`(total = 15 * emails + 7 * mathable + 1)]

  m_plot_dat <- melt(plot_dat, id.vars = c("date", "total"), variable.factor = TRUE)

  bw_ending <- as.Date("2018-04-07", format = "%Y-%m-%d")
  bw_period <- seq(bw_ending - 13, bw_ending, by = "day")

  bw_plot <- m_plot_dat[date %in% bw_period]
  bw_totals <- unique(bw_plot[, .(date, total)])

  p <- ggplot(bw_plot, aes(x = date, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_date(date_breaks = "day", date_labels = "%a, %b %d") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    geom_text(data = bw_totals, aes(x = date, y = 0, label = total), inherit.aes = FALSE)

  sum(bw_totals$total) / 60

  ggplotly(p)
}
