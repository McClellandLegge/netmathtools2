library("netmathtools2")
library("data.table")
library("lubridate")
library("purrr")
library("shiny")
library("ggplot2")

netid      <- "mkemp6"
year_start <- floor_date(Sys.Date(), "year")
bw_ending  <- floor_date(Sys.Date(), "week") - 1L
bw_tp      <- ceiling((bw_ending - min(year_start)) / dweeks(1L) / 2L) + 1
options("netmathtools.mathable.user" = "mkemp6", "netmathtools.mathable.passwd" = "4V9vqrw5")

students <- getStudents("mkemp6") # from nexus

active_xgr_ids <- c("khengst2", "chlasta2")
active_xgr_start <- as.Date("2019-06-10")
active_xgr_end   <- as.Date("2019-08-01")
students[student_netid %in% active_xgr_ids, `:=`(
    start_date = active_xgr_start
  , end_date   = active_xgr_end
  , start_days = as.integer(Sys.Date() - active_xgr_start)
  , end_days   = as.integer(active_xgr_end - Sys.Date())
)]


active_xgr_ids <- c("koleske2", "turner22")
active_xgr_start <- as.Date("2018-08-27")
active_xgr_end   <- as.Date("2018-12-12")
students[student_netid %in% active_xgr_ids, `:=`(
    start_date = active_xgr_start
  , end_date   = active_xgr_end
  , start_days = as.integer(Sys.Date() - active_xgr_start)
  , end_days   = as.integer(active_xgr_end - Sys.Date())
)]

active_xgr_ids <- c("mororke2", "bjburto2")
active_xgr_start <- as.Date("2019-01-14")
active_xgr_end   <- as.Date("2019-05-01")
students[student_netid %in% active_xgr_ids, `:=`(
  start_date = active_xgr_start
  , end_date   = active_xgr_end
  , start_days = as.integer(Sys.Date() - active_xgr_start)
  , end_days   = as.integer(active_xgr_end - Sys.Date())
)]

active_xgr_ids <- c("miaso2", "cfahmad2")
active_xgr_start <- as.Date("2019-08-26")
active_xgr_end   <- as.Date("2019-12-11")
students[student_netid %in% active_xgr_ids, `:=`(
    start_date = active_xgr_start
  , end_date   = active_xgr_end
  , start_days = as.integer(Sys.Date() - active_xgr_start)
  , end_days   = as.integer(active_xgr_end - Sys.Date())
)]


active_xgr_ids <- c("kfarver2", "ingegno2")
active_xgr_start <- as.Date("2020-01-21")
active_xgr_end   <- as.Date("2020-05-06")
students[student_netid %in% active_xgr_ids, `:=`(
  start_date = active_xgr_start
  , end_date   = active_xgr_end
  , start_days = as.integer(Sys.Date() - active_xgr_start)
  , end_days   = as.integer(active_xgr_end - Sys.Date())
)]


active_xgr_ids <- c("mmaynes2", "jlefer2")
active_xgr_start <- as.Date("2020-06-15")
active_xgr_end   <- as.Date("2020-08-06")
students[student_netid %in% active_xgr_ids, `:=`(
  start_date = active_xgr_start
  , end_date   = active_xgr_end
  , start_days = as.integer(Sys.Date() - active_xgr_start)
  , end_days   = as.integer(active_xgr_end - Sys.Date())
)]

# exclude finished students not being handled by the automated process
finished_xgr_ids <- c("jkim619", "konicek2", "mmbeasl2", "msalis2", "mabusch2")
active_students <- students[!student_netid %in% finished_xgr_ids & end_days > -30]


student_prog <- getStudentsProgress(active_students)

# chartProgress(student_prog)

fs::dir_create("app", recurse = TRUE)
fwrite(student_prog, "app/students.csv")

runApp("app/", port = 9999, launch.browser = TRUE, host = "0.0.0.0")

cache_fl <- fs::dir_ls("/home/mlegge/ShinyApps/netmath-students/data/", regex = "students-\\d{4}-\\d{2}-\\d{2}\\.csv") %>%
  sort() %>% tail(1L) %>% return()

students <- fread(cache_fl)
# Mentor Activity ---------------------------------------------------------

emails     <- getMentorActivity(students)
all_emails <- unlist(emails, use.names = FALSE)
all_dates  <- data.table(date = anytime::anydate(all_emails))
date_sum   <- all_dates[, .(emails = .N), by = date][order(date)]

# Course ------------------------------------------------------------------

#
# students$mathable_course_id[5]
# students$mathable_course_id[9]
#
# lk <- getAsssignmentDetail("mkemp6", students$mathable_course_id[2])

all_graded_assignments <- lapply(students$mathable_course_id, getAsssignmentDetail, netid = netid)


aga_dt <- rbindlist(all_graded_assignments)
aga_dt[, `:=`(date = anytime::anydate(graded))]

aga_sum   <- aga_dt[, .(mathable = .N), by = date]
plot_dat  <- merge(date_sum, aga_sum, by = "date", all = TRUE)
plot_dat[, student_progress_check := 15]

bw_period <- data.table(date = seq(bw_ending - 13, bw_ending, by = "day"))
bw_plot   <- merge(plot_dat, bw_period, by = "date", all.y = TRUE)

bw_plot[is.na(bw_plot)] <- 0
bw_plot[, `:=`(total = 10 * emails + 7 * mathable + student_progress_check)]

bw_totals <- unique(bw_plot[, .(date, total)])

m_plot_dat <- melt(bw_plot, id.vars = c("date", "total"), variable.factor = TRUE)



p <- ggplot(m_plot_dat, aes(x = date, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_date(date_breaks = "day", date_labels = "%a, %b %d") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_text(data = bw_totals, aes(x = date, y = 0, label = total), inherit.aes = FALSE)

sum(bw_totals$total) / 60

p

bw_totals[, total := plyr::round_any(total, 30, f = ceiling) / 60]

write.table(bw_totals, file = "clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)

clipr::write_clip(bw_totals, object_type = "table", breaks = "\t")



# Grading -----------------------------------------------------------------

library("futile.logger")

flog.threshold(DEBUG)

student_status <- c('Active', 'Inactive', 'Flagged', 'Withdrawing', 'Withdrawn', 'Completed')

status_res <- map(student_status, ~{

  flog.debug(.)

  # handle for this student's processing
  h <- composeNexusHandle(netid)

  netmathtools2::getRequest(
      handle   = h
    , where    = "nexus"
    , route    = "students"
    , format   = "Mathable"
    , status   = .
  )

})

status_ls <- unlist(status_res, recursive = FALSE) %>%
  keep(~!is.null(.$mathable$courseId) && .$mathable$courseId != "")

library("doParallel")

cl <- makeCluster(3L)
registerDoParallel(cl)


foreach(i = seq_along(status_ls)) %dopar% {

  lk <- status_ls[[i]]

  mentor <- list(
      netid = lk$mentor$netId
    , name  = lk$mentor$name$full
  )

  assignments <- getAsssignmentDetail(NULL, lk$mathable$courseId)

}

mathable$courseId
"students?term=Regular"



# Scratch -----------------------------------------------------------------


# compose the key value pairs for the mathable request header
mathable_cookies    <- with(cookies, paste0(name, "=", value))
mathable_cookie_str <- paste0(mathable_cookies, collapse = ";")

# set a new handle
h <- curl::new_handle()

# compose the handle
curl::handle_setheaders(
  handle             = h
  , "dnt"              = "1"
  , "accept-encoding"  = "gzip, deflate, br"
  , "accept-language"  = "en-US,en;q=0.9"
  , "accept"           = "*/*"
  , "user-agent"       = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.102 Safari/537.36"
  , "connection"       = "keep-alive"
  , "content-type"     = "application/json; charset=UTF-8"
  , "referer"          = mathable_url
  , "authority"        = mathable_hname
  , "x-requested-with" = "XMLHttpRequest"
  , "cookies"          = mathable_cookie_str
)

# don't verify SSL certs
# TODO: figure out why this is necessary and fix if possible
curl::handle_setopt(h, "ssl_verifypeer" = 0L)

return(h)
