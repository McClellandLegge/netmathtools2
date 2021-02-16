
dplyr_coalesce <- function (...)
{
  if (missing(..1)) {
    abort("At least one argument must be supplied")
  }
  values <- dots_list(...)
  x <- values[[1]]
  values <- values[-1]
  for (i in seq_along(values)) {
    x <- dplyr:::replace_with(x, is.na(x), values[[i]], glue("Argument {i + 1}"),
                      glue("length of {fmt_args(~x)}"))
  }
  x
}


#' Extract Student's Progress in a Course
#'
#' @param notebooks A JSON list
#' @param course_id A character string
#' @param days_left A numeric value
#'
#' @return A \code{\link[data.table]{data.table}} summarizing the student's progress
#' @export
#' @import data.table
extractStudentProgress <- function(notebooks, course_id, days_left) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  futile.logger::flog.debug(paste0("Extracting progress for ", course_id))

  relevant_assignments <- Filter(function(x) {
    x$type == "Homework" & x$courseId == course_id
  }, notebooks)

  # convert each students list of assignments to a data table with select info
  assignment_ls <- lapply(relevant_assignments, netmathtools2::extractAssignment)
  assignments   <- data.table::rbindlist(assignment_ls, fill = TRUE)
  rm(relevant_assignments, assignment_ls)

  # want to exclude the lesson 0 assignments
  graded_assignments    <- assignments[lesson_num != 0L]
  completed_assignments <- graded_assignments[tryit_complete == TRUE]
  at_lesson             <- ifelse(nrow(completed_assignments) > 0L,
                                  max(completed_assignments$lesson_num),
                                  0L)
  data.table::setkey(graded_assignments, lesson_num, tryit_name)
  data.table::setkey(completed_assignments, lesson_num, tryit_name)
  rm(assignments)

  # determine if a HS or EGR section
  is_highschool <- grepl("_hs_", course_id)
  is_xgr        <- grepl("_egr_|_xgr", course_id)

  # find the schedule
  if (is_highschool == TRUE) {
    schedule    <- netmathtools2:::schedules$MM461HS
    course_type <- "HS"
  } else if(is_xgr == TRUE) {
    schedule    <- netmathtools2:::schedules$MM461EGR
    course_type <- "XGR"
  } else {
    # schedule    <- netmathtools2:::schedules$MM461
    schedule    <- netmathtools2:::schedules$MM461EXT
    # course_type <- "REG"
    course_type <- "EXT"
  }

  # if days left is less than 0 then they probably have an extension pending
  # but we'll just act like the course is actually over, flooring at 0
  days_left <- ifelse(days_left < 0L, 0L, days_left)

  # see where the student should be based on the number of days they've been in
  # their course as of today
  if (!is.null(schedule) & !is.na(days_left)) {
    data.table::setkey(schedule, lesson_num, tryit_name)
    graded_schedule <- graded_assignments[schedule]

    course_days     <- max(schedule$day)
    should_schedule <- schedule[day <= course_days - days_left]

    should_day      <- should_schedule[, .(lesson_num, tryit_name)]
    should_lesson   <- ifelse(nrow(should_day) > 0L, max(should_day$lesson_num), 0L)
    should_tryit    <- nrow(unique(should_day))

    at_schedule     <- graded_schedule[tryit_complete == TRUE]
    at_day          <- at_schedule[, max(day, 0, na.rm = TRUE)]
    should_be_day   <- should_schedule[, max(day, 0, na.rm = TRUE)]

    # calculate the number of days attributed to skipped assignments
    days_skipped    <- graded_schedule[day <= at_day & tryit_complete == FALSE, .N]

    days_behind     <- should_be_day - at_day + days_skipped

  } else {
    should_tryit    <- NA
    should_lesson   <- NA
    days_behind     <- NA
  }

  # summarize the progress by pure completion to recognize (somewhat) if the
  # student has skipped assignments. if that is the case then the concept of
  # measuring the difficulty though the schedule pacing skews results a bit
  # but it is better than just taking the latest assignment
  progress_summary <- data.table::data.table(
    completed_assignments = nrow(completed_assignments),
    total_assignments     = nrow(graded_assignments),
    expected_complete     = should_tryit,
    should_lesson         = should_lesson,
    at_lesson             = at_lesson,
    days_behind           = days_behind,
    course_type           = course_type
  )

  return(progress_summary)
}


tryDateTime <- function(x) {
  if (is.null(x) || is.na(x) || x == "") {
    return(NA_POSIXct_)
  } else {
    anytime::anytime(x, asUTC = TRUE)
  }
}

#' Extract Important Student Information from JSON List
#'
#' @param student A JSON list
#' @param nexus_cookies A data.table of cookies for Nexus' \code{sessionId} (and any others)
#'
#' @return A \code{\link[data.table]{data.table}}
#' @export
#' @import data.table
extractStudent <- function(student, nexus_cookies = NULL) {

  if (is.null(nexus_cookies)) {
    nexus_cookies <- extractNexusCookies()
  }

  futile.logger::flog.debug(paste0("Extracting record for ", student$name$full, "/", student$netId))

  id <- student[["_id"]]

  res <- getNexusRequest(route = paste0("students/", id), nexus_cookies = nexus_cookies)

  dates_and_section <- data.table::data.table(
      grades_last_updated = tryDateTime(res$mathable$gradesUpdatedDate)
    , last_mentor_email   = tryDateTime(res$mentor$lastResponseDate)
    , last_student_email  = tryDateTime(res$mentor$lastEmailDate)
    , section             = res$section
  )

  # determine if the student has had any sort of extension approved
  exts <- student[["extensions"]]
  if (length(exts)) {
    timelines <- sapply(student[["extensions"]], `[[`, "type")
    if (sum(timelines != "original") > 0L) {
      timeline <- "extension"
    } else {
      timeline <- "original"
    }
  } else {
    timeline <- "original"
  }

  # determine if the student has a proctor
  has_proctor <- ifelse(length(student[["proctor"]]) > 0L || "Onsite" %in% student$tags, TRUE, FALSE)

  # some Mathable course ids do not have this pre-pended which results in
  # no matches down the road
  mathable_course_id <- student[["mathable"]][["courseId"]]
  mathable_course_id <- ifelse(is.null(mathable_course_id), NA, mathable_course_id)

  if (!is.na(mathable_course_id) && !grepl("^studentcourserecords\\/", mathable_course_id)) {
    mathable_course_id <- paste0("studentcourserecords/", mathable_course_id)
  }

  # select some non-array fields
  cstudent_names <- names(student)
  ix <- which(cstudent_names %in% c("netId", "course", "status", "startDate",
                                    "endDate", "endDays", "startDays", "email", "isPartner"))

  # combine with some nested lists and convert to data.table
  student_profile <- data.table::as.data.table(
    c(
      `id` = id,
      student[ix],
      student[["name"]],
      orientation_date   = tryDateTime(student[["courseOrientation"]][["date"]]),
      mathable_course_id = mathable_course_id,
      timeline           = timeline,
      has_proctor        = has_proctor
    )
  ) %>% cbind(dates_and_section)

  rename_from <- c("netId", "startDate", "endDate", "endDays", "startDays")
  rename_to   <- c("student_netid", "start_date", "end_date", "end_days", "start_days")
  rename_ix   <- which(rename_from %in% names(student_profile))

  # rename for consistency
  data.table::setnames(student_profile
     , rename_from[rename_ix]
     , rename_to[rename_ix]
  )

  # set latest email/emailed and days since
  student_profile[, `:=`(
    days_last_mentor_email  = round(as.numeric(difftime(Sys.time(), last_mentor_email, units = "days")), 2),
    days_last_student_email = round(as.numeric(difftime(Sys.time(), last_student_email, units = "days")), 2)
  )]

  # set R date/time types
  if (!is.null(student_profile$start_date) & !is.null(student_profile$end_date)) {
    student_profile[, `:=`(
      start_date = tryDateTime(start_date),
      end_date   = tryDateTime(end_date)
    )]
  }

  return(student_profile)
}

#' Extract Important Assignment Information from JSON List
#'
#' @param assignment A JSON list
#'
#' @return A \code{\link[data.table]{data.table}}
#' @export
#' @import data.table
extractAssignment <- function(assignment) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # we'll need the lesson number, completion status and try it number for
  # each assignment to be able to calculate where they are in the schedule
  assignment_dt <- data.table::data.table(
    lesson_num   = assignment$lesson$number,
    tryit_status = assignment$status,
    tryit_name   = assignment$item$name
  )

  # indicator for completeness
  assignment_dt[, tryit_complete := tryit_status %in% c("Graded", "TryAgain", "HandedIn")]

  return(assignment_dt)
}


#' Extract the Latest Email From Student
#'
#' @param student_netid A student's NetId
#' @param nexus_cookies A data.table of cookies for Nexus' \code{sessionId} (and any others)
#'
#' @return A Date type object
#' @export
extractLatestEmailDate <- function(nexus_cookies = NULL, student_netid) {

  if (is.null(nexus_cookies)) {
    nexus_cookies <- extractNexusCookies()
  }

  page               <- 1
  look_outbound      <- TRUE
  latest_msg_date    <- NA
  latest_mentor_date <- NA
  while (TRUE) {

    # get all the conversation tickets on the first page
    conversations <- netmathtools2::getTicketList(student_netid, page = page, nexus_cookies = nexus_cookies)

    # if there are no conversations on this page then we've reached the end
    # and there are no tickets from the student
    if (conversations$count == 0L) {
      break
    }

    # grab the conversation ids
    convo_ids <- sapply(conversations[["results"]], `[[`, "id", USE.NAMES = FALSE)

    # request the tickets, which include all of the messages
    convo_msgs <- lapply(convo_ids, netmathtools2::getTicketMessages, nexus_cookies = nexus_cookies)

    # combine the messages, we only care about the date, not convo ownership
    all_msg <- unlist(lapply(convo_msgs, `[[`, "results"), recursive = FALSE)

    # filter the inbound messages, we want when a student has last emailed
    inbound_msg <- Filter(function(msg) msg$is_outgoing == 0, all_msg)

    # fileter the outbound messages to find when a student was last emailed
    outbound_msg <- Filter(function(msg) msg$is_outgoing == 1, all_msg)

    # look for outbound messages, but don't break -- we're
    if (length(outbound_msg) > 0 & look_outbound == TRUE) {
      latest_mentor_date <- extractLatestDate(outbound_msg)
      look_outbound <- FALSE
    }

    # if there are no inbound emails from the convos on this page, go to the next
    if (length(inbound_msg) == 0L) {
      page <- page + 1
      next
    }

    latest_msg_date <- extractLatestDate(inbound_msg)
    break
  }

  message_dates <- c("last_mentor_email"  = latest_mentor_date,
                     "last_student_email" = latest_msg_date)

  return(message_dates)
}

extractLatestDate <- function(messages) {
  # extract the create date as a raw char
  char_dates <- sapply(messages, function(msg) msg$headers$date)

  # convert to UTC
  msg_dates <- as.POSIXct(char_dates, tz = "UTC", format = "%a, %d %b %Y %T %z")

  # local time
  local_msg_dates <- as.POSIXlt(msg_dates, tz = "America/Chicago")

  # calculate the latest date and exit
  latest_msg_date <- as.Date(max(local_msg_dates))

  return(latest_msg_date)
}
