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
  is_egr        <- grepl("_egr_", course_id)

  # find the schedule
  if (isTRUE(is_highschool)) {
    schedule <- NULL
  } else if(isTRUE(is_egr)) {
    schedule <- netmathtools2:::schedules$MM461EGR
  } else {
    schedule <- netmathtools2:::schedules$MM461
  }

  # if days left is less than 0 then they probably have an extension pending
  # but we'll just act like the course is actually over, flooring at 0
  days_left <- ifelse(days_left < 0L, 0L, days_left)

  # see where the student should be based on the number of days they've been in
  # their course as of today
  if (!is.null(schedule)) {
    data.table::setkey(schedule, lesson_num, tryit_name)
    graded_schedule <- graded_assignments[schedule]

    course_days     <- max(schedule$day)
    should_schedule <- schedule[day <= course_days - days_left]

    should_day      <- should_schedule[, .(lesson_num, tryit_name)]
    should_lesson   <- max(should_day$lesson_num)
    should_tryit    <- nrow(unique(should_day))

    at_schedule     <- graded_schedule[tryit_complete == TRUE]
    at_day          <- at_schedule[, max(day, 0, na.rm = TRUE)]
    should_be_day   <- should_schedule[, max(day, 0, na.rm = TRUE)]
    days_behind     <- should_be_day - at_day

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
    days_behind           = days_behind
  )

  return(progress_summary)
}


#' Extract Important Student Information from JSON List
#'
#' @param student A JSON list
#' @param handle A \code{curl} \code{\link[curl]{handle}}
#'
#' @return A \code{\link[data.table]{data.table}}
#' @export
#' @import data.table
extractStudent <- function(student, handle) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  id <- student[["_id"]]

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

  # some Mathable course ids do not have this pre-pended which results in
  # no matches down the road
  mathable_course_id <- student[["mathable"]][["courseId"]]
  if (!grepl("^studentcourserecords\\/", mathable_course_id)) {
    mathable_course_id <- paste0("studentcourserecords/", mathable_course_id)
  }

  # select some non-array fields
  cstudent_names <- names(student)
  ix <- which(cstudent_names %in% c("course", "status", "startDate",
                                    "endDate", "endDays", "startDays"))

  # combine with some nested lists and convert to data.table
  student_profile <- data.table::as.data.table(
    c(
      `id` = id,
      student[ix],
      student[["name"]],
      mathableCourseId = mathable_course_id,
      timeline = timeline
    )
  )

  # set R date/time types
  # student_profile[]

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
  assignment_dt[, tryit_complete := tryit_status %in% c("Graded", "TryAgain")]

  return(assignment_dt)
}

