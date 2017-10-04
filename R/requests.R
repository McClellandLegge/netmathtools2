
#' Request Student's Ticket Threads
#'
#' @param handle An active session established with \link{composeNexusHandle}
#' @param netId Student's netId
#' @param page Default \code{1}, the page of results to request
#'
#' @return A JSON list
#' @export
getTicketList <- function(handle, student_netid, page = 1) {
  conversations <- netmathtools2::getRequest(
    handle        = h,
    route         = "cerb/tickets",
    page          = page,
    status        = "All",
    student_netId = student_netid
  )

  return(conversations)
}

#' Request a Specific Student Ticket
#'
#' @param handle An active session established with \link{composeNexusHandle}
#' @param ticket_id The unique ticket_id of the
#'
#' @return A JSON list
#' @export
getTicketMessages <- function(handle, ticket_id) {
  ticket <- netmathtools2::getRequest(
    handle = h,
    route  = file.path("cerb/tickets", ticket_id, "messages")
  )

  return(ticket)
}

#' Get Students' Progress
#'
#' @param handle An active session established with \link{composeNexusHandle}
#' @param students A \code{\link[data.table]{data.table}} returned by \link{getStudents}
#'
#' @return A \code{\link[data.table]{data.table}} with the same columns and some
#'     progress metrics appended
#' @export
#' @import data.table
getStudentsProgress <- function(handle, students) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  students_dt <- data.table::as.data.table(students)

  # get all the grades and then limit to only the homeworks
  all_grades    <- sapply(students$id, netmathtools2::getGrades, handle = handle, simplify = FALSE)
  all_notebooks <- sapply(all_grades, `[[`, "homeworks", simplify = FALSE)
  rm(all_grades)

  # filter out only homeworks (throw out "Reading") and limit to the current course
  progress_ls <- list()
  student_ids <- names(all_notebooks)
  for (k in seq_along(all_notebooks)) {
    student_id <- student_ids[k]
    student    <- students_dt[student_id == id]
    progress_ls[[student_id]] <- netmathtools2::extractStudentProgress(
      notebooks      = all_notebooks[[student_id]],
      course_id      = student$mathable_course_id,
      days_left      = student$end_days
    )
  }

  progress <- data.table::rbindlist(progress_ls, idcol = "id")
  student_progress <- merge(progress, students_dt, by = "id", all.y = TRUE)

  student_progress[, `:=`(
    tryits_behind  = expected_complete - completed_assignments,
    lessons_behind = should_lesson - at_lesson,
    current_pace   = completed_assignments / start_days
  )]

  student_progress[, current_pace_interp := sapply(current_pace, currentPaceCompose)]

  student_progress[end_days > 0L, `:=`(
    needed_pace = (total_assignments - completed_assignments) / end_days
  )]

  student_progress[end_days > 0L, `:=`(
    needed_pace_interp = needPaceCompose(needed_pace)
  )]

  return(student_progress)
}

currentPaceCompose <- function(current_pace, interval = 0.5) {
  cp_long <- round(1 / current_pace / interval) * interval
  cp_short <- round(current_pace / interval) * interval
  pace_interp <- sapply(current_pace, function(x) {
    if (round(x, 4) == 0) {
      msg <- "have not submitted any Try Its"
    } else if (x < 1) {
      msg <- paste("have been submitting a Try It every", cp_long, "days")
    } else {
      msg <- paste("have been submitting", cp_short, "Try It(s) a day")
    }
    return(msg)
  })
  return(pace_interp)
}

needPaceCompose <- function(needed_pace, interval = 0.5) {
  np <- ceiling(needed_pace / interval) * interval
  msg <- paste("need to submit an average of", np, "Try It(s) a day")
  return(msg)
}

#' Get a Student's Grades
#'
#' @param handle An active session established with \link{composeNexusHandle}
#' @param student_id A character string
#'
#' @return A JSON list
#' @export
getGrades <- function(handle, student_id) {

  req <- file.path("students", student_id, "grades")
  res <- netmathtools2::getRequest(handle, req)

  return(res)
}

#' Get a Mentor's Students
#'
#' @param handle An active session established with \link{composeNexusHandle}
#' @param net_id A character string
#'
#' @return A \code{\link[data.table]{data.table}}
#' @export
#' @import data.table
getStudents <- function(handle, net_id) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  students_ls <- netmathtools2::getRequest(
    handle       = handle,
    route        = "students",
    isPartner    = "false",
    mentor.netId = net_id
  )

  # use a custom extractor to pull out specific information from the list
  students_detail_ls <- lapply(students_ls, netmathtools2::extractStudent, handle = handle)
  students_detail    <- data.table::rbindlist(students_detail_ls, fill = TRUE)

  orientation_dates_utc <- as.POSIXct(
    x      = students_detail$orientation_date,
    tz     = "UTC",
    format = "%Y-%m-%dT%H:%M:%OSZ"
  )

  students_detail$orientation_date <- as.Date(as.POSIXlt(
    x  = orientation_dates_utc,
    tz = "America/Chicago"
  ))

  return(students_detail)
}

#' Execute a GET request
#'
#' @param handle An active session established with \link{composeNexusHandle}
#' @param route A character string
#' @param ... Additional arguments to be passed in the url
#'
#' @return A JSON list
#' @export
getRequest <- function(handle, route, ...) {

  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("`curl` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("`jsonlite` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # compose the request url, get the arguments and convert to named list
  args    <- unlist(list(...))
  if (missing(...)) {
    arg_str <- ""
  } else {
    arg_str <- paste0(paste0(names(args), "=", args), collapse = "&")
  }
  req_url <- sprintf("%s/%s?%s", netmathtools2:::api_endpoint, route, arg_str)

  # perform the request
  res <- curl::curl_fetch_memory(req_url, handle = handle)

  # check the status code


  # extract the content
  # don't try to flatten the list, but convert arrays to atomic vectors
  content <- jsonlite::fromJSON(
    txt               = rawToChar(res$content),
    flatten           = FALSE,
    simplifyDataFrame = FALSE,
    simplifyVector    = TRUE
    )

  return(content)
}

