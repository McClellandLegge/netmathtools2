
#' Request Student's Ticket Threads
#'
#' @param student_netid Student's netId
#' @param nexus_cookies A data.table of cookies for Nexus' \code{sessionId} (and any others)
#' @param page Default \code{1}, the page of results to request
#'
#' @return A JSON list
#' @export
getTicketList <- function(student_netid, nexus_cookies = NULL, page = 1) {
  conversations <- getNexusRequest(
    nexus_cookies = nexus_cookies,
    route         = "cerb/tickets",
    page          = page,
    status        = "All",
    student_netId = student_netid
  )

  return(conversations)
}

#' Request a Specific Student Ticket
#'
#' @param ticket_id The unique ticket_id of the
#' @param nexus_cookies A data.table of cookies for Nexus' \code{sessionId} (and any others)
#'
#' @return A JSON list
#' @export
getTicketMessages <- function(ticket_id, nexus_cookies = NULL) {
  ticket <- getNexusRequest(
    route  = file.path("cerb/tickets", ticket_id, "messages"),
    nexus_cookies = nexus_cookies
  )

  return(ticket)
}

#' Get Students' Progress
#'
#' @param students A \code{\link[data.table]{data.table}} returned by \link{getStudents}
#' @param nexus_cookies A data.table of cookies for Nexus' \code{sessionId} (and any others)
#'
#' @return A \code{\link[data.table]{data.table}} with the same columns and some
#'     progress metrics appended
#' @export
#' @import data.table
getStudentsProgress <- function(students, nexus_cookies = NULL) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  students_dt <- data.table::as.data.table(students)

  if (is.null(nexus_cookies)) {
    nexus_cookies <- extractNexusCookies()
  }

  refresh_start <- Sys.time()
  purrr::pwalk(students_dt, function(id, student_netid, ...) {
    futile.logger::flog.info(paste0("Re-syncing grades for ", student_netid))
    netmathtools2::putNexus(route = paste0("students/", id, "/grades"), nexus_cookies = nexus_cookies)
  })

  refreshes <- students_dt$grades_last_updated
  refreshes[is.na(refreshes)] <- as.POSIXct("2019-12-31")

  refreshes <- as.list(refreshes)
  while (any(purrr::map_lgl(refreshes, ~difftime(refresh_start, ., units = "hours") > 0))) {
    flog.debug("Waiting for refreshes to complete")
    Sys.sleep(10L)

    refreshes <- purrr::pmap(students_dt, function(id, ...) {
      res <- getNexusRequest(route = paste0("students/", id), nexus_cookies = nexus_cookies)
      tryDateTime(res$mathable$gradesUpdatedDate)
    }) %>% unlist() %>% as.POSIXct(origin = "1970-01-01")

    refreshes[is.na(refreshes)] <- as.POSIXct("2019-12-31")
  }

  # get all the grades and then limit to only the homeworks
  all_grades    <- sapply(students$id, netmathtools2::getGrades, nexus_cookies = nexus_cookies, simplify = FALSE)
  all_notebooks <- sapply(all_grades, `[[`, "homeworks", simplify = FALSE)
  all_exams     <- sapply(all_grades, function(student_rec) {

    # make a list of all the exams, including the final
    exams <- c(student_rec$exams, list(student_rec$finalExam))

    # if the list is all nulls, this indicates its probably an EGR student
    # which doesn't have that field initialized, we fake it
    if (length(Filter(Negate(is.null), exams)) == 0L) {
      exams[[1]] <- list(name = "Final Exam")
    }

    # examine each record and attach either the score or an indicator that
    # it hasn't been taken
    exam_records <- sapply(exams, function(exam_rec) {
      score <- ifelse(exam_rec$status == "Entered", "Submitted", ifelse(is.null(exam_rec$score), "Not Taken", exam_rec$score))
      exam  <- paste0(exam_rec$name, ": ", score)
      return(exam)
    }, USE.NAMES = FALSE)

    # collapse with a break tag so that we can  use a single column for all exams
    # but they will all display on one line in the browser, wrap in a span that
    # disables line breaks
    break_exam <- paste0(exam_records, collapse = "<br>")
    html_exam  <- sprintf("<span style='white-space:nowrap'>%s<span>", break_exam)

    # convert to a data table for easier stacking in the next step
    html_exam_recs <- data.table::data.table(exams = html_exam)

    return(html_exam_recs)
  }, simplify = FALSE)

  # combine the exams into a data.table for merge later downstream
  exams_dt <- data.table::rbindlist(all_exams, idcol = "id")

  # filter out only homeworks (throw out "Reading") and limit to the current course
  progress_ls <- list()
  student_ids <- names(all_notebooks)
  if (all(sapply(all_notebooks, length) == 0)) {

  } else {
    for (k in seq_along(all_notebooks)) {
      student_id <- student_ids[k]
      student    <- students_dt[student_id == id]
      notebooks  <- all_notebooks[[student_id]]

      if (length(notebooks) == 0) {
        next
      }

      progress_ls[[student_id]] <- netmathtools2::extractStudentProgress(
        notebooks      = notebooks,
        course_id      = student$mathable_course_id,
        days_left      = student$end_days
      )
    }
  }

  # merge in the exams
  student_exam_dt <- merge(students_dt, exams_dt, by = "id", all.x = TRUE)

  # merge in the student progress
  progress <- data.table::rbindlist(progress_ls, idcol = "id")
  if (nrow(progress) > 0L) {
    student_progress <- merge(progress, student_exam_dt, by = "id", all.y = TRUE)
  } else {
    student_progress <- student_exam_dt
  }

  # calculate the comparisons to where they should be
  student_progress[!is.na(mathable_course_id), `:=`(
    tryits_behind  = expected_complete - completed_assignments,
    lessons_behind = should_lesson - at_lesson,
    current_pace   = completed_assignments / start_days
  )]

  # produce a plain-english interpretation of what their current pace is
  student_progress[!is.na(mathable_course_id), current_pace_interp := sapply(current_pace, currentPaceCompose)]

  # calculate what their needed pace to finish on time is
  student_progress[end_days > 0L & !is.na(mathable_course_id), `:=`(
    needed_pace = (total_assignments - completed_assignments) / end_days
  )]

  # produce a plain-english interpretation of what their needed pace is
  student_progress[end_days > 0L & !is.na(mathable_course_id), `:=`(
    needed_pace_interp = needPaceCompose(needed_pace)
  )]

  return(student_progress)
}

currentPaceCompose <- function(current_pace, interval = 0.5) {
  cp_long <- round(1 / current_pace / interval) * interval
  cp_short <- round(current_pace / interval) * interval
  short_plural <- sapply(cp_short, function(x) ifelse(x != 1, "s", ""), USE.NAMES = FALSE)
  pace_interp <- sapply(current_pace, function(x) {
    if (is.null(x) || is.nan(x) || is.na(x)) {
      msg <- "Not Applicable"
    } else if (round(x, 4) == 0) {
      msg <- "have not submitted any Try Its"
    } else if (x < 1) {
      msg <- paste("have been submitting a Try It every", cp_long, "days")
    } else {
      msg <- paste0("have been submitting ", cp_short, " Try It", short_plural, " a day")
    }
    return(msg)
  })
  return(pace_interp)
}

needPaceCompose <- function(needed_pace, interval = 0.5) {
  np <- ceiling(needed_pace / interval) * interval
  np_plural <- sapply(np, function(x) ifelse(x != 1, "s", ""), USE.NAMES = FALSE)

  msg <- mapply(function(np, np_plural) {
    if (is.null(np) || is.nan(np) || is.na(np)) {
      msg <- "Not Applicable"
    } else {
      msg <- paste0("need to submit an average of ", np, " Try It", np_plural, " a day")
    }

    return(msg)
  }, np, np_plural)

  return(msg)
}

#' Get a Student's Grades
#'
#' @param student_id A character string
#' @param nexus_cookies A data.table of cookies for Nexus' \code{sessionId} (and any others)
#'
#' @return A JSON list
#' @export
getGrades <- function(student_id, nexus_cookies = NULL) {

  if (is.null(nexus_cookies)) {
    nexus_cookies <- extractNexusCookies()
  }

  futile.logger::flog.debug(paste0("Getting grades for ", student_id))

  req <- file.path("students", student_id, "grades")
  res <- netmathtools2::getNexusRequest(route = req, nexus_cookies = nexus_cookies)

  return(res)
}

#' Get a Mentor's Students
#'
#' @param net_id A character string
#' @param nexus_cookies A data.table of cookies for Nexus' \code{sessionId} (and any others)
#'
#' @return A \code{\link[data.table]{data.table}}
#' @export
#' @import data.table
getStudents <- function(net_id, nexus_cookies = NULL) {

  if (is.null(net_id) || !inherits(net_id, "character")) {
    rlang::abort("'net_id' must be a character string!")
  }

  if (is.null(nexus_cookies)) {
    nexus_cookies <- extractNexusCookies()
  }

  futile.logger::flog.debug("Getting student list from Nexus...")
  students_ls <- getNexusRequest(
    route         = "students",
    nexus_cookies = nexus_cookies,
    mentor.netId  = net_id
  )

  # use a custom extractor to pull out specific information from the list
  futile.logger::flog.debug("Extracting relevant information from each student's record")
  students_detail_ls <- purrr::map(students_ls, extractStudent, nexus_cookies = nexus_cookies)
  students_detail    <- data.table::rbindlist(students_detail_ls, fill = TRUE)

  futile.logger::flog.debug("Converting character dates to datetime class")
  students_detail[, orientation_date := anytime::anytime(orientation_date, asUTC = TRUE)]

  return(students_detail)
}


#' Execute a request to Nexus
#'
#' @param route What route? E.g. \code{students}, as a character string
#' @param nexus_cookies A data.table of cookies for Nexus' \code{sessionId} (and any others)
#' @param ... Additional arguments to be passed in the url
#'
#' @return
#' @export
getNexusRequest <- function(route, nexus_cookies = NULL, ...) {

  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("`curl` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("`jsonlite` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("`httr` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # compose the request url, get the arguments and convert to named list
  arg_ls  <- list(...)
  args    <- unlist(arg_ls)
  if (missing(...)) {
    arg_str <- ""
  } else {
    arg_str <- paste0(paste0(names(args), "=", purrr::map_chr(args, URLencode, reserved = TRUE)), collapse = "&")
  }

  if (is.null(nexus_cookies)) {
    nexus_cookies <- extractNexusCookies()
  }

  req_url <- sprintf("%s/%s?%s", api_endpoint, route, arg_str)

  cookie_header <- with(nexus_cookies, {
    paste0(paste(name, value, sep = "="), collapse = "; ")
  })

  res <- httr::GET(url = req_url, httr::add_headers(Cookie = cookie_header))

  # check the status code
  if (res$status_code != 200) {
    rlang::abort(paste0(req_url, " failed with ", res$status_code))
  }

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

#' Execute a put to Nexus
#'
#' @param route What route? E.g. \code{students}, as a character string
#' @param nexus_cookies A data.table of cookies for Nexus' \code{sessionId} (and any others)
#' @param ... Additional arguments to be passed in the url
#'
#' @return
#' @export
putNexus <- function(route, nexus_cookies = NULL, ...) {

  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("`curl` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("`jsonlite` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("`httr` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # compose the request url, get the arguments and convert to named list
  arg_ls  <- list(...)
  args    <- unlist(arg_ls)
  if (missing(...)) {
    arg_str <- ""
  } else {
    arg_str <- paste0(paste0(names(args), "=", purrr::map_chr(args, URLencode, reserved = TRUE)), collapse = "&")
  }

  if (is.null(nexus_cookies)) {
    nexus_cookies <- extractNexusCookies()
  }

  req_url <- sprintf("%s/%s?%s", api_endpoint, route, arg_str)

  cookie_header <- with(nexus_cookies, {
    paste0(paste(name, value, sep = "="), collapse = "; ")
  })

  res <- httr::PUT(url = req_url, httr::add_headers(Cookie = cookie_header))

  # check the status code
  if (res$status_code != 200) {
    rlang::abort(paste0(req_url, " failed with ", res$status_code))
  }

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


#' Execute a GET request
#'
#' @param handle An active session established with \link{composeNexusHandle}
#' @param route A character string
#' @param ... Additional arguments to be passed in the url
#'
#' @return A JSON list
#' @export
getMathable <- function(netid, mathable_cookies = NULL, route, where = "nexus", ...) {

  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("`curl` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("`jsonlite` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("`httr` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # compose the request url, get the arguments and convert to named list
  args    <- unlist(list(...))
  if (missing(...)) {
    arg_str <- ""
  } else {
    arg_str <- paste0(paste0(names(args), "=", purrr::map_chr(args, URLencode, reserved = TRUE)), collapse = "&")
  }

  if (is.null(mathable_cookies)) {
    mathable_cookies <- extractMathableCookies(netid)
  }

  req_url <- sprintf("%s/%s?%s", netmathtools2:::mathable_endpoint, route, arg_str)

  cookie_vals        <- mathable_cookies$value
  names(cookie_vals) <- mathable_cookies$name

  heads <-  c("dnt"              = "1"
  , "accept-encoding"  = "gzip, deflate, br"
  , "accept-language"  = "en-US,en;q=0.9"
  , "accept"           = "*/*"
  , "user-agent"       = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.102 Safari/537.36"
  , "connection"       = "keep-alive"
  , "content-type"     = "application/json; charset=UTF-8"
  , "referer"          = mathable_url
  , "authority"        = mathable_hname
  , "x-requested-with" = "XMLHttpRequest")

  # perform the request
  res <- httr::GET(url = req_url, httr::add_headers(heads), httr::set_cookies(.cookies = cookie_vals))

  # # check the status code
  # if (res$status_code != 200) {
  #   rlang::abort(paste0(req_url, " failed with ", res$status_code))
  # }

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


#' Get the specifics of a notebook
#'
#' @param handle Valid Mathable curl handle
#' @param student_netid Student's Id
#' @param notebook_id Mathable Notebook Id
#'
#' @return
#' @export
getMathableNotebook <- function(mathable_cookies = NULL, netid, student_netid, notebook_id) {

  if (!is.null(mathable_cookies)) {
    mathable_cookies <- extractMathableCookies(netid)
  }

  prefix <- ifelse(grepl("_HS_", notebook_id, ignore.case = TRUE), "NetmathPHS", "UIUC")

  if (student_netid %in% c("rant2", "pdylag2", "pranava5")) {
    prefix <- "UIUC"
  }

  if (student_netid %in% c("cfahmad2")) {
    prefix <- "Mathable"
  }

  possibles <- c("UIUC", "NetmathPHS", "Mathable")

  student_prefixed <- paste0('"Users/', prefix, '_', student_netid, '"')
  res <- netmathtools2::getMathable(
      mathable_cookies   = mathable_cookies
    , where    = "mathable"
    , route    = "GetGradebookNotebook"
    , notebook = paste0('"', notebook_id, '"')
    , student  = student_prefixed
  )

  if (is.null(res$d$Results)) {
    warning(sprintf("Notebook %s for %s doesn't exist", notebook_id, student_netid))
    warning(paste0("Check student prefix: ", student_prefixed))
    return(NULL)
  }

  if (length(res$d$Results) == 0) {
    return(NULL)
  }

  return(res$d$Results[[1]])
}


#' Get Mathable Course Information
#'
#' @param netid Mentor NetID
#' @param student_courseid Student's Mathable Course ID
#'
#' @return
#' @export
getMathableCourse <- function(netid, student_courseid) {

  if (!is.null(mathable_cookies)) {
    mathable_cookies <- extractMathableCookies(netid)
  }

  res <- netmathtools2::getMathable(
      netid   = netid
    , mathable_cookies   = mathable_cookies
    , where    = "mathable"
    , route    = "GetCourse"
    , courseId = paste0('"', student_courseid, '"')
  )

  return(res)
}


getAllMathable <- function(netid, students) {
}

