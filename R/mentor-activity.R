#' Title
#'
#' @param handle
#' @param students
#'
#' @return
#' @export
#' @imports purrr
#'
getMentorActivity <- function(handle, students) {

  tickets <- list()

  for (student_id in students[['student_netid']]) {
    page <- 1L
    tickets[[paste0(student_id)]] <- list()
    while (TRUE) {

      # get all the conversation tickets on the first page
      conversations <- netmathtools2::getTicketList(handle, student_id, page = page)

      # if there are no conversations on this page then we've reached the end
      # and there are no tickets from the student
      if (conversations$count == 0L) {
        break
      }

      # grab the conversation ids
      convo_ids <- sapply(conversations[["results"]], `[[`, "id", USE.NAMES = FALSE)

      # request the tickets, which include all of the messages
      convo_msgs <- lapply(convo_ids, netmathtools2::getTicketMessages, handle = handle)

      # combine the messages, we only care about the date, not convo ownership
      all_msg <- unlist(lapply(convo_msgs, `[[`, "results"), recursive = FALSE)

      # filter the inbound messages, we want when a student has last emailed
      outbound_msg <- Filter(function(msg) msg$is_outgoing == 1, all_msg)

      dates <- map(outbound_msg, 'created') %>%
        map(anytime::anytime) %>%
        do.call(c, .)

      tickets[[paste0(student_id)]] <- c(dates, tickets[[paste0(student_id)]])

      page <- page + 1L
    }
  }

  return(tickets)

}



#' Get the details about a Mathable assignment
#'
#' @param netid
#' @param course_id
#'
#' @return
#' @export
getAsssignmentDetail <- function(netid, course_id) {

  `%>%` <- purrr::`%>%`

  if (is.na(course_id)) {
    return(NULL)
  }

  # strip out the deployed course ID and student netID
  info     <- strsplit(course_id, "_(?=[^_]+$)", perl = TRUE)
  deployed <- info[[1]][[1]] %>% gsub("studentcourserecords", "deployedcourses", .)
  s_netid  <- info[[1]][[2]]

  # handle for this student's processing
  h <- composeMathableHandle(netid)

  res <- netmathtools2::getRequest(
      handle   = h
    , where    = "mathable"
    , route    = "GetGradebook"
    , courseId = paste0('"', deployed, '"')
  )

  nbs <- res$d$fields %>%
    grep("GiveItaTry|Literacy", ., value = TRUE)

  graded_assignments <- purrr::map(nbs, getMathableNotebook, handle = h, student_netid = s_netid) %>%
    purrr::discard(is.null) %>%
    purrr::map(function(x) {
      attempt  <- x[['Attempt']]
      graded   <- x[['DateGraded']]
      submit   <- x[['SubmitDate']]
      unit     <- x[['UnitNumber']]
      tryit    <- x[['NotebookTitle']]

      if (is.null(attempt)) {
        return(NULL)
      }

      data.table::data.table(
          attempt  = attempt
        , graded   = graded
        , submit   = submit
        , unit     = unit
        , tryit    = tryit
      )
    }) %>%
    purrr::discard(is.null) %>%
    data.table::rbindlist(fill = TRUE)

  if (nrow(graded_assignments) > 0) {
    graded_assignments[, `:=`(
      graded  = as.POSIXct(graded, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS")
      , submit  = as.POSIXct(submit, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS")
      , student = s_netid
    )]

    return(graded_assignments)
  } else {
    return(NULL)
  }
}




