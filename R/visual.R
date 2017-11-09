#' Chart the Student's Progress
#'
#' @param students A data.table, the output of \link{getStudentsProgress}
#' @importFrom grDevices colorRampPalette
#' @export
chartProgress <- function(students) {

  if (!requireNamespace("tools", quietly = TRUE)) {
    stop("`tools` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("`shiny` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("`DT` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # prettify the names of the table
  spaced_names <- gsub("_", " ", names(students))
  pretty_names <- sapply(spaced_names, tools::toTitleCase, USE.NAMES = FALSE)

  # establish the breaks and the corresponding color assignments for the breaks
  status_breaks  <- c(0, 3, 7, 14, 30, 60)
  makeStatusCols <- grDevices::colorRampPalette(c("green", "yellow", "orange", "red"))
  status_colors  <- makeStatusCols(length(status_breaks) + 1)

  email_breaks  <- c(8, 13)
  makeEmailCols <- colorRampPalette(c("white", "purple"))
  email_colors  <- makeEmailCols(length(email_breaks) + 1)

  # the UI is just the output of the datatable
  ui <- shiny::fluidPage(DT::dataTableOutput("students"))

  # determine what the indicies are of the sort columns, subtract 1
  # because DT is zero-indexed
  ix_end_days    <- which(names(students) == "end_days") - 1L
  ix_days_behind <- which(names(students) == "days_behind") - 1L

  # the server just contains the datatable with a bunch of options
  server <- function(input, output) {
    output$students <- DT::renderDataTable({
      dt <- DT::datatable(students
                , rownames   = FALSE
                , escape     = FALSE
                , colnames   = pretty_names
                , extensions = c('FixedHeader', 'FixedColumns')
                , options = list(
                  pageLength = 50
                  , lengthMenu = c(50, 100)
                  , dom = 't'
                  , scrollX = TRUE
                  , order = list(list(ix_end_days, 'asc'), list(ix_days_behind, 'desc'))
                  , fixedColumns = list(leftColumns = 2)
                  , fixedHeader = TRUE
                ))
        dt <- DT::formatStyle(dt,
          'end_days',
          background = DT::styleColorBar(c(0, 112), 'steelblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
        dt <- DT::formatStyle(dt,
          'days_behind',
          backgroundColor = DT::styleInterval(
            status_breaks,
            status_colors
          )
        )
        dt <- DT::formatStyle(dt,
          'days_last_mentor_email',
          backgroundColor = DT::styleInterval(
            email_breaks,
            email_colors
          )
        )
        dt <- DT::formatStyle(dt,
          'days_last_student_email',
          backgroundColor = DT::styleInterval(
            email_breaks,
            email_colors
          )
        )
        return(dt)
    })
  }

  # run the app
  shiny::shinyApp(ui, server)
}
