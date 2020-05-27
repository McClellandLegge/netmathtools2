

#' Extract Nexus Cookie(s) from FireFox
#'
#' @param profile_pattern A regex pattern to help find the correct firefox profile from which to use the \code{cookies.sqlite} DB
#'
#' @return A data.table
#' @export
extractNexusCookies <- function(profile_pattern = "\\.default-release$") {

  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("`curl` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("glue", quietly = TRUE)) {
    stop("`glue` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("`purrr` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`dplyr` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("`RSQLite` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("`DBI` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  `%>%` <- purrr::`%>%`

  # find the
  os <- Sys.info()['sysname']
  if (os == "Linux") {
    default_firefox_profile <- fs::path(Sys.getenv("HOME"), ".mozilla/firefox/") %>%
      fs::dir_ls() %>%
      purrr::keep(~grepl("default-default", .))

    firefox_cookies_org <- fs::path(default_firefox_profile, "cookies.sqlite")
    td <- tempdir()
    fs::file_copy(firefox_cookies_org, td)
    firefox_cookies_db <- fs::path(td, "cookies.sqlite")
  } else if (os == "Darwin") {
    rlang::stop("Firefox location of cookies not defined for Mac")
  } else if (ox == "Windows") {
    default_firefox_profile <- fs::path(Sys.getenv("APPDATA"), "Mozilla", "Firefox", "Profiles") %>%
      fs::dir_ls() %>%
      purrr::keep(~grepl(profile_pattern, .))

    firefox_cookies_db <- fs::path(default_firefox_profile, "cookies.sqlite")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), firefox_cookies_db)

  nexus_cookies <- dplyr::tbl(con, "moz_cookies") %>%
    dplyr::filter(host == "nexus.netmath.illinois.edu") %>%
    data.table::as.data.table()

  DBI::dbDisconnect(con)

  if (os == "Linux") {
    fs::file_delete(firefox_cookies_db)
  }

  if (nrow(nexus_cookies) == 0L) {
    rlang::abort("No cookies found for Nexus... are you logged in on Firefox?")
  }

  return(nexus_cookies)
}


#' Compose a Mathable Curl Handle
#'
#' @param netid The user netid
#'
#' @return A handle object (external pointer to the underlying curl handle).
#' @export
extractMathableCookies <- function(netid) {

  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("`curl` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # extract the mathable cookies
  h <- curl::new_handle()
  curl::handle_setheaders(handle = h, "content-type" = "application/json; charset=UTF-8")
  req_url <- glue::glue(
    "{api}/LogIn?LoginName=%22{user}%40netmath.illinois.edu%22&Password=%22{passwd}%22"
    , api    = mathable_endpoint
    , user   = getOption("netmathtools.mathable.user")
    , passwd = getOption("netmathtools.mathable.passwd")
  )

  # server sets cookies
  req     <- curl::curl_fetch_memory(req_url, handle = h)
  cookies <- curl::handle_cookies(h) %>% data.table::as.data.table() %>%
    return()
}
