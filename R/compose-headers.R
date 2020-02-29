

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
  default_firefox_profile <- fs::path(Sys.getenv("APPDATA"), "Mozilla", "Firefox", "Profiles") %>%
    fs::dir_ls() %>%
    purrr::keep(~grepl(profile_pattern, .))

  firefox_cookies_db <- fs::path(default_firefox_profile, "cookies.sqlite")

  con <- DBI::dbConnect(RSQLite::SQLite(), firefox_cookies_db)
  on.exit(expr = DBI::dbDisconnect(con), add = TRUE, after = TRUE)

  nexus_cookies <- dplyr::tbl(con, "moz_cookies") %>%
    dplyr::filter(host == "nexus.netmath.illinois.edu") %>%
    data.table::as.data.table()

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
composeMathableHandle <- function(netid) {

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
  cookies <- curl::handle_cookies(h) %>% data.table::as.data.table()

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
}
