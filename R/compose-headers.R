#' Decrypt the Chrome Cookies for Nexus
#'
#' @param what What cookies do decrpyt
#'
#' @return a \code{\link[data.table]{data.table}} with the cookie names and values
#'     along with other meta information.
#' @importFrom dplyr %>%
decrpytChromeCookies <- function(what) {

  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("`DBI` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("`RSQLite` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("`dplyr` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("`data.table` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # find and execute the python (3.6) script that copies the Chrome cookie
  # SQLite3 database, decrpyts the cookie values
  py_script <- system.file(package = "netmathtools2", "decrypt-chrome-cookies.py")
  py_cmd <- sprintf("python %s", py_script)
  system(py_cmd, intern = TRUE, wait = TRUE)

  # connect to the altered database
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ".\\Cookies")

  # extract the cookie table
  if (what == "nexus") {
    cookies <- dplyr::tbl(con, "cookies") %>%
      dplyr::filter(host_key == "nexus.netmath.illinois.edu" & name == "sessionId") %>%
      dplyr::collect()
  } else if (what == "mathable") {
    cookies <- dplyr::tbl(con, "cookies") %>%
      dplyr::filter(host_key == mathable_hname) %>%
      dplyr::collect()
  }


  # delete the intermediate sqlite db
  unlink(".\\Cookies")

  # close the connection
  DBI::dbDisconnect(con)

  # if there are no rows then there are no active/cached cookies
  if (nrow(cookies) == 0L) {
    stop("No Nexus cookies detected, are you signed in?")
  }

  return(cookies)
}

#' Compose a Nexus Curl Handle
#'
#' @param netid The mentor netid
#'
#' @return A handle object (external pointer to the underlying curl handle).
#' @export
#' @examples
#' \dontrun{
#' h <- composeNexusHandle("mkemp6")
#' }
composeNexusHandle <- function(netid) {

  if (!requireNamespace("curl", quietly = TRUE)) {
    stop("`curl` needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # extract the nexus cookies
  cookies <- decrpytChromeCookies("nexus")

  # compose the key value pairs for the nexus request header
  nexus_cookies    <- with(cookies, paste0(name, "=", value))
  nexus_cookie_str <- paste0(nexus_cookies, collapse = ";")

  # set a new handle
  h <- curl::new_handle()

  # compose the handle
  curl::handle_setheaders(h,
    "DNT"             = "1",
    "Accept-Encoding" = "gzip, deflate, br",
    "Accept-Language" = "en-US,en;q=0.8",
    "Accept"          = "application/json, text/plain, */*",
    "User-Agent"      = "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36",
    "Connection"      = "keep-alive",
    "Cookie"          = nexus_cookie_str
  )

  # don't verify SSL certs
  # TODO: figure out why this is necessary and fix if possible
  curl::handle_setopt(h, "ssl_verifypeer" = 0L)

  return(h)
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
  cookies <- decrpytChromeCookies("mathable")

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
    , "cookie"           = mathable_cookie_str
  )

  # don't verify SSL certs
  # TODO: figure out why this is necessary and fix if possible
  curl::handle_setopt(h, "ssl_verifypeer" = 0L)

  return(h)
}
