library("data.table")
library("jsonlite")
library("curl")
library("dplyr")

py_cmd <- sprintf("python %s", system.file(package = "netmathtools2", "decrypt-chrome-cookies.py"))
system(py_cmd, intern = TRUE, wait = TRUE)

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = ".\\Cookies")

DBI::dbListTables(con)

cookie_tbl <- dplyr::tbl(con, "cookies")

cookies <- cookie_tbl %>%
  filter(host_key == "nexus.netmath.illinois.edu" & name == "sessionId") %>%
  collect()


DBI::dbConnect(con)

nexus_cookie_header <- paste0(with(cookies, paste0(name, "=", value)), collapse = ";")

h <- curl::new_handle()
curl::handle_setheaders(h,
  "DNT" = "1",
  "Accept-Encoding" = "gzip, deflate, br",
  "Accept-Language" = "en-US,en;q=0.8",
  "Accept" = "application/json, text/plain, */*",
  "User-Agent" = "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36",
  "Connection" = "keep-alive",
  "Cookie" = nexus_cookie_header
)

curl::handle_setopt(h, "ssl_verifypeer" = 0L)

net_id <- "mkemp6"
api_endpoint <- "https://nexus.netmath.illinois.edu/api"

req_url <- file.path(api_endpoint, "students?isPartner=false&mentor.netId=mkemp6")
res <- curl::curl_fetch_memory(req_url, handle = h)

content <- jsonlite::fromJSON(rawToChar(res$content), flatten = FALSE, simplifyDataFrame = FALSE)

data.table::rbindlist(lapply(content, extractJSONStudent))



extractJSONStudent <- function(student) {

  # determine if the student has had any sort of extension approved
  exts <- student[["extensions"]]
  if (length(exts)) {
    timelines <- sapply(student[["extensions"]], `[[`, "type")
    if (!"original" %in% timelines) {
      timeline <- "extension"
    } else {
      timeline <- "original"
    }
  } else {
    timeline <- "original"
  }

  # select some non-array fields
  cstudent_names <- names(student)
  ix <- which(cstudent_names %in% c("_id", "course", "status", "startDate",
                                    "endDate", "endDays", "startDays"))

  # combine with some nested lists and convert to data.table
  student_profile <- as.data.table(
    c(
      student[ix],
      student[["name"]],
      mathableCourseId = student[["mathable"]][["courseId"]],
      timeline = timeline
    )
  )

  # set R date/time types
  student_profile[]

  return(student_profile)
}

rbindlist(lapply(x, extractJSONStudent))

lapply(lapply(x, `[[`, "extensions"), function(x) sapply(x, `[[`, "type"))
