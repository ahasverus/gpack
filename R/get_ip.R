get_ip <- function() {

  readLines(
    con      = "https://api.ipify.org/",
    warn     = FALSE,
    encoding = "UTF-8"
  )
}
