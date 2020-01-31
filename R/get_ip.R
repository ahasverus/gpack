#' @title Get System Public IP Address
#'
#' @description This function retrieves the public IP address of the system using the ipify API (\url{https://api.ipify.org}).
#'
#' @return The IPv4 address (character of length 1)
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' get_ip()
#' }



get_ip <- function() {

  readLines(
    con      = "https://api.ipify.org/",
    warn     = FALSE,
    encoding = "UTF-8"
  )
}
