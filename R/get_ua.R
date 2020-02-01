#' @title Check How the User Agent is Detected
#'
#' @description This function checks how a user agent is detected by a server using
#' the What's My UA API (\url{https://www.whatsmyua.info/api/v1/ua}).
#'
#' @param agent A character of length 1 specifying a user agent.
#' @param verbose A boolean. If TRUE, the new user agent is printed.
#'
#' @import httr
#' @import jsonlite
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' get_ua()
#' 
#' ua <- change_ua()
#' get_ua(agent = ua)
#' }



get_ua <- function(agent, verbose = TRUE) {

  if (!missing(agent)) {

    if (is.null(agent)) {
      stop("Argument 'agent' cannot be NULL.")
    }

    if (length(agent) != 1) {
      stop("Argument 'agent' must be a character of length 1.")
    }

    if (!is.character(agent)) {
      stop("Argument 'agent' must be a character of length 1.")
    }

    ua_sent  <- httr::user_agent(agent)
    response <- httr::GET("https://www.whatsmyua.info/api/v1/ua", ua_sent)

  } else {

    response <- httr::GET("https://www.whatsmyua.info/api/v1/ua")
  }

  if (response$status_code != 200) {
    stop("Unable to resolve host: https://www.whatsmyua.info.")
  }

  response <- httr::content(response, as = "text")

  ua_long  <- jsonlite::fromJSON(response)[1, "ua"]$"rawUa"
  ua_short <- jsonlite::fromJSON(response)[3, "device"]$"description"

  if (!missing(agent)) {
    if (agent != ua_long) {
      stop("Unable to change user-agent.")
    }
  }

  if (verbose) {
    cat("Your system is now detected as:", ua_short, "\n")
  }
}
