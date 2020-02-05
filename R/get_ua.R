#' @title Check How the User Agent is Detected
#'
#' @description This function checks how a user agent is detected by a server using
#' the What's My UA API (\url{https://www.whatsmyua.info/api/v1/ua}).
#'
#' @param agent A character of length 1 specifying a user agent.
#' @param verbose A boolean. If TRUE, the new user agent is printed.
#'
#' @import usethis
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
#'
#' ua <- change_ua("Mozilla/5.0 (Linux x86_64) Gecko/20100101 Firefox/72.0")
#' get_ua(agent = ua)
#' }



get_ua <- function(agent, verbose = TRUE) {

  if (!missing(agent)) {

    if (is.null(agent)) {

      usethis::ui_stop(
        stick(
          "
            Argument
            {usethis::ui_field('agent')}
            cannot be
            {usethis::ui_value('NULL')}
          "
        )
      )
    }

    if (length(agent) != 1) {

      usethis::ui_stop(
        stick(
          "
            Argument
            {usethis::ui_field('agent')}
            must be a
            {usethis::ui_value('character of length 1')}
          "
        )
      )
    }

    if (!is.character(agent)) {

      usethis::ui_stop(
        stick(
          "
            Argument
            {usethis::ui_field('agent')}
            must be a
            {usethis::ui_value('character of length 1')}
          "
        )
      )
    }

    ua_sent  <- httr::user_agent(agent)
    response <- httr::GET("https://www.whatsmyua.info/api/v1/ua", ua_sent)

  } else {

    response <- httr::GET("https://www.whatsmyua.info/api/v1/ua")
  }

  if (response$status_code != 200) {

    usethis::ui_stop(
      stick(
        "
          Unable to resolve host
          {usethis::ui_value('https://www.whatsmyua.info')}
        "
      )
    )
  }

  response <- httr::content(response, as = "text")

  ua_long  <- jsonlite::fromJSON(response)[1, "ua"]$"rawUa"
  ua_short <- jsonlite::fromJSON(response)[3, "device"]$"description"

  if (!missing(agent)) {

    if (agent != ua_long) {

      usethis::ui_stop(
        stick(
          "
            Unable to change
            {usethis::ui_value('user-agent')}
          "
        )
      )
    }
  }

  if (verbose) {

    usethis::ui_done(
      stick(
        "
          Your system is now detected as
          {usethis::ui_value(ua_short)}
        ",
        indent = " "
      )
    )
  }
}
