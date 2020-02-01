#' @title Get a Random User Agent
#'
#' @description This function randomly selects a user agent among a list of 81
#' obtained from \url{https://techblog.willshouse.com/2012/01/03/most-common-user-agents/}.
#' The new user agent is checked using the What's My UA API (\url{https://www.whatsmyua.info/api/v1/ua}).
#'
#' @param verbose A boolean. If TRUE, the new user agent is printed.
#'
#' @return The selected user agent.
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' change_ua()
#' }



change_ua <- function(verbose = TRUE) {

  agent <- as.character(sample(agents[ , "user_agent"], 1))

  if (verbose) {
    get_ua(agent)
  }

  return(agent)
}
