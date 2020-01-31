#' @title Get a Random User Agents
#'
#' @description This function randomly selects a user agent among a list of 81 obtained from \url{https://techblog.willshouse.com/2012/01/03/most-common-user-agents/} (Last Updated: Fri, 31 Jan 2020).
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



change_ua <- function() {

  as.character(sample(uagents[ , "user_agent"], 1))
}
