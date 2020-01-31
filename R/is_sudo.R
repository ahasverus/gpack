#' @title Check for Super User (sudo)
#'
#' @description This function checks if R/RStudio is running under super-user do (sudo).
#' This is required to control OpenVPN and change IP address.
#'
#' @return Nothing. An error is generated if this prerequisite is not met.
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' is_sudo()
#' }



is_sudo <- function() {

  is_unix()

  if (system("echo $EUID", intern = TRUE) != 0) {

    stop("You must run R as a super user (sudo) to change your IP.")
  }
}
