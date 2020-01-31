#' @title Check for Unix System and Super User
#'
#' @description This function checks 1) if the system is running under Unix
#' (GNU/Linux and macOS) and 2) if R/RStudio is running under super-user do (sudo).
#' This last one is required to control OpenVPN and change IP address.
#'
#' @return Nothing. An error is generated if the two prerequisites are not met.
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' is_sudo()



is_sudo <- function() {

  if (!(.Platform$OS.type == "unix")) {

    stop("Only Unix (GNU/Linux or macOS) systems are supported.")
  }

  if (system("echo $EUID", intern = TRUE) != 0) {

    stop("You must run R as a super user (sudo) to change your IP.")
  }
}
