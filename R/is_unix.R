#' @title Check for Unix System
#'
#' @description This function checks if the system is running under Unix
#' (GNU/Linux and macOS). The package is not develop for others platforms.
#'
#' @return Nothing. An error is generated if this prerequisite is not met.
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' is_unix()



is_unix <- function() {

  if (!(.Platform$OS.type == "unix")) {

    stop("Only Unix (GNU/Linux or macOS) systems are supported.")
  }
}
