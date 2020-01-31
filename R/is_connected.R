#' @title Check for Internet Connexion
#'
#' @description This function checks if the sustem is connected to Internet.
#' R/RStudio must be running under super-user do (sudo).
#'
#' @return Nothing. An error is generated if your system is disconnected from Internet.
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' is_connected()



is_connected <- function() {

  is_unix()
  is_sudo()

  interfaces <- system("ifconfig", intern = TRUE)
  pattern    <- paste0(
    "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}",
     "(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  )
  matches    <- grep(pattern, interfaces)

  if (length(matches) == 1) {

    stop("No internet connexion.")
  }
}
