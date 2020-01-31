#' @title Disconnect from VPN and close OpenVPN Daemon
#'
#' @description This function disconnects from a VPN server and closes the OpenVPN Daemon.
#'
#' @return Nothing.
#'
#' @param verbose A boolean. If TRUE, prints informations.
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' close_vpn()
#' }



close_vpn <- function(verbose = TRUE) {

  is_unix()
  is_sudo()

  std <- system("killall openvpn", ignore.stderr = TRUE)

  if (verbose) {

    if (std == 0) {

      Sys.sleep(2)
      ip <- get_ip()

      cat("You are now disconnected from OpenVPN.\n")

    } else {

      ip <- get_ip()

      cat("OpenVPN was already closed.\n")
    }

    cat("Your public IP address is:", ip, "\n")
  }
}
