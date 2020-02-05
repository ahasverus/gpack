#' @title Disconnect from VPN Server and close OpenVPN Daemon
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

  std <- system(
    paste(
      "echo",
      paste0("'", unix_password(), "'"),
      "| sudo -S killall openvpn"
    ),
    ignore.stderr = TRUE
  )

  if (verbose) {

    if (std == 0) {

      Sys.sleep(2)
      ip <- get_ip()

      usethis::ui_done(
        stick(
          "OpenVPN has been successfully stopped",
          indent = " "
        )
      )

    } else {

      ip <- get_ip()

      usethis::ui_todo(
        stick(
          "OpenVPN was already stopped",
          indent = " "
        )
      )
    }

    usethis::ui_info(
      stick(
        "
          Unprotected public IP address:
          {usethis::ui_value(ip)}
        ",
        indent = " "
      )
    )
  }
}
