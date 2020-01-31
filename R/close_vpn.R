close_vpn <- function(verbose = TRUE) {

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
