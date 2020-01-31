#' @title Run OpenVPN and Change IP Address
#'
#' @description This function changes your IP Address by connecting to a randomly
#' selected third-party server using OpenVPN software.
#'
#' @param config_path The path to the folder containing server configuration files.
#' @param exposed_ip User non-protected public IPv4 address obtained with \code{get_ip()}.
#' @param country [optional] The ISO-2 code of the country to pick up a server.
#' @param ignore_files [optional] TVector of servers configuration file not to be connected.
#' @param verbose A boolean. If TRUE, prints connexion informations.
#'
#' @return The selected server.
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' change_ip()
#' }



change_ip <- function(config_path = "~/.ovpn", exposed_ip, country, ignore_files = NULL, verbose = TRUE) {

  if (missing(exposed_ip) || !is.null(exposed_ip)) {
    stop("Please provide your non-protected public IP address (run `get_ip()`).")
  }

  is_unix()
  is_sudo()

  if (system("which openvpn", ignore.stdout = TRUE) == 1) {
    stop("Unable to find the Unix command 'openvpn'.")
  }

  if (!dir.exists(config_path)) {
    stop(paste0("Cannot find the folder ", config_path, "."))
  }

  config_files <- list.files(path = config_path, pattern = "\\.ovpn$")

  if (!length(config_files)) {
    stop(paste0("Unable to find config files in ", config_path, "."))
  }

  if (!is.null(ignore_files)) {
    config_files <- config_files[which(!(config_files %in% ignore_files))]
  }

  if (!missing(country) || !is.null(country)) {

    if (length(country) > 1) {
      stop("Argument 'country' must be length 1.")
    }

    if (nchar(country) != 2) {
      stop("Argument 'country' must be the ISO-2 code of the country.")
    }

    config_files <- config_files[grep(paste0("^", country), config_files, ignore.case = TRUE)]

    if (!length(config_files)) {
      stop(paste0("No config files available for ", toupper(country), "."))
    }
  }

  config_file <- sample(config_files, 1)

  system("killall openvpn", ignore.stderr = TRUE)

  system(
    paste(
      "openvpn --config",
      file.path(config_path, config_file),
      "--daemon --auth-nocache"
    )
  )

  Sys.sleep(5)

  ip <- get_ip()

  if (ip == exposed_ip) {
    stop("Unable to connect OpenVPN to server.")
  }

  if (verbose) {

    cat("You are now connected to:", config_file, "\n")
    cat("with the IP address:", ip, "\n")
  }

  return(config_file)
}
