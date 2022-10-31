#' Check user system before using the package
#'
#' @description
#' Checks user system before using the package. In particular, checks if:
#'   - the OS is based on Unix (no support for Windows)
#'   - the user password is correctly stored
#'   - the system has an Internet connection
#'   - R is running outside RStudio Desktop
#'   - the software OpenVPN is installed and accessible
#'   - VPN configuration files are stored locally
#'   - the software Docker is installed and running
#'   - the Docker image Selenium has been downloaded
#'
#' @param verbose a `logical`. If `TRUE`, displays information.
#' 
#' @return No return value. An error is raised if user system is not correctly
#' set.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check_system()
#' }

check_system <- function(verbose = TRUE) {
  
  is_logical(verbose)
  
  if (verbose) {
    messages::msg_rule("Checking system")
    messages::msg_line() 
  }
  
  
  ## Check OS ----
  
  check_unix()
  
  if (verbose) {
    messages::msg_line(paste0("   ", cli::symbol$"bullet", " Operating system"))
  }
  
  
  ## Check Unix password ----
  
  check_password()
  
  if (verbose) {
    messages::msg_line(paste0("   ", cli::symbol$"bullet", " User password"))
  }
  
  
  ## Check internet connection ----
  
  check_network()
  
  if (verbose) {
    messages::msg_line(paste0("   ", cli::symbol$"bullet", " Internet connection"))
  }
  
  
  ## Check R outside RStudio ----
  
  check_rstudio()
  
  if (verbose) {
    messages::msg_line(paste0("   ", cli::symbol$"bullet", " R system"))
  }
  
  
  ## Check OpenVPN & NordVPN config files ----
  
  check_openvpn()
  
  if (verbose) {
    messages::msg_line(paste0("   ", cli::symbol$"bullet", " OpenVPN"))
  }
  
  
  ## Check Docker ----
  
  check_docker()
  
  if (verbose) {
    messages::msg_line(paste0("   ", cli::symbol$"bullet", " Docker engine"))
  }
  
  
  ## Check Selenium image ----
  
  check_selenium()
  
  if (verbose) {
    messages::msg_line(paste0("   ", cli::symbol$"bullet", " Selenium image"))
  }
  
  
  if (verbose) {
    messages::msg_line()
    messages::msg_done("System ready to run", messages::msg_value('gpack'))
  }
  
  invisible(NULL)
}
