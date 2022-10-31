#' Check if the operating system is Unix
#'
#' @description
#' Checks if the system is running under Unix (GNU/Linux and macOS).
#' The package is not implemented to work under Windows.
#'
#' @return No return value. An error is generated if this prerequisite is not 
#' met.
#'
#' @noRd
#' 
#' @examples
#' \dontrun{
#' check_unix()
#' }

check_unix <- function() {
  
  if (tolower(.Platform$"OS.type") != "unix") {
    stop("Only Unix systems are supported", call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check if RStudio is running
#'
#' @description
#' Checks if the package is running outside RStudio. RStudio has a strange
#' behavior with the command [system()] and the env. variable `PATH` 
#' (`openvpn`).
#'
#' @return No return value. An error is generated if this prerequisite is not 
#' met.
#'
#' @noRd
#' 
#' @examples
#' \dontrun{
#' check_rstudio()
#' }

check_rstudio <- function() {
  
  if (rstudioapi::isAvailable()) {
    stop("The package 'gpack' must be run outside RStudio", call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check if OpenVPN is installed
#'
#' @description
#' Checks if the software OpenVPN is installed and if the path to its binaries
#' is found.
#'
#' @return No return value. An error is generated if this prerequisite is not 
#' met.
#'
#' @noRd
#' 
#' @examples
#' \dontrun{
#' check_openvpn()
#' }

check_openvpn <- function() {
  
  is_openvpn <- system(paste("echo", paste0("'", get_password(), "'"),
                             "| sudo -S which openvpn"),
                       ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  if (is_openvpn == 1) {
    stop("Unable to find the Unix command 'openvpn'", call. = FALSE)
  }
  
  
  ## Check NordVPN configuration files ----
  
  if (!dir.exists("~/.ovpn")) {
    stop("Cannot find the folder '~/.ovpn/'", call. = FALSE)
  }
  
  config_files <- list.files(path = "~/.ovpn/", pattern = "\\.ovpn$")
  
  if (length(config_files) == 0) {
    stop("Unable to find config files in '~/.ovpn/'", call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check if Docker is installed and running
#'
#' @description
#' Checks if Docker is installed and running.
#'
#' @return No return value. An error is raised if Docker is not installed or 
#' not running.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' check_docker()
#' }

check_docker <- function() {
  
  std <- system("which docker", ignore.stdout = TRUE)
  
  if (std == 1) {
    stop("Please install the software Docker", call. = FALSE)
  }
  
  std <- system("docker ps", ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  if (std == 1) {
    stop("Docker daemon is not running", call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check if the Docker image selenium/standalone-firefox is installed
#'
#' @description
#' Checks if the Docker image `selenium/standalone-firefox` is installed.
#'
#' @return No return value. An error is raised if the Docker image 
#' `selenium/standalone-firefox` is absent.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' check_selenium()
#' }

check_selenium <- function() {
  
  images <- system("docker images", intern = TRUE)
  images <- unlist(lapply(strsplit(images, " "), function(x) x[1]))
  
  if (!("selenium/standalone-firefox" %in% images)) {
    stop("Unable to find the Docker image 'selenium/standalone-firefox'",
         call. = FALSE)
  }
  
  invisible(NULL)
}



#' Check if the Unix user password is correctly stored
#'
#' @description
#' Checks if the Unix user password is correctly stored in the `.Renviron` file.
#'
#' @return No return value. An error is generated if this prerequisite is not 
#' met.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' check_password()
#' }

check_password <- function() {
  
  pwd <- Sys.getenv("UNIX_PASSWD")
  
  if (pwd == "") {
    
    stop("Unable to read the UNIX user password. Run ", 
         "`usethis::edit_r_environ()` and store your password under the key ", 
         "'UNIX_PASSWD'.", call. = FALSE)
  }
  
  invisible(NULL)
}



#' Get the Unix user password
#'
#' @description
#' Gets the Unix user password stored in `.Renviron` under the key 
#' `UNIX_PASSWD`.
#'
#' @return The Unix user password value.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_password()
#' }

get_password <- function() {
  
  check_password()
  
  Sys.getenv("UNIX_PASSWD")
}



#' Get if the laptop is connected to Internet
#'
#' @description
#' Gets if the laptop is connected to Internet.
#'
#' @return No return value. An error is generated if this prerequisite is not 
#' met.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' check_network()
#' }

check_network <- function() {
  
  std <- system(paste("ping -c 1 -W 2", "8.8.8.8"), 
                ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  if (std != 0) {
    stop("No network connection detected", call. = FALSE)
  }
  
  invisible(NULL)
}
