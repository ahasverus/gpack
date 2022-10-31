#' Check if an IP address is valid
#'
#' @description
#' Checks if an IP address is valid and well-formed. Only IPv4 addresses are 
#' supported.
#' 
#' @param x a `character` of length 1. An IPv4 address to check.
#'
#' @return No return value. An error is raised if the IPv4 address is malformed
#' and not valid.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' is_valid_ip("127.0.0.1")
#' is_valid_ip("255.255.255.255")
#' is_valid_ip("256.0.0.1")
#' }

is_valid_ip <- function(x) {
  
  is_character(x)
  
  ip_regex <- "^([0-9]{1,3}\\.){3}[0-9]{1,3}$"
  
  if (!grepl(ip_regex, x)) {
    stop("Invalid IPv4 address", call. = FALSE)
  }
  
  x <- unlist(strsplit(x, "\\.")[[1]])
  x <- as.numeric(x)
  
  if (any(x > 255)) {
    stop("Invalid IPv4 address", call. = FALSE)
  }
  
  invisible(NULL)
}



#' Get the public IPv4 address
#'
#' @description 
#' Retrieves the public IPv4 address of the network using the **ipify API** 
#' (\url{https://api.ipify.org}).
#'
#' @return The IPv4 address.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' get_ip()
#' }

get_ip <- function() {
  
  readLines(con = "https://api.ipify.org/", warn = FALSE, encoding = "UTF-8")
}



#' Disconnect from a VPN server and close OpenVPN daemon
#'
#' @description 
#' Disconnects from a VPN server and closes the OpenVPN Daemon.
#' 
#' @param verbose a `logical`. If `TRUE`, displays information.
#' 
#' @return No return value.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' stop_vpn()
#' }

stop_vpn <- function(verbose = TRUE) {
  
  is_logical(verbose)
  
  check_unix()
  check_password()
  check_rstudio()
  check_openvpn()
  
  std <- system(paste("echo", paste0("'", get_password(), "'"), 
                      "| sudo -S killall openvpn"),
                ignore.stderr = TRUE)
  
  if (verbose) {
    
    if (std == 0) {
      
      Sys.sleep(2)
      ip <- get_ip()
      messages::msg_done("OpenVPN has been successfully stopped")
      
    } else {
      
      ip <- get_ip()
      messages::msg_todo("OpenVPN was already stopped")
    }
    
    messages::msg_info("Unprotected public IP address:", 
                       messages::msg_value(ip))
  }
  
  invisible(NULL)
}



#' Disconnect from a VPN server and close OpenVPN daemon
#'
#' @description 
#' Disconnects from a VPN server and closes the OpenVPN Daemon.
#'
#' @param server a `character` of length 1. The configuration file of one VPN 
#'   server.
#' 
#' @param verbose a `logical`. If `TRUE`, displays information.
#'
#' @return No return value.
#'
#' @noRd
#' 
#' @examples
#' \dontrun{
#' start_vpn()
#' }

start_vpn <- function(server, verbose = TRUE) {
  
  
  ## Check system ----
  
  check_unix()
  check_password()
  check_rstudio()
  check_openvpn()
  
  
  ## Check args ----
  
  is_character(server)
  is_logical(verbose)
  
  stop_vpn(verbose)
  
  Sys.sleep(5)
  
  std <- system(paste("echo", paste0("'", get_password(), "'"),
                      "| sudo -S openvpn --config",
                      file.path("~/.ovpn", server),
                      "--daemon --auth-nocache"),
                ignore.stderr = TRUE, ignore.stdout = TRUE)
  
  if (std == 1) {
    stop("Unable to connect to VPN server", call. = FALSE)
  }
  
  Sys.sleep(5)
  
  if (verbose) {
    messages::msg_done("Successfully connected to", messages::msg_value(server))
    messages::msg_done("New public IP address:", messages::msg_value(get_ip()))
  }
  
  invisible(NULL)
}



#' Start OpenVPN and change IP address
#'
#' @description
#' Changes the public IP address by connecting to a randomly selected 
#' third-party server using OpenVPN software.
#'   
#' @param country (optional) a `character` vector. ISO-2 code of the countries 
#'   (>= 1) from which a VPN server will be picked.
#'   
#' @param ignore_files (optional) a `character` vector. VPN servers 
#'   configuration file to ignore.
#'   
#' @param verbose a `logical`. If `TRUE`, prints connection information.
#'
#' @return The selected server.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' close_vpn()
#' get_ip()
#' 
#' change_ip()
#' get_ip()
#'
#' close_vpn()
#' }

change_ip <- function(country = NULL, ignore_files = NULL, verbose = TRUE) {
  
  
  ## Check system ----
  
  check_unix()
  check_password()
  check_rstudio()
  check_openvpn()
  
  
  ## Check args ----
  
  if (!is.null(country)) {
    is_characters(country)
  }
  
  if (!is.null(ignore_files)) {
    is_characters(ignore_files)
  }
  
  is_logical(verbose)
  
  
  ## Get VPN servers configuration files ----
  
  config_files <- list.files(path = "~/.ovpn", pattern = "\\.ovpn$")
  
  
  ## Filter VPN servers configuration files ----
  
  if (!is.null(ignore_files)) {
    config_files <- config_files[!(config_files %in% ignore_files)]
  }
  
  if (!is.null(country)) {
    
    pattern      <- paste0(paste0("^", country), collapse = "|")
    config_files <- config_files[grep(pattern, config_files, 
                                      ignore.case = TRUE)]
    
    if (!length(config_files)) {
      stop("No config files available for ", 
           messages::msg_value(toupper(paste0(country, collapse = ", "))),
           call. = FALSE)
    }
  }
  
  
  k <- 1
  
  server <- "offline"
  
  while (server == "offline") {
    
    ## Select a VPN server ----
    
    config_file <- sample(config_files, 1)
    
    
    ## Stop and start OpenVPN ----
    
    start_vpn(server = config_file, verbose)
    
    
    ## Test server ----
    
    test <- suppressMessages(tryCatch(get_ip(), error = function(e) NA))
    
    if (!is.na(test)) {
      server <- "online"
    }
    
    
    ## Prevent infinite loop ----
    
    if (k > 10) {
      stop("Unable to find an appropriate VPN server", call. = FALSE)
    }
    
    
    k <- k + 1
  }
  
  
  
  
  invisible(config_file)
}



#' Check the current web browser user agent
#'
#' @description
#' Checks how a web browser is detected (user agent) by a server using
#' the **What's My UA API** (\url{https://www.whatsmyua.info/api/v1/ua}).
#'
#' @param agent a `character` of length 1. Check a user agent.
#' 
#' @param verbose a `logical`. If `TRUE`, the new user agent is printed.
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' ## Default user agent ----
#' check_ua()
#'
#' ## Change user agent ----
#' ua <- change_ua()
#' check_ua(agent = ua)
#' }

check_ua <- function(agent = NULL, verbose = TRUE) {
  
  is_logical(verbose)
  
  if (!is.null(agent)) {
    
    is_character(agent)
    
    ua_sent  <- httr::user_agent(agent)
    response <- httr::GET("https://www.whatsmyua.info/api/v1/ua", ua_sent)
    
  } else {
    
    response <- httr::GET("https://www.whatsmyua.info/api/v1/ua")
  }
  
  
  if (response$"status_code" != 200) {
    stop("Unable to resolve host 'https://www.whatsmyua.info'", call. = FALSE)
  }
  
  response <- httr::content(response, as = "text")
  
  ua_long  <- jsonlite::fromJSON(response)[1, "ua"]$"rawUa"
  ua_short <- jsonlite::fromJSON(response)[3, "device"]$"description"
  
  
  if (!is.null(agent)) {
    
    if (agent != ua_long) {
      stop("Unable to change 'user-agent'", call. = FALSE)
    }
  }
  
  if (verbose) {
    
    messages::msg_done("Your system is now detected as", 
                       messages::msg_value(ua_short))
  }
  
  invisible(NULL)
}



#' Get a random web browser user agent
#'
#' @description
#' Randomly selects a user agent among a list of 81 obtained from 
#' \url{https://techblog.willshouse.com/2012/01/03/most-common-user-agents/}.
#' The new user agent is checked using the What's My UA API 
#' (\url{https://www.whatsmyua.info/api/v1/ua}).
#'
#' @param verbose a `logical`. If `TRUE`, the new user agent is printed.
#'
#' @return The selected user agent (a `character`).
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' change_ua()
#' }

change_ua <- function(verbose = TRUE) {
  
  agent <- as.character(sample(agents$"user_agent", 1))
  
  if (verbose) {
    check_ua(agent)
  }
  
  agent
}
