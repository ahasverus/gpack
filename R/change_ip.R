#' @title Run OpenVPN and Change IP Address
#'
#' @description This function changes your IP Address by connecting to a randomly
#' selected third-party server using OpenVPN software.
#'
#' @param config_path The path to the folder containing server configuration files.
#' @param exposed_ip User unprotected public IPv4 address obtained with \code{close_vpn()}.
#' @param country (optional) The ISO-2 code of the country (>= 1) from which a VPN server will be pick up .
#' @param ignore_files (optional) Vector of servers configuration file not to be connected.
#' @param verbose A boolean. If TRUE, prints connexion informations.
#'
#' @return The selected server.
#'
#' @import usethis
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' close_vpn()
#' ip <- get_ip()
#' change_ip(exposed_ip = get_ip())
#'
#' close_vpn()
#' change_ip(exposed_ip = get_ip(), country = "es")
#' }



change_ip <- function(
  config_path = "~/.ovpn", exposed_ip, country, ignore_files = NULL,
  verbose = TRUE
) {

  is_unix()

  if (missing(exposed_ip)) {

    usethis::ui_stop(
      stick(
        "
          Please provide your unprotected public IP address.
          Run
          {usethis::ui_code('close_vpn()')}
        "
      )
    )
  }

  if (is.null(exposed_ip)) {

    usethis::ui_stop(
      stick(
        "
          Please provide your unprotected public IP address.
          Run
          {usethis::ui_code('close_vpn()')}
        "
      )
    )
  }

  is_openvpn <- system(
    paste(
      "echo",
      paste0("'", unix_password(), "'"),
      "| sudo -S which openvpn"
    ),
    ignore.stdout = TRUE,
    ignore.stderr = TRUE
  )

  if (is_openvpn == 1) {

    usethis::ui_stop(
      stick(
        "
          Unable to find the Unix command
          {usethis::ui_field('openvpn')}
        "
      )
    )
  }

  if (!dir.exists(config_path)) {

    usethis::ui_stop(
      stick(
        "
          Cannot find the folder
          {usethis::ui_value(config_path)}
        "
      )
    )
  }

  config_files <- list.files(path = config_path, pattern = "\\.ovpn$")

  if (!length(config_files)) {

    usethis::ui_stop(
      stick(
        "
          Unable to find config files in
          {usethis::ui_value(config_path)}
        "
      )
    )
  }

  if (!is.null(ignore_files)) {

    config_files <- config_files[which(!(config_files %in% ignore_files))]
  }

  if (!missing(country)) {

    if (!is.null(country)) {

      if (!length(country)) {

        usethis::ui_stop(
          stick(
            "
              Argument
              {usethis::ui_field('country')}
              must be a
              {usethis::ui_value('character of length >= 1')}
            "
          )
        )
      }

      if (!is.character(country)) {

        usethis::ui_stop(
          stick(
            "
              Argument
              {usethis::ui_field('country')}
              must be a
              {usethis::ui_value('character of length >= 1')}
            "
          )
        )
      }


      pattern <- paste0(paste0("^", country), collapse = "|")
      config_files <- config_files[grep(pattern, config_files, ignore.case = TRUE)]

      if (!length(config_files)) {

        usethis::ui_stop(
          stick(
            "
              No config files available for
              {usethis::ui_value(toupper(paste0(country, collapse = ", ")))}
            "
          )
        )
      }
    }
  }


  config_file <- sample(config_files, 1)

  invisible(
    system(
      paste(
        "echo",
        paste0("'", unix_password(), "'"),
        "| sudo -S killall openvpn"
      ),
      ignore.stderr = TRUE
    )
  )

  invisible(
    system(
      paste(
        "echo",
        paste0("'", unix_password(), "'"),
        "| sudo -S openvpn --config",
        file.path(config_path, config_file),
        "--daemon --auth-nocache"
      ),
      ignore.stderr = TRUE
    )
  )

  Sys.sleep(5)

  ip <- get_ip()

  if (ip == exposed_ip) {

    usethis::ui_stop(
      stick(
        "
          Unable to reach the
          {usethis::ui_value('VPN server')}
        "
      )
    )
  }

  if (verbose) {

    iso_2   <- substr(config_file, 1, 2)
    country <- get_countries()[which(get_countries()$iso_2 == iso_2), "country"]



    usethis::ui_done(
      stick(
        paste0(
          "New public IP address: ",
          usethis::ui_value(ip),
          " (",
          usethis::ui_field(country),
          ")"
        ),
        indent = " "
      )
    )
  }

  return(config_file)
}
