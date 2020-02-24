#' @title Retrieve Data from Google Search
#'
#' @description This function sends a request to Google Scholar service and
#' retrieves results (title, authors, source and year of publications,
#' and the total number of citations). As no API is provided by Google Scholar
#' (except the one for authors with a Google Scholar ID), this function webscraps
#' the service using \code{RSelenium} or the packages \code{rvest} and \code{httr}.
#' To bypass Google IP bans, user is strongly encouraged to set the arguments
#' \code{openvpn} and \code{agent} as \code{TRUE}. In this way, every 10-20
#' sub-requests your IP and your user agent will be automatically and randomly changed.
#'
#' @param search_terms A character vector of terms to search publications for.
#' @param port The port number to run on Selenium server.
#' @param openvpn A boolean. If TRUE, public IP address will be randomly changed.
#' @param config_path The path to the folder containing server configuration files.
#' @param ovpn_country The ISO-2 code of the country to pick up a server. Use \code{get_countries()} to get a list.
#' @param agent A boolean. If TRUE, browser user agent will be randomly changed.
#' @param sleep The time interval (in seconds) between two sub-requests.
#' @param verbose A boolean. If TRUE, connexion and webscraping informations are printing.
#' @param output_path The path to the folder to save data.
#' @param exposed_ip The unprotected IPv4 address.
#' @param rs_driver An RSelenium server.
#'
#' @return The RSelenium server
#'
#' @import RSelenium
#' @import xml2
#' @import rvest
#' @import cli
#' @import usethis
#' @import crayon
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' scrap_gsearch()
#' }



scrap_gsearch <- function(
  search_terms, port = 4567L, openvpn = TRUE, rs_driver = NULL,
  config_path = "~/.ovpn", ovpn_country, agent = TRUE, sleep = 1,
  verbose = TRUE, output_path = ".", exposed_ip = NULL
) {



  ### Parameters checks                                                         ----------


  if (is.null(sleep)) {

    sleep <- 0
  }

  if (length(sleep) != 1) {

    usethis::ui_stop(
      stick(
        "
          Argument
          {usethis::ui_field('sleep')}
          must be a
          {usethis::ui_value('numeric of length 1')}
        "
      )
    )
  }

  if (!is.numeric(sleep)) {

    usethis::ui_stop(
      stick(
        "
          Argument
          {usethis::ui_field('sleep')}
          must be a
          {usethis::ui_value('numeric of length 1')}
        "
      )
    )
  }


    if (is.null(port)) {

      port <- 4567L
    }

    if (length(port) != 1) {

      usethis::ui_stop(
        stick(
          "
            Argument
            {usethis::ui_field('port')}
            must be a
            {usethis::ui_value('numeric of length 1')}
          "
        )
      )
    }

    if (!is.numeric(port)) {

      usethis::ui_stop(
        stick(
          "
            Argument
            {usethis::ui_field('port')}
            must be a
            {usethis::ui_value('numeric of length 1')}
          "
        )
      )
    }


  env <- new.env()
  env$openvpn           <- openvpn
  env$agent             <- agent
  env$verbose           <- verbose

  check_boolean("openvpn", env)
  check_boolean("agent", env)
  check_boolean("verbose", env)


  if (is.null(search_terms)) {

    usethis::ui_stop(
      stick(
        "You must provide a term (or an expression) to search for"
      )
    )
  }

  env$search_terms <- search_terms
  check_string("search_terms", env)

  search_terms <- escape_url(search_terms)


  if (missing(ovpn_country)) {

    ovpn_country <- NULL
  }



  ### Welcome message                                                           ----------



  if (verbose) {

    cli::cat_line()
    cli::cat_rule(left = "Scraping Google Search Engine", line_col = "darkgrey")
    cli::cat_line()

    cli::cat_line(crayon::underline("Request details"))

    terms <- usethis::ui_value(gsub('%20', ' ', search_terms))
    terms <- paste(terms, "(keywords)")
    terms <- paste0(" Searching for ", terms)

    usethis::ui_info(terms)
  }



  ### Write GS request                                                        ----------



  url <- paste0(
    "https://www.google.com/search",      # URL Root
    "?safe=active",                                  # Secure Search
    "&hl=en",                                        # Interface Language
    "&q=",                                      # Exact terms (in same order)
    "%22", search_terms, "%22"
  )



  filename <- paste0(
    tolower(gsub("%20", "_", search_terms)),
    "_",
    format(Sys.time(), "%y%m%d%H%M%S")
  )



  Sys.sleep(sleep)



  ### Open URL in Browser                                                     ----------


  rs_driver$client$navigate(url)



  ### Check for Ban                                                         ----------



  k       <- 1
  captcha <- rs_driver$client$findElements(using = "id", value = "gs_captcha_c")

  if (!length(captcha)) {

    captcha <- rs_driver$client$findElements(using = "id", value = "recaptcha")

  }

  if (!length(captcha)) {

    captcha <- rs_driver$client$findElement(using = "tag", value = "div")
    captcha <- grep("\\\nip addr", tolower(captcha$getElementText()))
  }



  while (length(captcha)) {



    Sys.sleep(sleep)



    ### Ban messages                                                            ----------



    if (!openvpn) {

      usethis::ui_stop(
        stick("You have been banned from Google Scholar")
      )

    } else {

      if (k == 1) {

        if (verbose) {

          cli::cat_line()
          cli::cat_line()
          cli::cat_line(crayon::underline("Google Search ban"))
        }
      }

      usethis::ui_todo(
        stick(
          "
            Trying to connect to another server.
            Attempt
            {usethis::ui_value(k)}
          ",
          indent = " "
        )
      )



      ### Change IP address                                                     ----------


      close_vpn(verbose = FALSE)

      invisible(
        change_ip(
          config_path  = config_path,
          exposed_ip   = exposed_ip,
          country      = ovpn_country,
          ignore_files = NULL,
          verbose      = verbose
        )
      )



      rs_driver$client$closeWindow()

      Sys.sleep(2)

      rs_driver$client$open(silent = TRUE)

      rs_driver$client$navigate(url)


      k <- k + 1


      rs_driver$client$refresh()

      captcha <- rs_driver$client$findElements(using = "id", value = "gs_captcha_c")

      if (!length(captcha)) {

        captcha <- rs_driver$client$findElements(using = "id", value = "recaptcha")

      }

      if (!length(captcha)) {

        captcha <- rs_driver$client$findElement(using = "tag", value = "div")
        captcha <- grep("\\\nip addr", tolower(captcha$getElementText()))

      }



      ### Prevent infinite loop                                                 ----------



      if (k > 10) {

        usethis::ui_stop(
          stick("You have been permanently banned from Google Scholar")
        )
      }
    }  # e_o while openvpn
  } # e_o while ban



  ### Get Total Matches                                                         ----------


  n_matches <- rs_driver$client$findElement(using = "id", value = "mBMHK")

  total <- n_matches$getElementText()[[1]]
  total <- gsub("\\(.+\\)|About|results|result|[[:space:]]|[[:punct:]]", "", total)
  total <- as.numeric(total)
  total <- ifelse(is.na(total), 0, total)


  if (verbose) {

    cli::cat_line()
    cli::cat_line(crayon::underline("Response details"))
    usethis::ui_info(
      stick(
        "
          Estimated number of results:
          {usethis::ui_value(total)}
        ",
        indent = " "
      )
    )
  }



  rs_driver$client$closeWindow()



  cli::cat_line()
  cli::cat_rule(right = "done.", col = "darkgrey")



  dir.create(
    file.path(
      output_path,
      tolower(gsub("%20", "_", search_terms))
    ),
    showWarnings = FALSE,
    recursive    = TRUE
  )

  save(
    total,
    file = file.path(
      output_path,
      tolower(gsub("%20", "_", search_terms)),
      paste0(filename)
    )
  )

  return(rs_driver)

}
