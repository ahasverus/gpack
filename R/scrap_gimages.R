#' @title Download Images from Google Images
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
#' @param start A numeric specifying the number of the first results from which the results are extracted.
#' @param n_max A numeric specifying the number of results to extract.
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
#' @import utils
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' scrap_gimages()
#' }



scrap_gimages <- function(
  search_terms, start = 0, n_max = NULL, rs_driver = NULL, openvpn = TRUE,
  config_path = "~/.ovpn", ovpn_country, agent = TRUE, sleep = 1,
  verbose = TRUE, output_path = ".", exposed_ip = NULL
) {



  ### Parameters checks                                                         ----------


  if (is.null(start)) {

    start <- 0
  }

  if (length(start) != 1) {

    usethis::ui_stop(
      stick(
        "
          Argument
          {usethis::ui_field('start')}
          must be a
          {usethis::ui_value('numeric of length 1')}
        "
      )
    )
  }

  if (!is.numeric(start)) {

    usethis::ui_stop(
      stick(
        "
          Argument
          {usethis::ui_field('start')}
          must be a
          {usethis::ui_value('numeric of length 1')}
        "
      )
    )
  }

  if (!is.null(n_max)) {

    if (length(n_max) != 1) {

      usethis::ui_stop(
        stick(
          "
            Argument
            {usethis::ui_field('n_max')}
            must be a
            {usethis::ui_value('numeric of length 1')}
          "
        )
      )
    }

    if (!is.numeric(n_max)) {

      usethis::ui_stop(
        stick(
          "
            Argument
            {usethis::ui_field('n_max')}
            must be a
            {usethis::ui_value('numeric of length 1')}
          "
        )
      )
    }
  }

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
    cli::cat_rule(left = "Scraping Google Images", line_col = "darkgrey")
    cli::cat_line()

    cli::cat_line(crayon::underline("Request details"))


    terms <- usethis::ui_value(gsub('%20', ' ', search_terms))
    terms <- paste(terms, "(keywords)")

    terms <- paste0(" Searching for ", terms)

    usethis::ui_info(terms)

  }



  ### Write GI request                                                          ----------



  url <- paste0(
    "https://www.google.com/search",      # URL Root
    "?q=",                                # Exact terms (in same order)
    search_terms,
    "&source=lnms",
    "&tbm=isch",
    "&safe=images",
    "&hl=en"                             # Interface Language
  )



  Sys.sleep(sleep)



  ### Open URL in Browser                                                       ----------



  rs_driver$client$navigate(url)



  ### Check for Ban                                                             ----------



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
        stick("You have been banned from Google Images")
      )

    } else {

      if (k == 1) {

        if (verbose) {

          cli::cat_line()
          cli::cat_line()
          cli::cat_line(crayon::underline("Google Images ban"))
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
          stick("You have been permanently banned from Google Images")
        )
      }

    }  # e_o while openvpn
  } # e_o while ban




  ### Scroll Down                                                               ----------


  thumb_links <- rs_driver$client$findElements(using = "class", value = "rg_i")
  new_links   <- TRUE

  while (length(thumb_links) < (n_max + start) && new_links) {

    scroll_down <- rs_driver$client$findElement(using = "css", value = "body")
    scroll_down$sendKeysToElement(list(key = "end"))

    previous_links <- thumb_links

    Sys.sleep(0.75)

    thumb_links <- rs_driver$client$findElements(using = "class", value = "rg_i")

    if (length(thumb_links) == length(previous_links)) {

      if (length(thumb_links) < (n_max + start)) {

        button <- rs_driver$client$findElements(using = 'tag', value = "input")
        button[[4]]$clickElement()

        Sys.sleep(0.75)

        scroll_down <- rs_driver$client$findElement(using = "css", value = "body")
        scroll_down$sendKeysToElement(list(key = "end"))

        Sys.sleep(0.75)

        previous_links <- thumb_links

        thumb_links <- rs_driver$client$findElements(using = "class", value = "rg_i")

        if (length(thumb_links) == length(previous_links)) {

          new_links <- FALSE
        }

      } else {

        new_links <- FALSE

      }
    }
  }



  ### Get Thumbnails URL                                                        ----------


  thumb_links <- rs_driver$client$findElements(using = "class", value = "rg_i")

  if (length(thumb_links) >= (n_max + start)) {

    thumb_links <- thumb_links[(start + 1):(n_max + start)]

  } else {

    thumb_links <- thumb_links[(start + 1):length(thumb_links)]
  }


  if (verbose) {

    cli::cat_line()
    cli::cat_line(crayon::underline("Response details"))
    usethis::ui_info(
      stick(
        "
          Number of images:
          {usethis::ui_value(length(thumb_links))}
        ",
        indent = " "
      )
    )
  }


  photo_id <- as.numeric(format(Sys.time(), "%Y%m%d%H%M%S"))
  count    <- 0

  gi_results <- data.frame()

  for (k in 1:length(thumb_links)) {



    ### Go to original image                                                    ----------


    Sys.sleep(sample(seq(0, 2, by = 0.01), 1))

    thumb_links[[k]]$clickElement()



    ### Get original image URL                                                  ----------


    img_link <- rs_driver$client$findElements(using = "tag", value = "img")

    img_link <- unlist(
      sapply(
        img_link,
        function(x){
          x$getElementAttribute("src")
        }
      )
    )

    img_link <- img_link[grep("^http", img_link)]

    pos <- grep("encrypted-tbn0", img_link)
    if (length(pos)) {
      img_link <- img_link[-pos]
    }



    ### Download original image                                                 ----------

    # attempt <- tryCatch({
    #   utils::download.file(
    #     url       = img_link,
    #     destfile  = file.path(output_path, paste0("IMG", photo_id, ".jpg")),
    #     quiet     = TRUE
    #   )},
    #   error = function(e){}
    # )

    if (!is.null(attempt)) {

      count    <- count + 1
      photo_id <- photo_id + 1
    }

    dat <- data.frame(
      species = strsplit(output_path, "/")[[1]][length(strsplit(output_path, "/")[[1]])],
      query   = search_terms,
      url     = img_link
    )
    gi_results <- rbind(gi_results, dat)
  }

  if (verbose) {

    usethis::ui_info(
      stick(
        "
          Images successfully downloaded:
          {usethis::ui_value(paste0(count, \" on \", length(thumb_links)))}
        ",
        indent = " "
      )
    )
  }

  save(gi_results, file = file.path(output_path, paste0(format(Sys.time(), "%Y%m%d%H%M"), ".rda")))

  rs_driver$client$closeWindow()

  cli::cat_line()
  cli::cat_rule(right = "done.", col = "darkgrey")

  return(rs_driver)

}
