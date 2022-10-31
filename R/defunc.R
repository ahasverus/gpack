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
#' @param download A boolean. If TRUE, original picture will be downloaded.
#'
#' @return The RSelenium server
#'
#' @noRd

# scrap_gimages <- function(
#   search_terms, start = 0, n_max = NULL, rs_driver = NULL, openvpn = TRUE,
#   config_path = "~/.ovpn", ovpn_country, agent = TRUE, sleep = 1,
#   verbose = TRUE, output_path = ".", exposed_ip = NULL, download = TRUE
# ) {
# 
# 
# 
#   ### Parameters checks                                                         ----------
# 
# 
#   if (is.null(start)) {
# 
#     start <- 0
#   }
# 
#   if (length(start) != 1) {
# 
#     usethis::ui_stop(
#       stick(
#         "
#           Argument
#           {usethis::ui_field('start')}
#           must be a
#           {usethis::ui_value('numeric of length 1')}
#         "
#       )
#     )
#   }
# 
#   if (!is.numeric(start)) {
# 
#     usethis::ui_stop(
#       stick(
#         "
#           Argument
#           {usethis::ui_field('start')}
#           must be a
#           {usethis::ui_value('numeric of length 1')}
#         "
#       )
#     )
#   }
# 
#   if (!is.null(n_max)) {
# 
#     if (length(n_max) != 1) {
# 
#       usethis::ui_stop(
#         stick(
#           "
#             Argument
#             {usethis::ui_field('n_max')}
#             must be a
#             {usethis::ui_value('numeric of length 1')}
#           "
#         )
#       )
#     }
# 
#     if (!is.numeric(n_max)) {
# 
#       usethis::ui_stop(
#         stick(
#           "
#             Argument
#             {usethis::ui_field('n_max')}
#             must be a
#             {usethis::ui_value('numeric of length 1')}
#           "
#         )
#       )
#     }
#   }
# 
#   if (is.null(sleep)) {
# 
#     sleep <- 0
#   }
# 
#   if (length(sleep) != 1) {
# 
#     usethis::ui_stop(
#       stick(
#         "
#           Argument
#           {usethis::ui_field('sleep')}
#           must be a
#           {usethis::ui_value('numeric of length 1')}
#         "
#       )
#     )
#   }
# 
#   if (!is.numeric(sleep)) {
# 
#     usethis::ui_stop(
#       stick(
#         "
#           Argument
#           {usethis::ui_field('sleep')}
#           must be a
#           {usethis::ui_value('numeric of length 1')}
#         "
#       )
#     )
#   }
# 
# 
# 
#   env <- new.env()
#   env$openvpn           <- openvpn
#   env$agent             <- agent
#   env$verbose           <- verbose
# 
#   check_boolean("openvpn", env)
#   check_boolean("agent", env)
#   check_boolean("verbose", env)
# 
# 
#   if (is.null(search_terms)) {
# 
#     usethis::ui_stop(
#       stick(
#         "You must provide a term (or an expression) to search for"
#       )
#     )
#   }
# 
#   env$search_terms <- search_terms
#   check_string("search_terms", env)
# 
#   search_terms <- escape_url(search_terms)
# 
# 
#   if (missing(ovpn_country)) {
# 
#     ovpn_country <- NULL
#   }
# 
# 
# 
#   ### Welcome message                                                           ----------
# 
# 
# 
#   if (verbose) {
# 
#     cli::cat_line()
#     cli::cat_rule(left = "Scraping Google Images", line_col = "darkgrey")
#     cli::cat_line()
# 
#     cli::cat_line(crayon::underline("Request details"))
# 
# 
#     terms <- usethis::ui_value(gsub('%20', ' ', search_terms))
#     terms <- paste(terms, "(keywords)")
# 
#     terms <- paste0(" Searching for ", terms)
# 
#     usethis::ui_info(terms)
# 
#   }
# 
# 
# 
#   ### Write GI request                                                          ----------
# 
# 
# 
#   url <- paste0(
#     "https://www.google.com/search",      # URL Root
#     "?q=",                                # Exact terms (in same order)
#     search_terms,
#     "&source=lnms",
#     "&tbm=isch",
#     "&safe=images",
#     "&hl=en"                             # Interface Language
#   )
# 
# 
# 
#   Sys.sleep(sleep)
# 
# 
# 
#   ### Open URL in Browser                                                       ----------
# 
# 
# 
#   rs_driver$client$navigate(url)
# 
# 
# 
#   ### Check for Ban                                                             ----------
# 
# 
# 
#   k       <- 1
#   captcha <- rs_driver$client$findElements(using = "id", value = "gs_captcha_c")
# 
#   if (!length(captcha)) {
# 
#     captcha <- rs_driver$client$findElements(using = "id", value = "recaptcha")
# 
#   }
# 
#   if (!length(captcha)) {
# 
#     captcha <- rs_driver$client$findElement(using = "tag", value = "div")
#     captcha <- grep("\\\nip addr", tolower(captcha$getElementText()))
#   }
# 
# 
# 
#   while (length(captcha)) {
# 
# 
# 
#     Sys.sleep(sleep)
# 
# 
# 
#     ### Ban messages                                                            ----------
# 
# 
# 
#     if (!openvpn) {
# 
#       usethis::ui_stop(
#         stick("You have been banned from Google Images")
#       )
# 
#     } else {
# 
#       if (k == 1) {
# 
#         if (verbose) {
# 
#           cli::cat_line()
#           cli::cat_line()
#           cli::cat_line(crayon::underline("Google Images ban"))
#         }
#       }
# 
#       usethis::ui_todo(
#         stick(
#           "
#             Trying to connect to another server.
#             Attempt
#             {usethis::ui_value(k)}
#           ",
#           indent = " "
#         )
#       )
# 
# 
# 
#       ### Change IP address                                                     ----------
# 
# 
#       close_vpn(verbose = FALSE)
# 
#       invisible(
#         change_ip(
#           config_path  = config_path,
#           exposed_ip   = exposed_ip,
#           country      = ovpn_country,
#           ignore_files = NULL,
#           verbose      = verbose
#         )
#       )
# 
# 
# 
#       rs_driver$client$closeWindow()
# 
#       Sys.sleep(2)
# 
#       rs_driver$client$open(silent = TRUE)
# 
#       rs_driver$client$navigate(url)
# 
# 
#       k <- k + 1
# 
# 
#       rs_driver$client$refresh()
# 
#       captcha <- rs_driver$client$findElements(using = "id", value = "gs_captcha_c")
# 
#       if (!length(captcha)) {
# 
#         captcha <- rs_driver$client$findElements(using = "id", value = "recaptcha")
# 
#       }
# 
#       if (!length(captcha)) {
# 
#         captcha <- rs_driver$client$findElement(using = "tag", value = "div")
#         captcha <- grep("\\\nip addr", tolower(captcha$getElementText()))
# 
#       }
# 
# 
# 
#       ### Prevent infinite loop                                                 ----------
# 
# 
# 
#       if (k > 10) {
# 
#         usethis::ui_stop(
#           stick("You have been permanently banned from Google Images")
#         )
#       }
# 
#     }  # e_o while openvpn
#   } # e_o while ban
# 
# 
# 
# 
#   ### Scroll Down                                                               ----------
# 
# 
#   thumb_links <- rs_driver$client$findElements(using = "class", value = "rg_i")
#   new_links   <- TRUE
# 
#   while (length(thumb_links) < (n_max + start) && new_links) {
# 
#     scroll_down <- rs_driver$client$findElement(using = "css", value = "body")
#     scroll_down$sendKeysToElement(list(key = "end"))
# 
#     previous_links <- thumb_links
# 
#     Sys.sleep(0.75)
# 
#     thumb_links <- rs_driver$client$findElements(using = "class", value = "rg_i")
# 
#     if (length(thumb_links) == length(previous_links)) {
# 
#       if (length(thumb_links) < (n_max + start)) {
# 
#         button <- rs_driver$client$findElements(using = 'tag', value = "input")
#         button[[4]]$clickElement()
# 
#         Sys.sleep(0.75)
# 
#         scroll_down <- rs_driver$client$findElement(using = "css", value = "body")
#         scroll_down$sendKeysToElement(list(key = "end"))
# 
#         Sys.sleep(0.75)
# 
#         previous_links <- thumb_links
# 
#         thumb_links <- rs_driver$client$findElements(using = "class", value = "rg_i")
# 
#         if (length(thumb_links) == length(previous_links)) {
# 
#           new_links <- FALSE
#         }
# 
#       } else {
# 
#         new_links <- FALSE
# 
#       }
#     }
#   }
# 
# 
# 
#   ### Get Thumbnails URL                                                        ----------
# 
# 
#   thumb_links <- rs_driver$client$findElements(using = "class", value = "rg_i")
# 
#   if (length(thumb_links) >= (n_max + start)) {
# 
#     thumb_links <- thumb_links[(start + 1):(n_max + start)]
# 
#   } else {
# 
#     thumb_links <- thumb_links[(start + 1):length(thumb_links)]
#   }
# 
# 
#   if (verbose) {
# 
#     cli::cat_line()
#     cli::cat_line(crayon::underline("Response details"))
#     usethis::ui_info(
#       stick(
#         "
#           Number of images:
#           {usethis::ui_value(length(thumb_links))}
#         ",
#         indent = " "
#       )
#     )
#   }
# 
# 
#   photo_id <- as.numeric(format(Sys.time(), "%Y%m%d%H%M%S"))
#   count    <- 0
# 
#   gi_results <- data.frame()
# 
#   for (k in 1:length(thumb_links)) {
# 
# 
# 
#     ### Go to original image                                                    ----------
# 
# 
#     thumb_links[[k]]$clickElement()
# 
#     Sys.sleep(sample(seq(0, 2, by = 0.01), 1))
# 
#     session  <- rs_driver$client$getPageSource()[[1]]
#     session  <- xml2::read_html(session)
#     img_link <- rvest::html_nodes(session, "img")
#     img_link <- rvest::html_attr(img_link, "src")
# 
#     img_link <- img_link[grep("^http", img_link)]
# 
#     pos <- grep("encrypted-tbn0", img_link)
#     if (length(pos)) {
#       img_link <- img_link[-pos]
#     }
# 
# 
#     if (length(img_link)) {
# 
#       ### Download original image                                                 ----------
# 
#       if (download) {
# 
#         attempt <- tryCatch({
#           utils::download.file(
#             url       = img_link,
#             destfile  = file.path(output_path, paste0("IMG", photo_id, ".jpg")),
#             quiet     = TRUE
#           )},
#           error = function(e){}
#         )
#       }
# 
#       dat <- data.frame(
#         species  = strsplit(output_path, "/")[[1]][length(strsplit(output_path, "/")[[1]])],
#         query    = search_terms,
#         photo_id = paste0("IMG", photo_id),
#         url      = img_link
#       )
#       gi_results <- rbind(gi_results, dat)
# 
#       count    <- count + 1
#       photo_id <- photo_id + 1
#     }
#   }
# 
#   if (verbose) {
# 
#     usethis::ui_info(
#       stick(
#         "
#           Images successfully downloaded:
#           {usethis::ui_value(paste0(count, \" on \", length(thumb_links)))}
#         ",
#         indent = " "
#       )
#     )
#   }
# 
#   save(gi_results, file = file.path(output_path, paste0(format(Sys.time(), "%Y%m%d%H%M"), ".rda")))
# 
#   rs_driver$client$closeWindow()
# 
#   cli::cat_line()
#   cli::cat_rule(right = "done.", col = "darkgrey")
# 
#   return(rs_driver)
# 
# }



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
#' @noRd

# scrap_gsearch <- function(
    #   search_terms, port = 4567L, openvpn = TRUE, rs_driver = NULL,
#   config_path = "~/.ovpn", ovpn_country, agent = TRUE, sleep = 1,
#   verbose = TRUE, output_path = ".", exposed_ip = NULL
# ) {
# 
# 
# 
#   ### Parameters checks                                                         ----------
# 
# 
#   if (is.null(sleep)) {
# 
#     sleep <- 0
#   }
# 
#   if (length(sleep) != 1) {
# 
#     usethis::ui_stop(
#       stick(
#         "
#           Argument
#           {usethis::ui_field('sleep')}
#           must be a
#           {usethis::ui_value('numeric of length 1')}
#         "
#       )
#     )
#   }
# 
#   if (!is.numeric(sleep)) {
# 
#     usethis::ui_stop(
#       stick(
#         "
#           Argument
#           {usethis::ui_field('sleep')}
#           must be a
#           {usethis::ui_value('numeric of length 1')}
#         "
#       )
#     )
#   }
# 
# 
#     if (is.null(port)) {
# 
#       port <- 4567L
#     }
# 
#     if (length(port) != 1) {
# 
#       usethis::ui_stop(
#         stick(
#           "
#             Argument
#             {usethis::ui_field('port')}
#             must be a
#             {usethis::ui_value('numeric of length 1')}
#           "
#         )
#       )
#     }
# 
#     if (!is.numeric(port)) {
# 
#       usethis::ui_stop(
#         stick(
#           "
#             Argument
#             {usethis::ui_field('port')}
#             must be a
#             {usethis::ui_value('numeric of length 1')}
#           "
#         )
#       )
#     }
# 
# 
#   env <- new.env()
#   env$openvpn           <- openvpn
#   env$agent             <- agent
#   env$verbose           <- verbose
# 
#   check_boolean("openvpn", env)
#   check_boolean("agent", env)
#   check_boolean("verbose", env)
# 
# 
#   if (is.null(search_terms)) {
# 
#     usethis::ui_stop(
#       stick(
#         "You must provide a term (or an expression) to search for"
#       )
#     )
#   }
# 
#   env$search_terms <- search_terms
#   check_string("search_terms", env)
# 
#   original_terms <- search_terms
#   original_terms <- strsplit(original_terms, " OR ")[[1]][1]
#   original_terms <- gsub("'", "", original_terms)
# 
#   search_terms   <- escape_url2(search_terms)
# 
# 
#   if (missing(ovpn_country)) {
# 
#     ovpn_country <- NULL
#   }
# 
# 
# 
#   ### Welcome message                                                           ----------
# 
# 
# 
#   if (verbose) {
# 
#     cli::cat_line()
#     cli::cat_rule(left = "Scraping Google Search Engine", line_col = "darkgrey")
#     cli::cat_line()
# 
#     cli::cat_line(crayon::underline("Request details"))
# 
#     terms <- usethis::ui_value(original_terms)
#     terms <- paste(terms, "(keywords)")
#     terms <- paste0(" Searching for ", terms)
# 
#     usethis::ui_info(terms)
#   }
# 
# 
# 
#   ### Write GS request                                                          ----------
# 
# 
# 
#   url <- paste0(
#     "https://www.google.com/search",                 # URL Root
#     "?safe=active",                                  # Secure Search
#     "&hl=en",                                        # Interface Language
#     "&q=",                                           # Exact terms (in same order)
#     search_terms,
#     "&as_qdr=all"
#   )
# 
# 
# 
#   filename <- paste0(
#     tolower(gsub("\\s", "_", original_terms)),
#     "_",
#     format(Sys.time(), "%y%m%d%H%M%S")
#   )
# 
# 
# 
#   Sys.sleep(sleep)
# 
# 
# 
#   ### Open URL in Browser                                                     ----------
# 
# 
#   rs_driver$client$navigate(url)
# 
# 
# 
#   ### Check for Ban                                                         ----------
# 
# 
# 
#   k       <- 1
#   captcha <- rs_driver$client$findElements(using = "id", value = "gs_captcha_c")
# 
#   if (!length(captcha)) {
# 
#     captcha <- rs_driver$client$findElements(using = "id", value = "recaptcha")
# 
#   }
# 
#   if (!length(captcha)) {
# 
#     captcha <- rs_driver$client$findElement(using = "tag", value = "div")
#     captcha <- grep("\\\nip addr", tolower(captcha$getElementText()))
#   }
# 
# 
# 
#   while (length(captcha)) {
# 
# 
# 
#     Sys.sleep(sleep)
# 
# 
# 
#     ### Ban messages                                                            ----------
# 
# 
# 
#     if (!openvpn) {
# 
#       usethis::ui_stop(
#         stick("You have been banned from Google Scholar")
#       )
# 
#     } else {
# 
#       if (k == 1) {
# 
#         if (verbose) {
# 
#           cli::cat_line()
#           cli::cat_line()
#           cli::cat_line(crayon::underline("Google Search ban"))
#         }
#       }
# 
#       usethis::ui_todo(
#         stick(
#           "
#             Trying to connect to another server.
#             Attempt
#             {usethis::ui_value(k)}
#           ",
#           indent = " "
#         )
#       )
# 
# 
# 
#       ### Change IP address                                                     ----------
# 
# 
#       close_vpn(verbose = FALSE)
# 
#       invisible(
#         change_ip(
#           config_path  = config_path,
#           exposed_ip   = exposed_ip,
#           country      = ovpn_country,
#           ignore_files = NULL,
#           verbose      = verbose
#         )
#       )
# 
# 
# 
#       rs_driver$client$closeWindow()
# 
#       Sys.sleep(2)
# 
#       rs_driver$client$open(silent = TRUE)
# 
#       rs_driver$client$navigate(url)
# 
# 
#       k <- k + 1
# 
# 
#       rs_driver$client$refresh()
# 
#       captcha <- rs_driver$client$findElements(using = "id", value = "gs_captcha_c")
# 
#       if (!length(captcha)) {
# 
#         captcha <- rs_driver$client$findElements(using = "id", value = "recaptcha")
# 
#       }
# 
#       if (!length(captcha)) {
# 
#         captcha <- rs_driver$client$findElement(using = "tag", value = "div")
#         captcha <- grep("\\\nip addr", tolower(captcha$getElementText()))
# 
#       }
# 
# 
# 
#       ### Prevent infinite loop                                                 ----------
# 
# 
# 
#       if (k > 10) {
# 
#         usethis::ui_stop(
#           stick("You have been permanently banned from Google Scholar")
#         )
#       }
#     }  # e_o while openvpn
#   } # e_o while ban
# 
# 
# 
#   ### Check spelling                                                            ----------
# 
#   clicker <- rs_driver$client$findElements(using = "class", value = "spell_orig")
# 
#   if (length(clicker)) {
# 
#     clicker[[2]]$clickElement()
#   }
# 
# 
# 
#   ### Get Total Matches                                                         ----------
# 
# 
#   n_matches <- rs_driver$client$findElement(using = "id", value = "result-stats")
# 
#   total <- n_matches$getElementText()[[1]]
#   total <- gsub("\\(.+\\)|About|results|result|[[:space:]]|[[:punct:]]", "", total)
#   total <- as.numeric(total)
#   total <- ifelse(is.na(total), 0, total)
# 
# 
#   if (verbose) {
# 
#     cli::cat_line()
#     cli::cat_line(crayon::underline("Response details"))
#     usethis::ui_info(
#       stick(
#         "
#           Estimated number of results:
#           {usethis::ui_value(total)}
#         ",
#         indent = " "
#       )
#     )
#   }
# 
# 
# 
#   rs_driver$client$closeWindow()
# 
# 
# 
#   cli::cat_line()
#   cli::cat_rule(right = "done.", col = "darkgrey")
# 
# 
# 
#   dir.create(
#     file.path(
#       output_path,
#       tolower(gsub("\\s", "_", original_terms))
#     ),
#     showWarnings = FALSE,
#     recursive    = TRUE
#   )
# 
#   save(
#     total,
#     file = file.path(
#       output_path,
#       tolower(gsub("\\s", "_", original_terms)),
#       paste0(filename)
#     )
#   )
# 
#   return(rs_driver)
# 
# }



#' Escape whitespace in URL
#'
#' @description
#' Escapes all whitespaces in a URL.
#'
#' @param x a character of length 1. Typically an URL to encode.
#'
#' @return A escaped string.
#'
#' @noRd
#'
#' @examples
#' escape_url2("https://google.com/canis lupus")

# escape_url2 <- function(x) {
# 
#   if (missing(x)) {
#     stop("Argument 'x' is required", call. = FALSE)
#   }
#   
#   if (is.null(x)) {
#     stop("Argument 'x' is required", call. = FALSE)
#   }
#   
#   if (!is.character(x)) {
#     stop("Argument 'x' must be a character", call. = FALSE)
#   }
#   
#   if (length(x) != 1) {
#     stop("Argument 'x' must be a character of length 1", call. = FALSE)
#   }
#   
#   x <- gsub("[[:space:]]+", " ", x)
#   x <- gsub("^[[:space:]]|[[:space:]]$", "", x)
#   x <- gsub("[[:space:]]", "+", x)
#   
#   if (length(grep("\\+OR\\+", x))) {
#     
#     x <- paste0("&as_oq=", gsub("\\+OR\\+", "+", x))
#     
#   } else {
#     
#     x <- paste0("&as_epq=", x)
#     
#   }
#   
#   x <- gsub("'", "%22", x)
#   x <- gsub("[[:space:]]", "%20", x)
# 
#   x
# }
