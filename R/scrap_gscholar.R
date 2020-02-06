#' @title Retrieve Data from Google Scholar
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
#' @param exact A boolen. If TRUE, searchs for the exact terms, otherwise searchs at least one of the terms.
#' @param exclude_terms A character vector of terms to exclude from the search.
#' @param search_author A character vector of authors to search for.
#' @param search_source A character vector of publication sources to search for.
#' @param metadata ...
#' @param where Search in the whole document ('any') or only in the title ('title').
#' @param years A vector of 1 or 2 years specifying the temporal extent of the search.
#' @param lang The ISO-2 code of the language to search for. Use \code{get_languages()} to get a list.
#' @param start A numeric specifying the number of the first results from which the results are extracted.
#' @param n_max A numeric specifying the number of results to extract.
#' @param include_patents A boolean. If TRUE, patents are included in the search results.
#' @param include_citations A boolean. If TRUE, citations are included in the search results.
#' @param selenium A boolean. If TRUE, webscraping is performed with the Selenium technology.
#' @param port The port number to run on Selenium server.
#' @param openvpn A boolean. If TRUE, public IP address will be randomly changed.
#' @param config_path The path to the folder containing server configuration files.
#' @param ovpn_country The ISO-2 code of the country to pick up a server. Use \code{get_countries()} to get a list.
#' @param agent A boolean. If TRUE, browser user agent will be randomly changed.
#' @param sleep The time interval (in seconds) between two sub-requests.
#' @param verbose A boolean. If TRUE, connexion and webscraping informations are printing.
#'
#' @return A 7-columns data frame with:
#'   - query: the query terms
#'   - gsid: the publication Google Scholar ID
#'   - title: the publication title
#'   - authors: the publication authors
#'   - source: the publication source
#'   - year: the publication year
#'   - citation: the number of citations of the publication
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
#' scrap_gscholar()
#' }



scrap_gscholar <- function(
  search_terms, exact = TRUE, exclude_terms = NULL, search_author = NULL,
  search_source = NULL, metadata = FALSE, where = NULL, years = NULL,
  lang = NULL, start = 0, n_max = NULL, include_patents = FALSE,
  include_citations = FALSE, selenium = FALSE, port = 4567L, openvpn = TRUE,
  config_path = "~/.ovpn", ovpn_country, agent = TRUE, sleep = 1,
  verbose = TRUE
) {


  ### Parameters checks   ----------

  if (is.null(where)) {
    where <- "any"
  }

  where <- tolower(where)

  if (!(where %in% c("any", "title"))) {

    usethis::ui_stop(
      stick(
        "
          You must choose
          {usethis::ui_field('where')}
          among
          {usethis::ui_value('any')}
          or
          {usethis::ui_value('title')}
        "
      )
    )
  }

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


    if (is.null(port)) {

      port <- 0
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


  if (!is.null(lang)) {

    lang <- tolower(lang)

    if (!(lang %in% languages[ , "iso_2"])) {

      usethis::ui_stop(
        stick(
          "
            Unable to find the language
            {usethis::ui_value('lang')}.
            Run
            {usethis::ui_code('get_languages()')}
            to select an appropriate language (ISO-2)
          "
        )
      )

    } else {

      lang <- paste0("lang_", lang)

    }

  } else {

    lang <- ""
  }

  if (!is.null(years)) {

    if (length(years) == 1) {

      if (!is.numeric(years)) {

        usethis::ui_stop(
          stick(
            "
              Argument
              {usethis::ui_field('years')}
              must be a
              {usethis::ui_value('numeric of length 1 or 2')}
            "
          )
        )

      } else {

        years <- rep(years, 2)
      }

    } else {

      if (length(years) > 2) {

        usethis::ui_stop(
          stick(
            "
              Argument
              {usethis::ui_field('years')}
              must be a
              {usethis::ui_value('numeric of length 1 or 2')}
            "
          )
        )

      } else {

        if (!is.numeric(years)) {

          usethis::ui_stop(
            stick(
              "
                Argument
                {usethis::ui_field('years')}
                must be a
                {usethis::ui_value('numeric of length 1 or 2')}
              "
            )
          )

        } else {

          years <- sort(years)
        }
      }
    }

  } else {

    years <- rep("", 2)
  }

  env <- new.env()
  env$exact             <- exact
  env$metadata          <- metadata
  env$selenium          <- selenium
  env$openvpn           <- openvpn
  env$agent             <- agent
  env$verbose           <- verbose
  env$include_patents   <- include_patents
  env$include_citations <- include_citations

  check_boolean("exact", env)
  check_boolean("metadata", env)
  check_boolean("selenium", env)
  check_boolean("openvpn", env)
  check_boolean("agent", env)
  check_boolean("verbose", env)
  check_boolean("include_patents", env)
  check_boolean("include_citations", env)


  if (
    is.null(search_terms) &&
    is.null(search_author) &&
    is.null(search_source)) {

    usethis::ui_stop(
      stick(
        "You must provide a term (or an expression) to search for"
      )
    )
  }

  if (!is.null(search_terms)) {

    env$search_terms <- search_terms
    check_string("search_terms", env)

    search_terms <- escape_url(search_terms)

  } else {

    search_terms <- ""
  }

  if (!is.null(search_author)) {

    env$search_author <- search_author
    check_string("search_author", env)

    search_author <- escape_url(search_author)

  } else {

    search_author <- ""
  }

  if (!is.null(search_source)) {

    env$search_source <- search_source
    check_string("search_source", env)

    search_source <- escape_url(search_source)

  } else {

    search_source <- ""
  }

  if (!is.null(exclude_terms)) {

    env$exclude_terms <- exclude_terms
    check_string("exclude_terms", env)

    exclude_terms <- escape_url(exclude_terms)

  } else {

    exclude_terms <- ""
  }


  if (missing(ovpn_country)) {

    ovpn_country <- NULL
  }

  if (verbose) {

    cli::cat_line()
    cli::cat_rule(left = "Scraping Google Scholar", line_col = "darkgrey")
    cli::cat_line()

    cli::cat_line(crayon::underline("Request details"))

    if (search_terms != "") {

      term_1 <- usethis::ui_value(gsub('%20', ' ', search_terms))
      term_1 <- paste(term_1, "(keywords)")

    } else {

      term_1 <- NULL
    }

    if (search_author != "") {

      term_2 <- usethis::ui_value(gsub('%20', ' ', search_author))
      term_2 <- paste(term_2, "(author)")

    } else {

      term_2 <- NULL
    }

    if (search_source != "") {

      term_3 <- usethis::ui_value(gsub('%20', ' ', search_source))
      term_3 <- paste(term_3, "(journal)")

    } else {

      term_3 <- NULL
    }

    terms <- paste0(c(term_1, term_2, term_3), collapse = " and ")
    terms <- paste0(" Searching for ", terms)

    usethis::ui_info(terms)



    term_0 <- " Searching"

    if (where == "any") {

      term_1 <- "in"
      term_2 <- usethis::ui_value('whole')
      term_3 <- "document"

    } else {

      term_1 <- "only in"
      term_2 <- usethis::ui_value('title')
      term_3 <- NULL
    }

    if (exact) {

      term_4 <- "with"
      term_5 <- usethis::ui_value('exact words')
      term_6 <- "matching"

    } else {

      term_4 <- "with"
      term_5 <- usethis::ui_value('at least one word')
      term_6 <- "matching"
    }

    terms <- paste0(
      c(
        term_0, term_1, term_2, term_3,
        term_4, term_5, term_6
      ),
      collapse = " "
    )

    usethis::ui_info(terms)

    term_0 <- " For the period:"

    if (years[1] == "") {
        terms <- usethis::ui_value('all times')
    } else {
      terms <- unique(years)
      terms <- paste0(years, collapse = "-")
      terms <- usethis::ui_value(terms)
    }

    usethis::ui_info(paste(term_0, terms))

  }


  ### Change IP address   ----------

  if (openvpn) {

    if (verbose) {

      cli::cat_line()
      cli::cat_line(crayon::underline("Activating VPN protection"))
    }

    close_vpn(verbose = FALSE)
    exposed_ip  <- get_ip()

    usethis::ui_info(
      stick(
        paste0(
          "Unprotected public IP address: ",
          usethis::ui_value(get_ip())
        ),
        indent = " "
      )
    )

    vpn_servers <- change_ip(
      config_path  = config_path,
      exposed_ip   = exposed_ip,
      country      = ovpn_country,
      ignore_files = NULL,
      verbose      = verbose
    )

  } else {

    if (verbose) {

      cli::cat_line()
      cli::cat_line(crayon::underline("No VPN protection"))
      usethis::ui_info(
        stick(
          "You'll probably get blocked by Google Scholar",
          indent = " "
        )
      )
      usethis::ui_info(
        stick(
          paste0(
            "Unprotected public IP address: ",
            usethis::ui_value(get_ip())
          ),
          indent = " "
        )
      )
    }
  }


  ### Change User Agent                                                         ----------

  if (agent) {

    if (verbose) {
      cli::cat_line()
      cli::cat_line(crayon::underline("Changing user-agent"))
    }
    uagent <- change_ua(verbose = verbose)
  }


  ### Start RSelenium server                                                    ----------

  if (selenium) {

    if (agent) {

      rs_driver <- rsDriver(
        port              = port,
        browser           = "firefox",
        verbose           = FALSE,
        extraCapabilities = makeFirefoxProfile(
          list(
            general.useragent.override = uagent
          )
        )
      )

    } else {

      rs_driver <- rsDriver(
        port    = port,
        browser = "firefox",
        verbose = FALSE
      )
    }

    rs_client <- rs_driver$client
    closed    <- FALSE
  }


  next_btn   <- vector("list", 1)        # Next Page Button (used in while loop)
  gs_results <- data.frame()             # Results Storage
  k          <- 1
  display    <- TRUE


  while (length(next_btn)) {


    ### Write GS request                                                        ----------

    url <- paste0(
      "https://scholar.google.com/scholar?as_q=",      # URL Root
      "&safe=active",                                  # Secure Search
      "&btnG=Search+Scholar",                          # Search Engine
      "&hl=en",                                        # Interface Language
      "&as_epq=",                                      # Exact terms (in same order)
      "%22", ifelse( exact, search_terms, ""), "%22",
      "&as_oq=",                                       # At least one of these terms
      ifelse(!exact, search_terms, ""),
      "&as_eq=",                                       # Exclude these terms
      "%22", exclude_terms, "%22",
      "&as_sauthors=",                                 # Search by author
      "%22", search_author, "%22",
      "&as_publication=",                              # Search by publication source
      "%22", search_source, "%22",
      "&start=",                                       # Number of first article
      start,
      "&lr=",                                          # Language to search for
      lang,
      "&as_occt=",                                     # Search in whole document or title
      where,
      "&as_sdt=",                                      # Include or remove Patents
      ifelse(include_patents, 0, 1),
      "&as_vis=",                                      # Include or remove Citations
      ifelse(include_citations, 0, 1),
      "&as_ylo=",                                      # Starting year (included)
      years[1],
      "&as_yhi=",                                      # Ending year (included)
      years[2]
    )

    url <- gsub("&[a-z]{1,}_[a-z]{1,}=%22%22", "", url)
    url <- gsub("&[a-z]{1,}_[a-z]{1,}=&", "&", url)
    url <- gsub("&[a-z]{1,}=&", "&", url)
    url <- gsub("&[a-z]{1,}_[a-z]{1,}=$", "", url)


    Sys.sleep(sleep)


    ### Open URL in Browser                                                     ----------

    if (agent) {

      session <- GET(url, user_agent(uagent))

    } else  {

      session <- GET(url)

    }



    ### Check for Ban                                                           ----------

    while (session$status_code != 200) { # 429

      Sys.sleep(sleep)

      if (!openvpn) {

        usethis::ui_stop(
          stick("You have been banned from Google Scholar")
        )

      } else {

        if (k == 1) {

          if (verbose) {

            cli::cat_line()
            cli::cat_line()
            cli::cat_line(crayon::underline("Google Scholar ban"))
          }
        }

        usethis::ui_todo(
          stick(
            "
              Trying to connect to another server.
              Attempt number
              {usethis::ui_value(k)}
            ",
            indent = " "
          )
        )

        close_vpn(verbose = FALSE)

        vpn_servers <- c(
          vpn_servers,
          change_ip(
            config_path  = config_path,
            exposed_ip   = exposed_ip,
            country      = ovpn_country,
            ignore_files = vpn_servers,
            verbose      = verbose
          )
        )
        k <- k + 1
      }


      if (selenium) {

        rs_client$close()
        invisible(rs_driver$server$stop())
        closed <- TRUE
      }

      if (agent) {

        session <- GET(url, user_agent(uagent))

      } else  {

        session <- GET(url)
      }
    }


    if (selenium) {

      if (closed) {

        if (agent) {

          rs_driver <- rsDriver(
            port              = port,
            browser           = "firefox",
            verbose           = FALSE,
            extraCapabilities = makeFirefoxProfile(
              list(
                general.useragent.override = change_ua(verbose)
              )
            )
          )

        } else {

          rs_driver <- rsDriver(
            port    = port,
            browser = "firefox",
            verbose = FALSE
          )
        }

        rs_client <- rs_driver$client
      }

      rs_client$navigate(url)
      session <- rs_client$getPageSource()[[1]]
    }

    session <- xml2::read_html(session)


    ### Extract METADATA                                                        ----------

    if (length(html_nodes(session, ".gs_ri"))) {

      if (!nrow(gs_results)) {

        total <- html_text(html_nodes(session, "#gs_ab_md"))
        total <- gsub("\\(.+\\)|About|results|result|[[:space:]]|[[:punct:]]", "", total)
        total <- as.numeric(total)
      }

      if (verbose && display) {

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

        if (metadata) {

          if (
            (total > 999 && is.null(n_max)) ||
            (total > 999 && n_max > 999)
          ) {

            usethis::ui_todo(
              stick(
                "Only the first 999 results will be extracted",
                indent = " "
              )
            )
            usethis::ui_line(
              stick(
                "
                  You may shorten the search period with the argument
                  {usethis::ui_field('years')}
                ",
                indent = "   "
              )
            )
          }

          cli::cat_line()
          cli::cat_line(crayon::underline("Metadata extraction"))

        } else {

          cli::cat_line()
          cli::cat_line(crayon::underline("No metadata extraction"))
        }
        display <- FALSE
      }


      if (metadata) {

        gs_blocks <- html_nodes(session, ".gs_ri")


        gs_titles <- html_text(html_nodes(gs_blocks, "h3"))
        gs_titles <- gsub("\\[[[:alpha:]]+\\]", "", gs_titles)
        gs_titles <- gsub("^[[:space:]]|[[:space:]]$", "", gs_titles)


        gs_citations <- html_nodes(gs_blocks, ".gs_fl")

        gs_citations <- unlist(
          lapply(
            gs_citations,
            function(x) {
              x <- html_nodes(x, "a")
              x <- html_text(x)
              pos <- grep("Cited by", x)
              if (length(pos)) {
                return(as.numeric(gsub("Cited by", "",  x[pos])))
              } else {
                return(0)
              }
            }
          )
        )


        gs_infos <- html_text(html_nodes(gs_blocks, ".gs_a"))


        gs_links <- unlist(
          lapply(
            gs_blocks,
            function(x) {
              x <- html_nodes(x, "h3")
              x <- html_nodes(x, "a")
              return(html_attr(x, "href"))
            }
          )
        )


        gs_infos <- data.frame(
          query    = search_terms,
          title    = gs_titles,
          infos    = gs_infos,
          citation = gs_citations,
          links    = gs_links
        )

      } else {

        gs_infos <- data.frame()
      }

    } else {

      total    <- 0
      gs_infos <- data.frame()

    }

    gs_results <- rbind(gs_results, gs_infos)


    if (verbose) {

      cat(
        paste0(
          "\r",
          "   Scraping references ",
          nrow(gs_results),
          " on ",
          total,
          "..."
        )
      )
    }


    ### Check for next pages                                                    ----------

    if (!length(gs_results)) {

      next_btn <- NULL

    } else {

      if (is.null(n_max)) {

        pages <- html_table(session)[[1]]
        pages <- as.character(pages[1, ])
        pages <- as.numeric(pages[which(!(pages %in% c("Previous", "Next")))])

        page <- (start / 10) + 1

        if (sum(pages > page) > 0) {

          start <- start + 10

        } else {

          next_btn <- NULL
        }

      } else {

        if (nrow(gs_results) < n_max) {

          pages <- html_table(session)[[1]]
          pages <- as.character(pages[1, ])
          pages <- as.numeric(pages[which(!(pages %in% c("Previous", "Next")))])

          page <- (start / 10) + 1

          if (sum(pages > page) > 0) {

            start <- start + 10

          } else {

            next_btn <- NULL
          }

        } else {

          gs_results <- gs_results[1:n_max, ]
          next_btn <- NULL
        }
      }
    }
  }


  if (selenium) {

    rs_client$close()
    invisible(rs_driver$server$stop())
  }

  if (openvpn) {

    if (verbose) {

      cli::cat_line()
      cli::cat_line()
      cli::cat_line(crayon::underline("Stopping VPN protection"))
    }

    close_vpn(verbose)
  }

  cli::cat_line()
  cli::cat_rule(right = "done.", col = "darkgrey")

  if (metadata) {

    return(gs_results)

  } else {

    return(total)
  }
}
