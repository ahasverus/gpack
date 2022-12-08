#' Retrieve metadata from Google Scholar
#'
#' @description
#' Sends a request to Google Scholar service and retrieves results (title, 
#' authors, source and year of publications, and the total number of citations).
#' 
#' As no API is provided by Google Scholar (except the one for authors with a 
#' Google Scholar ID), this function scraps the service using the package 
#' [`RSelenium`].
#' 
#' To bypass Google IP bans, the IP address and the User agent will be changed
#' in case of ban.
#'
#' @param search_terms a `character` of length 1. Terms to search papers for
#'   (optional).
#' 
#' @param exact a `logical`. If `TRUE`, search for the exact terms, otherwise 
#'   search at least one of the terms.
#'   
#' @param exclude_terms a `character` of length 1. Terms to exclude from the 
#'   search (optional).
#'   
#' @param search_author a `character` of length 1. Authors to search for 
#'   (optional).
#' 
#' @param search_source a `character` of length 1. Publication sources to 
#'   search for (optional).
#'   
#' @param metadata a `logical`. If `TRUE`, all publications data are extracted.
#'   Otherwise, only the total number of publications is returned.
#'   
#' @param where a `character` of length 1. One among `'any'` (search in the 
#'   whole document) or `'title'` (search only in the title).
#'   
#' @param years a `integer` of length 1 or 2. Year(s) specifying the temporal 
#'   extent of the search.
#'   
#' @param lang a `character` of length 1. The ISO-2 code of the language to 
#'   search for. Use [get_languages()] to get a list (optional).
#'   
#' @param start a `numeric` of length 1. The number of the first results from 
#'   which the results are extracted (default is `0`, start from the first 
#'   result).
#'   
#' @param n_max a `numeric` of length 1. The number of results to extract.
#' 
#' @param include_patents a `logical`. If `TRUE`, patents are included in the 
#'   search results.
#' 
#' @param include_citations a `logical`. If `TRUE`, citations are included in 
#'   the search results.
#'   
#' @param ovpn_country a `character` vector. The ISO-2 code of the country to 
#'   pick up a VPN server. Use [get_countries()] to get a list.
#'   
#' @param agent a `logical`. If `TRUE`, web browser user agent will be randomly
#'   changed.
#'   
#' @param verbose a `logical`. If `TRUE`, connection and scraping information 
#'   are printing.
#'   
#' @param keep_html a `logical`. If `TRUE`, raw HTML pages are kept.
#' 
#' @param output_path a `character` of length 1. The path to the folder to 
#'   save data.
#'
#' @return No return value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' scrap_gscholar()
#' }

scrap_gscholar <- function(search_terms, exact = TRUE, exclude_terms = NULL, 
                           search_author = NULL, search_source = NULL, 
                           metadata = FALSE, where = NULL, years = NULL,
                           lang = NULL, start = 0, n_max = NULL, 
                           include_patents = FALSE, include_citations = FALSE,
                           ovpn_country, agent = TRUE, verbose = TRUE, 
                           keep_html = FALSE, output_path = ".") {


  ## Check system ----

  check_system(verbose)

  
  ## Check 'output_path' arg ----
  
  is_character(output_path)
  
  if (!dir.exists(output_path)) {
    stop("The path '", output_path, "' does not exist", call. = FALSE)
  }
  
  
  ## Check 'where' arg ----

  if (is.null(where)) {
    where <- "any"
  }

  is_character(where)
  where <- tolower(where)

  if (!(where %in% c("any", "title"))) {
    stop("Argument 'where' must be one of 'any' or 'title'", call. = FALSE)
  }
  
  
  ## Check 'start' arg ----

  if (is.null(start)) {
    start <- 0
  }
  
  is_numeric(start)


  ## Check 'n_max' arg ----
  
  if (!is.null(n_max)) {
    is_numeric(n_max)
  }
  

  ## Check 'lang' arg ----
  
  if (!is.null(lang)) {
    
    is_character(lang)

    lang <- tolower(lang)

    if (!(lang %in% languages$"iso_2")) {
      stop("Language (argument 'lang') unavailable. Run `get_languages()` to ", 
           "select an appropriate language (ISO-2)", call. = FALSE)
    } else {
      lang <- paste0("lang_", lang)
    }
    
  } else {
    lang <- ""
  }

  
  ## Check 'years' arg ----
  
  if (!is.null(years)) {

    if (length(years) == 1) {
      
      is_numeric(years)
      years <- rep(years, 2)

    } else {

      if (length(years) > 2) {

        stop("Argument 'years' must be of length < 3", call. = FALSE)

      } else {

        is_numerics(years)
        years <- sort(years)
      }
    }

  } else {

    years <- rep("", 2)
  }

  
  ## Check boolean args ----
  
  is_logical(exact)
  is_logical(metadata)
  is_logical(agent)
  is_logical(verbose)
  is_logical(include_patents)
  is_logical(include_citations)
  is_logical(keep_html)


  ## Check search terms ----
  
  if (missing(search_terms)) {
    search_terms <- NULL
  }
  
  if (is.null(search_terms) && is.null(search_author) && 
      is.null(search_source)) {
    stop("You must provide a term (or an expression) to search for ",
         "arguments 'search_terms' or 'search_author' or 'search_source'", 
         call. = FALSE)
  }

  if (!is.null(search_terms)) {
    is_character(search_terms)
    search_terms <- escape_url(search_terms)
  } else {
    search_terms <- ""
  }

  if (!is.null(search_author)) {
    is_character(search_author)
    search_author <- escape_url(search_author)
  } else {
    search_author <- ""
  }

  if (!is.null(search_source)) {
    is_character(search_source)
    search_source <- escape_url(search_source)
  } else {
    search_source <- ""
  }

  
  ## Check exclude terms ----
  
  if (!is.null(exclude_terms)) {
    is_character(exclude_terms)
    exclude_terms <- escape_url(exclude_terms)
  } else {
    exclude_terms <- ""
  }


  ## Check 'ovpn_country' terms -----
  
  if (missing(ovpn_country)) {
    ovpn_country <- NULL
  }


  ### Welcome message                                                           ----------



  if (verbose) {

    messages::msg_line()
    messages::msg_rule(left = "Scraping Google Scholar", line_col = "darkgrey")
    messages::msg_line()

    messages::msg_line(cli::style_underline("Request details"))

    if (search_terms != "") {

      term_1 <- messages::msg_value(gsub('%20', ' ', search_terms))
      term_1 <- paste(term_1, "(keywords)")

    } else {

      term_1 <- NULL
    }

    if (search_author != "") {

      term_2 <- messages::msg_value(gsub('%20', ' ', search_author))
      term_2 <- paste(term_2, "(author)")

    } else {

      term_2 <- NULL
    }

    if (search_source != "") {

      term_3 <- messages::msg_value(gsub('%20', ' ', search_source))
      term_3 <- paste(term_3, "(journal)")

    } else {

      term_3 <- NULL
    }

    terms <- paste0(c(term_1, term_2, term_3), collapse = " and ")
    terms <- paste0(" Searching for ", terms)

    messages::msg_info(terms)

    term_0 <- " Searching"

    if (where == "any") {

      term_1 <- "in"
      term_2 <- messages::msg_value('whole')
      term_3 <- "document"

    } else {

      term_1 <- "only in"
      term_2 <- messages::msg_value('title')
      term_3 <- NULL
    }

    if (exact) {

      term_4 <- "with"
      term_5 <- messages::msg_value('exact words')
      term_6 <- "matching"

    } else {

      term_4 <- "with"
      term_5 <- messages::msg_value('at least one word')
      term_6 <- "matching"
    }

    terms <- paste0(c(term_0, term_1, term_2, term_3, term_4, term_5, term_6),
                    collapse = " ")

    messages::msg_info(terms)

    term_0 <- " For the period:"

    if (years[1] == "") {
        terms <- messages::msg_value('all times')
    } else {
      terms <- unique(years)
      terms <- paste0(years, collapse = "-")
      terms <- messages::msg_value(terms)
    }

    messages::msg_info(paste(term_0, terms))
  }
  
  
  ## Start RSelenium server ----
  
  rs_driver <- start_selenium(output_path, agent = agent)
  
  
  ## Change IP address ----

  if (verbose) {

    messages::msg_line()
    messages::msg_line(crayon::underline("Activating VPN protection"))
  }

  vpn_servers <- change_ip(rs_driver, country = ovpn_country, 
                           ignore_files = NULL, verbose = verbose)


  next_btn   <- vector("list", 1)        # Next Page Button (used in while loop)
  gs_results <- data.frame()             # Results Storage

  
  while (length(next_btn)) {

    
    ## Write GS request ----

    url <- write_gs_url(search_terms, search_author, search_source, 
                        exclude_terms, years, exact, start, where, lang, 
                        include_patents, include_citations)

    
    filename <- paste0(tolower(gsub("%20", "_", search_terms)), "_",
                       format(Sys.time(), "%y%m%d%H%M%S"))

    Sys.sleep(2)
    

    ## Open URL in Browser ----

    suppressMessages(rs_driver$navigate(url))

    
    ## Avoid Google Ban ----
    
    rs_driver <- avoid_ban(rs_driver, url, agent, ovpn_country, output_path, 
                           verbose)


    ## Get total matches ----

    if (!nrow(gs_results)) {

      session  <- rs_driver$getPageSource()[[1]]
      session  <- xml2::read_html(session)
      gs_block <- rvest::html_nodes(session, "#gs_ab_md")
      total    <- rvest::html_text(gs_block)
      
      total <- gsub("\\(.+\\)|About|results|result|[[:space:]]|[[:punct:]]", 
                    "", total)
      total <- suppressWarnings(as.numeric(total))
      total <- ifelse(is.na(total), 0, total)

      if (length(total) == 0) {
        
        cat(paste0(spname, " : ", paste0(years, collapse = "-"), "\n"), 
            file = "fails.txt", append = TRUE)
        end_of <- FALSE
        
      } else {
        
        if (total == 0) end_of <- TRUE else end_of <- FALSE
        
        if (metadata) {
          
          if ((total > 999 && is.null(n_max)) || (total > 999 && n_max > 999)) {
            
            cat(paste0(spname, " : ", paste0(years, collapse = "-"), "\n"), 
                file = "exceeds.txt", append = TRUE)
            # messages::msg_todo("Only the first 999 results will be extracted")
            # messages::msg_line("You may shorten the search period with the", 
            #                    "argument", messages::msg_field('years'))
          }
        }
      }

      if (verbose) {

        messages::msg_line()
        messages::msg_line(crayon::underline("Response details"))
        messages::msg_info("Estimated number of results:", 
                           messages::msg_value(total))
      }
    }


    if (!end_of) { # Results found

      ## Store HTML code source ----

      session  <- rs_driver$getPageSource()[[1]]

      if (keep_html) {

        html_dir <- file.path(output_path, tolower(gsub("%20", "_", 
                                                        search_terms)), "html")
        
        dir.create(html_dir, showWarnings = FALSE, recursive = TRUE)

        cat(session, file = file.path(html_dir, paste0(filename, ".html")))
      }


      ## Extract METADATA ----

      session <- xml2::read_html(session)

      if (metadata) {

        gs_blocks <- rvest::html_nodes(session, ".gs_ri")

        if (length(gs_blocks)) {

          gs_titles <- rvest::html_text(rvest::html_nodes(gs_blocks, "h3"))
          gs_titles <- gsub("\\[[[:alpha:]]+\\]", "", gs_titles)
          gs_titles <- gsub("^[[:space:]]|[[:space:]]$", "", gs_titles)

          gs_citations <- rvest::html_nodes(gs_blocks, ".gs_fl")

          gs_citations <- unlist(
            lapply(
              gs_citations,
              function(x) {
                x <- rvest::html_nodes(x, "a")
                x <- rvest::html_text(x)
                pos <- grep("Cited by", x)
                if (length(pos)) {
                  return(as.numeric(gsub("Cited by", "",  x[pos])))
                } else {
                  return(0)
                }
              }
            )
          )

          gs_infos <- rvest::html_text(rvest::html_nodes(gs_blocks, ".gs_a"))

          gs_links <- unlist(
            lapply(
              gs_blocks,
              function(x) {
                x <- rvest::html_nodes(x, "h3")
                x <- rvest::html_nodes(x, "a")
                return(rvest::html_attr(x, "href"))
              }
            )
          )

          gs_infos <- data.frame(
            query    = tolower(gsub("%20", "_", search_terms)),
            period   = ifelse(
              years[1] == "",
              NA,
              paste(unique(c(years[1], years[2])), collapse = "-")
            ),
            title    = gs_titles,
            infos    = gs_infos,
            citation = gs_citations,
            links    = gs_links
          )

          gs_results <- rbind(gs_results, gs_infos)

        } else {

          gs_infos   <- data.frame()
          gs_results <- rbind(gs_results, gs_infos)
        }

      } else {

        gs_infos   <- data.frame()
        gs_results <- rbind(gs_results, gs_infos)
      }


      if (verbose) {
        cat(paste0("\r   Scraping references ", nrow(gs_results), " on ", 
                   total, "..."))
      }
      
      
      ## Check for next pages ----

      if (!length(gs_infos)) {

        next_btn <- NULL

      } else {

        if (is.null(n_max)) {

          pages <- rvest::html_table(session)

          if (!length(pages)) {

            next_btn <- NULL

          } else {

            pages <- pages[[1]]
            pages <- as.character(pages[1, ])
            pages <- as.numeric(pages[which(!(pages %in% c("Previous", 
                                                           "Next")))])

            page  <- (start / 10) + 1

            if (sum(pages > page) > 0) {
              start <- start + 10
            } else {
              next_btn <- NULL
            }
          }

        } else {

          if (nrow(gs_results) < n_max) {

            pages <- rvest::html_table(session)

            if (!length(pages)) {

              next_btn <- NULL

            } else {

              pages <- pages[[1]]
              pages <- as.character(pages[1, ])
              pages <- as.numeric(pages[which(!(pages %in% c("Previous", 
                                                             "Next")))])

              page  <- (start / 10) + 1

              if (sum(pages > page) > 0) {
                start <- start + 10
              } else {
                next_btn <- NULL
              }
            }

          } else {

            gs_results <- gs_results[1:n_max, ]
            next_btn <- NULL
          }
        }
      }
    } else {
      next_btn <- NULL
    }
  }


  stop_selenium()
  stop_vpn(verbose)

  messages::msg_line()
  messages::msg_rule(right = "done.", col = "darkgrey")

  data_dir <- file.path(output_path, tolower(gsub("%20", "_", search_terms)),
                        "data")
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

  save(gs_results, file = file.path(data_dir, paste0(filename)))

  invisible(NULL)
}
