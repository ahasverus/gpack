#' @title Retrieve Data from Google Scholar
#'
#' @description This function sends a request to Google Scholar service and
#' retrieves results (title, authors, source and year of publications,
#' and the total number of citations). As no API is provided by Google Scholar
#' (except the one for authors with a Google Scholar ID), this function webscraps
#' the service using \code{RSelenium} or the packages \code{rvest} and \code{httr}.
#' To bypass Google IP bans, user is strongly encouraged to set the arguments
#' \code{openvpn} and \code{user_agent} as \code{TRUE}. In this way, every 10-20
#' sub-requests your IP and your user agent will be automatically and randomly changed.
#'
#' @param search_terms A character vector of terms to search publications for.
#' @param exact A boolen. If TRUE, searchs for the exact terms, otherwise searchs at least one of the terms.
#' @param exclude_terms A character vector of terms to exclude from the search.
#' @param search_author A character vector of authors to search for.
#' @param search_source A character vector of publication sources to search for.
#' @param where Search in the whole document ('any') or only in the title ('title').
#' @param years A vector of 1 or 2 years specifying the temporal extent of the search.
#' @param lang The ISO-2 code of the language to search for. Use \code{get_languages()} to get a list.
#' @param start A numeric specifying the number of the first results from which the results are extracted.
#' @param include_patents A boolean. If TRUE, patents are included in the search results.
#' @param include_citations A boolean. If TRUE, citations are included in the search results.
#' @param selenium A boolean. If TRUE, webscraping is performed with the Selenium technology.
#' @param browser The browser name ('chrome' or 'firefox'). Ignored if \code{selenium = FALSE}.
#' @param openvpn A boolean. If TRUE, public IP address will be randomly changed.
#' @param config_path The path to the folder containing server configuration files.
#' @param exposed_ip User non-protected public IPv4 address obtained with \code{get_ip()}.
#' @param ovpn_country The ISO-2 code of the country to pick up a server. Use \code{get_countries()} to get a list.
#' @param user_agent A boolean. If TRUE, browser user agent will be randomly changed.
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
#' @import rvest
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
  search_source = NULL, where = NULL, years = NULL, lang = NULL, start = 0,
  include_patents = FALSE, include_citations = FALSE, selenium = FALSE,
  browser = "firefox", openvpn = TRUE, config_path = "~/.ovpn", exposed_ip,
  ovpn_country, user_agent = TRUE, sleep = 1, verbose = TRUE
) {


  if (selenium) {}

  if (openvpn) {}


}
