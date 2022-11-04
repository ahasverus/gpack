#' Escape whitespace in URL
#'
#' @description
#' Escapes all whitespaces in a URL.
#'
#' @param x a `character` of length 1. Typically an URL to encode.
#'
#' @return A escaped string.
#'
#' @noRd

escape_url <- function(x) {

  is_character(x)
  
  x <- gsub("[[:space:]]+", " ", x)
  x <- gsub("^[[:space:]]|[[:space:]]$", "", x)
  x <- gsub("[[:space:]]", "%20", x)
  
  x
}



#' Shorten a Google Scholar URL
#'
#' @description
#' Shortens a Google Scholar URL
#'
#' @param url a `character` of length 1. An URL to shorten.
#'
#' @return An URL.
#'
#' @noRd

shorten_gs_url <- function(url) {
  
  is_character(url)
  
  url <- gsub("&[a-z]{1,}_[a-z]{1,}=%22%22", "", url)
  url <- gsub("&[a-z]{1,}_[a-z]{1,}=&", "&", url)
  url <- gsub("&[a-z]{1,}=&", "&", url)
  url <- gsub("&[a-z]{1,}_[a-z]{1,}=$", "", url)
  
  url
}



write_gs_url <- function(search_terms, search_author, search_source, 
                         exclude_terms, years, exact, start, where, lang, 
                         include_patents, include_citations) {
 
  url <- paste0(
    "https://scholar.google.com/scholar?as_q=",   # URL Root
    "&safe=active",                               # Secure Search
    "&btnG=Search+Scholar",                       # Search Engine
    "&hl=en",                                     # Interface Language
    "&as_epq=",                                   # Exact terms (in same order)
    "%22", ifelse(exact, search_terms, ""), "%22",
    "&as_oq=",                                    # At least one of these terms
    ifelse(!exact, search_terms, ""),
    "&as_eq=",                                    # Exclude these terms
    "%22", exclude_terms, "%22",
    "&as_sauthors=",                              # Search by author
    "%22", search_author, "%22",
    "&as_publication=",                           # Search by publication source
    "%22", search_source, "%22",
    "&start=",                                    # Number of first article
    start,
    "&lr=",                                       # Language to search for
    lang,
    "&as_occt=",                                  # Whole document/only title
    where,
    "&as_sdt=",                                   # Include or remove Patents
    ifelse(include_patents,   0, 1),
    "&as_vis=",                                   # Include or remove Citations
    ifelse(include_citations, 0, 1),
    "&as_ylo=",                                   # Starting year (included)
    years[1],
    "&as_yhi=",                                   # Ending year (included)
    years[2])

  shorten_gs_url(url)
}
