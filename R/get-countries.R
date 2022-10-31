#' Get the ISO-2 code of countries
#'
#' @description
#' Returns names and ISO-2 codes for 57 World countries (listed from NordVPN 
#' servers). User can search for specific patterns (see examples below).
#'
#' @param pattern a `character` of length 1. A regular expression to be matched 
#'   (optional).
#'
#' @return A 2-column `data.frame` with:
#'   - `country`: the country name
#'   - `iso_2`: the country ISO-2 code
#'
#' @export
#'
#' @examples
#' get_countries()
#' get_countries(pattern = "Fr")

get_countries <- function(pattern = NULL) {

  if (!is.null(pattern)) {
    
    is_character(pattern)
    
  } else {
    
    pattern <- ""
  }
  
  
  for (i in 1:2) {
    countries[ , i] <- as.character(countries[ , i])
  }

  
  results <-  rbind(countries[grep(tolower(pattern), 
                                   tolower(countries$"country")), ],
                    countries[grep(tolower(pattern), 
                                   tolower(countries$"iso_2")), ])
  
  results <- results[!duplicated(results[ , "country"]), ]
  rownames(results) <- NULL

  results
}
