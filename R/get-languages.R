#' Get the ISO-2 code of Google Scholar languages
#'
#' @description 
#' Returns names and ISO-2 codes for 43 World languages supported by 
#' **Google Scholar**. User can search for specific patterns (see examples 
#' below).
#'
#' @param pattern a `character` of length 1. A regular expression to be matched 
#'   (optional).
#'
#' @return A 2-column `data.frame` with:
#'   - `language`: the language name
#'   - `iso_2`: the ISO-2 code
#'
#' @export
#'
#' @examples
#' get_languages()
#' get_languages(pattern = "^Sp")

get_languages <- function(pattern = NULL) {

  if (!is.null(pattern)) {
    
    is_character(pattern)
    
  } else {
    
    pattern <- ""
  }
  
  
  for (i in 1:2) {
    languages[ , i] <- as.character(languages[ , i])
  }

  
  results <-  rbind(languages[grep(tolower(pattern), 
                                   tolower(languages$"language")), ],
                    languages[grep(tolower(pattern), 
                                   tolower(languages$"iso_2")), ])

  results <- results[!duplicated(results[ , "language"]), ]
  rownames(results) <- NULL

  results  
}
