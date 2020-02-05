#' @title Get ISO-2 Code of Google Scholar Languages
#'
#' @description This function returns names and ISO-2 codes for 43 World
#' languages used by Google Scholar. User can search for specific patterns
#' (see examples below).
#'
#' @param pattern A character of length 1 containing a regular expression to be matched (optional).
#'
#' @return A 2-columns data frame with:
#'   - language: the language name
#'   - iso_2: the language ISO-2 code
#'
#' @import usethis
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' get_languages()
#'
#' get_languages(pattern = "spain")
#' get_languages(pattern = "Sp")
#' get_languages(pattern = "^Sp")
#' get_languages(pattern = "^Sp")$iso_2
#' }



get_languages <- function(pattern) {

  for (i in 1:2) {
    languages[ , i] <- as.character(languages[ , i])
  }

  if (missing(pattern)) {

    return(languages)

  } else {

    if (is.null(pattern)) {

      usethis::ui_stop(
        stick(
          "
            Argument
            {usethis::ui_field('pattern')}
            cannot be
            {usethis::ui_value('NULL')}
          "
        )
      )
    }

    if (length(pattern) != 1) {

      usethis::ui_stop(
        stick(
          "
            Argument
            {usethis::ui_field('pattern')}
            must be a
            {usethis::ui_value('character of length 1')}
          "
        )
      )
    }

    if (!is.character(pattern)) {

      usethis::ui_stop(
        stick(
          "
            Argument
            {usethis::ui_field('pattern')}
            must be a
            {usethis::ui_value('character of length 1')}
          "
        )
      )
    }

    results <-  rbind(
      languages[grep(tolower(pattern), tolower(languages[ , "language"])), ],
      languages[grep(tolower(pattern), tolower(languages[ , "iso_2"])), ]
    )

    if (nrow(results)) {

      results <- results[!duplicated(results[ , "language"]), ]
      rownames(results) <- NULL

      usethis::ui_done(
        stick(
          "
            {nrow(results)}
            matches found
          ",
          indent = " "
        )
      )

      return(results)

    } else {

      usethis::ui_todo(
        stick(
          "No matches found",
          indent = " "
        )
      )
    }
  }
}
