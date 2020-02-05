#' @title Get ISO-2 Code of Countries
#'
#' @description This function returns names and ISO-2 codes for 57 World
#' countries (listed from NordVPN servers). User can search for specific patterns
#' (see examples below).
#'
#' @param pattern A character of length 1 containing a regular expression to be matched (optional).
#'
#' @return A 2-columns data frame with:
#'   - country: the country name
#'   - iso_2: the country ISO-2 code
#'
#' @import usethis
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' get_countries()
#'
#' get_countries(pattern = "Fre")
#' get_countries(pattern = "Fr")
#' get_countries(pattern = "^Fr")
#' get_countries(pattern = "^Fr")$iso_2
#' }



get_countries <- function(pattern) {

  for (i in 1:2) {
    countries[ , i] <- as.character(countries[ , i])
  }

  if (missing(pattern)) {

    return(countries)

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
      countries[grep(tolower(pattern), tolower(countries[ , "country"])), ],
      countries[grep(tolower(pattern), tolower(countries[ , "iso_2"])), ]
    )

    if (nrow(results)) {

      results <- results[!duplicated(results[ , "country"]), ]
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
