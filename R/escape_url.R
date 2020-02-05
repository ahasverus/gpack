#' @title Escape Whitespace in URL
#'
#' @description This function escapes all whitespaces in a URL.
#'
#' @param x A string to escape whitespaces.
#'
#' @return A escaped string.
#'
#' @import usethis
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' escape_url("Canis lupus lupus")
#' escape_url("https://google.com/canis lupus")
#' }



escape_url <- function(x) {

  if (missing(x)) {

    usethis::ui_stop(
      stick(
        "
          {usethis::ui_field('x')}
          must be a
          {usethis::ui_value('character')}
        "
      )
    )
  }

  if (is.null(x)) {

    usethis::ui_stop(
      stick(
        "
          {usethis::ui_field('x')}
          must be a
          {usethis::ui_value('character')}
        "
      )
    )
  }

  if (!is.character(x)) {

    usethis::ui_stop(
      stick(
        "
          {usethis::ui_field('x')}
          must be a
          {usethis::ui_value('character')}
        "
      )
    )
  }

  unlist(
    lapply(
      x,
      function(x) {
        x <- gsub("[[:space:]]+", " ", x)
        x <- gsub("^[[:space:]]|[[:space:]]$", "", x)
        gsub("[[:space:]]", "%20", x)
      }
    ),
    use.names = FALSE
  )
}
