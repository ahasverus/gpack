#' @title Paste in a Single Line a Multi-lines String
#'
#' @description This function pastes in a single line a multi-lines string.
#'
#' @return A character.
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' x <-
#'  "
#'    Hello
#'    world!
#'  "
#'
#' cat(x)
#' cat(stick(x))
#'
#' message(stick(x))
#' message(stick(x, indent = ">>> "))
#'
#' usethis::ui_done(stick(x))
#' usethis::ui_done(stick(x, indent = " "))
#' }



stick <- function(..., indent = "") {

  if (is.null(indent)) {
    indent <- ""
  }

  if (!length(indent)) {
    indent <- ""
  }

  if (length(indent) > 1) {
    indent <- indent[1]
  }

  if (is.na(indent)) {
    indent <- ""
  }

  x <- paste(...)

  if (!length(x)) {
    x <- ""
  }

  x <- gsub("\\\\n", "", x)
  x <- gsub("\\s+", " ", x)
  x <- gsub("^\\s|\\s$", "", x)

  return(paste0(indent, x, collapse = ""))
}
