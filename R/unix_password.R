#' @title Get the UNIX Password Stored in .Renviron
#'
#' @description This function gets the UNIX password stored in .Renviron under the key \code{UNIX_PASSWD}.
#' Run \code{usethis::edit_r_environ()} and add the following line \code{UNIX_PASSWD='*********'} (make sure to
#' replace \code{*********} by your password).
#'
#' @return The UNIX password value.
#'
#' @import usethis
#'
#' @export
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@gmail.com}
#'
#' @examples
#' \dontrun{
#' unix_password()
#' }



unix_password <- function() {

  pwd <- Sys.getenv("UNIX_PASSWD")

  if (pwd == "") {

    usethis::ui_stop(
      stick(
        "
          Unable to read
          {usethis::ui_field('UNIX user password')}.
          Run
          {usethis::ui_code('usethis::edit_r_environ()')}
          and store your password under the key
          {usethis::ui_field('UNIX_PASSWD')}
        "
      )
    )
  }
}
