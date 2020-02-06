check_string <- function(x, env) {

  if (missing(x)) {

    usethis::ui_stop(
      stick(
        "
          Argument
          {usethis::ui_field('x')}
          is mandatory and must be a
          {usethis::ui_value('character of length 1')}
        "
      )
    )
  }

  if (is.null(x)) {

    usethis::ui_stop(
      stick(
        "
          Argument
          {usethis::ui_field('x')}
          must be a
          {usethis::ui_value('character of length 1')}
        "
      )
    )
  }

  if (!is.character(x) || length(x) != 1) {

    usethis::ui_stop(
      stick(
        "
          Argument
          {usethis::ui_field('x')}
          must be a
          {usethis::ui_value('character of length 1')}
        "
      )
    )
  }

  if (x == "") {

    usethis::ui_stop(
      stick(
        "
          Argument
          {usethis::ui_field('x')}
          must be a
          {usethis::ui_value('character of length 1')}
        "
      )
    )
  }


  if (is.null(get(x, envir = env))) {

    usethis::ui_stop(
      stick(
        "
          Argument
          {usethis::ui_field(x)}
          must be a
          {usethis::ui_value('character of length 1')}
        "
      )
    )
  }

  if (length(get(x, envir = env)) != 1) {

    usethis::ui_stop(
      stick(
        "
          Argument
          {usethis::ui_field(x)}
          must be a
          {usethis::ui_value('character of length 1')}
        "
      )
    )
  }


  if (is.na(get(x, envir = env))) {

    usethis::ui_stop(
      stick(
        "
          Argument
          {usethis::ui_field(x)}
          must be a
          {usethis::ui_value('character of length 1')}
        "
      )
    )
  }

  if (!is.character(get(x, envir = env))) {

    usethis::ui_stop(
      stick(
        "
          Argument
          {usethis::ui_field(x)}
          must be a
          {usethis::ui_value('character of length 1')}
        "
      )
    )
  }

  if (get(x, envir = env) == "") {

    usethis::ui_stop(
      stick(
        "
          Argument
          {usethis::ui_field(x)}
          must be a
          {usethis::ui_value('character of length 1')}
        "
      )
    )
  }
}
