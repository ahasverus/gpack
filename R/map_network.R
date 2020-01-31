map_network <- function() {

  is_sudo()

  interfaces <- system("ifconfig", intern = TRUE)
  pattern    <- paste0(
    "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}",
     "(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  )
  matches    <- grep(pattern, interfaces)

  if (length(matches) == 1) {

    stop("No internet connexion.")
  }
}
