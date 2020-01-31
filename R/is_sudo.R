is_sudo <- function() {

  if (!(.Platform$OS.type == "unix")) {

    stop("Only Unix (GNU/Linux or macOS) systems are supported.")
  }

  if (system("echo $EUID", intern = TRUE) != 0) {

    stop("You must run R as a super user (sudo) to change your IP.")
  }
}
