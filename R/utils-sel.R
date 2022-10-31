start_selenium <- function(path, agent = FALSE) {
  
  ## Check system ----
  
  check_docker()
  check_selenium()
  
  
  ## Check args ----
  
  is_character(path)
  
  if (!dir.exists(path)) {
    stop("The path '", path, "' does not exist", call. = FALSE)
  }
  
  is_logical(agent)
  
  
  ## Stop previous container ----
  
  stop_selenium()
  
  
  ## Start Docker container with Selenium ----
  
  shell_expr <- paste0("docker run -d ", 
                       "-p 4444:4444 ", 
                       "-p 7900:7900 ", 
                       "-v ", path, ":/home/seluser/Downloads ", 
                       "--name 'selenium' ", 
                       "selenium/standalone-firefox:latest")
  
  system(shell_expr, ignore.stdout = TRUE)
  
  
  ## Start Selenium session ----
  
  if (agent) {
    
    uagent <- change_ua(verbose = FALSE)
    
    firefox_prefs <- RSelenium::makeFirefoxProfile(
      list("general.useragent.override" = uagent))
    
    rs_driver <- RSelenium::remoteDriver(port = 4444L, browser = "firefox",
                                         extraCapabilities = firefox_prefs)
    
  } else {
    
    rs_driver <- RSelenium::remoteDriver(port = 4444L, browser = "firefox")  
  }
  
  Sys.sleep(5)
  rs_driver$open(silent = TRUE)
  rs_driver$deleteAllCookies()
  
  rs_driver
}


stop_selenium <- function() {
  
  
  ## Check system ----
  
  check_docker()
  check_selenium()
  
  
  containers <- system("docker ps", intern = TRUE, ignore.stderr = TRUE)
  containers <- containers[-1]
  
  
  ## Stop container(s) ----
  
  pos <- grep("selenium", containers)
  
  if (length(pos)) {
    
    containers <- unlist(lapply(strsplit(containers[pos], " "), 
                                function(x) x[1]))
    
    lapply(containers, function(x) system(paste("docker stop", x), 
                                          ignore.stdout = TRUE))
    
    lapply(containers, function(x) system(paste("docker rm", x), 
                                          ignore.stdout = TRUE))
  }
  
  invisible(NULL)
}
