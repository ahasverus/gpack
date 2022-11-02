detect_captcha <- function(rs_driver) {
  
  # captcha <- rs_driver$findElements(using = "id", value = "gs_captcha_c")
  # 
  # if (!length(captcha)) {
  #   captcha <- rs_driver$findElements(using = "id", value = "recaptcha")
  # }
  # 
  # if (!length(captcha)) {
  #   captcha <- rs_driver$findElement(using = "tag", value = "div")
  #   captcha <- grep("\\\nip addr", tolower(captcha$getElementText()))
  # }
  
  # if (!length(captcha)) {
    session <- rs_driver$getPageSource()[[1]]
    captcha <- grep("sending automated queries", session)
  # }
  
  captcha
}


avoid_ban <- function(rs_driver, url, agent, ovpn_country, path, verbose) {
  
  k <- 1
  
  captcha <- detect_captcha(rs_driver)
  
  while (length(captcha)) {
    
    
    ## Prevent infinite loop ----
    
    if (k > 10) {
      stop("You have been permanently banned from G**gle Scholar",
           call. = FALSE)
    }
    
    
    ## Ban message ----
    
    if (k == 1) {
      
      if (verbose) {
        
        messages::msg_line()
        messages::msg_line()
        messages::msg_line(crayon::underline("G**gle Scholar ban"))
      }
    }
    
    messages::msg_todo("Trying to connect to another server. Attempt", 
                       messages::msg_value(k))
    
    
    ## Change IP address ----
    
    # stop_selenium()
    
    stop_vpn(verbose = FALSE)
    
    change_ip(country = ovpn_country, ignore_files = NULL, verbose = verbose)
    
      
    ## Refresh RSelenium ----
    
    rs_driver$navigate(url)
    
    
    ## Check for ban ----
    
    captcha <- detect_captcha(rs_driver)
    
    k <- k + 1
  }
  
  rs_driver
}
