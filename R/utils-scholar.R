detect_captcha <- function(rs_driver) {
  
  captcha <- rs_driver$client$findElements(using = "id", value = "gs_captcha_c")
  
  if (!length(captcha)) {
    captcha <- rs_driver$client$findElements(using = "id", value = "recaptcha")
  }
  
  if (!length(captcha)) {
    captcha <- rs_driver$client$findElement(using = "tag", value = "div")
    captcha <- grep("\\\nip addr", tolower(captcha$getElementText()))
  }
  
  captcha
}


avoid_ban <- function(rs_driver, url, agent, ovpn_country, path, verbose) {
  
  k <- 1
  
  captcha <- detect_captcha(rs_driver)
  
  while (length(captcha)) {
    
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
    
    stop_selenium()
    
    stop_vpn(verbose = FALSE)
    
    change_ip(country = ovpn_country, ignore_files = NULL, verbose = verbose)
    
    
    ## Restart RSelenium ----
    
    rs_driver <- start_selenium(path, agent)
    
    rs_driver$navigate(url)
    
    k <- k + 1
    
    
    ## Check for ban ----
    
    captcha <- detect_captcha(rs_driver)
    
    
    ## Prevent infinite loop ----
    
    if (k > 10) {
      stop("You have been permanently banned from G**gle Scholar",
           call. = FALSE)
    }
  }
  
  rs_driver
}
