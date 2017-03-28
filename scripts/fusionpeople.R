scrape_fusionpeople <- function(){

  # vignette('RSelenium-basics')
  # vignette("RSelenium-docker", package = "RSelenium")
  
  require(RSelenium)
  
  # Start a selenium server
  rD <- rsDriver(browser = "firefox")
  remDr <- rD[["client"]]
  #remDr$open()
  remDr$navigate("https://fusionpeople.com/search?keywords=labourer&sector=0&location=59")
  
  remDr$getCurrentUrl() # get current URL
  
  webElem <- remDr$findElement(using = "class", value = "load-more-jobs") # load more button
  webElem$getElementAttribute("class")
  
  # Scrape salaries
  webElems <- remDr$findElements(using = "css selector", ".job-details-box tr:nth-last-child(1) td:nth-child(2)")
  n <- length(unlist(lapply(webElems, function(x){ x$getElementText() } )))
  
  repeat{
    
    # Load more
    webElem <- remDr$findElement(using = "class", value = "load-more-jobs")
    webElem$sendKeysToElement(list(key = "enter"))
    
    Sys.sleep(3) # allow time for results to load
    
    # Count jobs
    webElems <- remDr$findElements(using = "css selector", ".job-details-box tr:nth-last-child(1) td:nth-child(2)")
    m <- length(unlist(lapply(webElems, function(x){ x$getElementText() } )))
    
    if(m == n) {
      break
    } else {
      n <- m
    } 
  }
  
  jobdetails <- vector('list')
  
  #salaries
  webElems <- remDr$findElements(using = "css selector", ".job-details-box tr:nth-last-child(1) td:nth-child(2)")
  jobdetails$salary <- unlist(lapply(webElems, function(x){ x$getElementText() } ))
  
  #job types
  webElems <- remDr$findElements(using = "css selector", ".job-details-box tr:nth-last-child(3) td:nth-child(2)")
  jobdetails$jobtype <- unlist(lapply(webElems, function(x){ x$getElementText() } ))
  
  #locations
  webElems <- remDr$findElements(using = "css selector", ".job-details-box tr:nth-last-child(2) td:nth-child(2)")
  jobdetails$location <- unlist(lapply(webElems, function(x){ x$getElementText() } ))
  
  #dates posted
  webElems <- remDr$findElements(using = "css selector", ".job-details-box tr:nth-child(1) td:nth-child(2)")
  jobdetails$posted <- unlist(lapply(webElems, function(x){ x$getElementText() } ))
  
  fusionpeople_labourers <- as.data.frame(do.call('cbind', jobdetails))
  
  
  # ---------------------------- #
  # stop the selenium server
  remDr$close()
  rD[["server"]]$stop() 
  
  # check server
  rD$server$process 
  # ---------------------------- #
  
  
  ## Transform data for upload
  library(dplyr); library(tidyr)
  fusionpeople_labourers <- fusionpeople_labourers %>%
    mutate(
      id = NA_character_,
      date_crawled = Sys.Date(),
      time_crawled = Sys.time(),
      role = paste("Labourer"),
      opco = paste("CPE"),
      location = gsub(" \\(map\\)", "", location),
      date_posted = as.POSIXct(strptime(posted, format = "%d/%m/%Y")),
      site = paste("Fusion People"),
      competitor_type = paste("Specialist"),
      salary_rate = NA_character_,
      salary_max = NA_character_,
      town = NA_character_
    ) %>%
    separate("salary", c("salary", "currency"), sep = " ", extra = "merge") %>%
    dplyr::rename(salary_min = salary) %>%
    separate("location", c("county", "country"), sep = " ,", extra = "merge", fill = "left") %>%
    select(
      id,
      date_crawled,
      time_crawled,
      role,
      opco,
      salary_min,
      salary_max,
      currency,
      salary_rate,
      jobtype,
      town,
      county,
      country,
      date_posted,
      site,
      competitor_type
    ) 
  
  return(fusionpeople_labourers)

}