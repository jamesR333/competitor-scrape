scrape_anderselite <- function(){

  # Store web url
  url.seed <- read_html("http://www.anderselite.com/Jobs/?st=1&s=1&q=labourer") # p query is for page numbers
  search_url <- "http://www.anderselite.com/Jobs/?st=1&s=1&q=labourer&page="

  # Get number of results pages
  last_page <- url.seed %>%
    html_nodes(css = "#ctl00_CPHContent_pagingHeader_pnlDynamicPaging .dynamicPagingLink:nth-last-child(2)") %>%
    html_attr("href") %>%
    strsplit("&page=")
  last_page <- unlist(last_page)[[2]] %>% as.numeric()
  
  # Scrape salary and location info
  scrape_results <- vector('list')
  scrape_results$salaries <- vector('list', length = last_page)
  scrape_results$locations <- vector('list', length = last_page)
  scrape_results$types <- vector('list', length = last_page)
  
  for(i in 1:last_page) {
    print(paste0("Scraping URL: ", i, " of ", last_page))
    
    # Get salary data
    scrape_results$salaries[[i]] <- read_html(paste0(search_url, i)) %>%
      html_nodes(css = ".salary span") %>%
      html_text()
    
    # Get location data
    scrape_results$locations[[i]] <- read_html(paste0(search_url, i)) %>%
      html_nodes(css = ".location span") %>%
      html_text()
    
    # Get job type data
    scrape_results$types[[i]] <- read_html(paste0(search_url, i)) %>%
      html_nodes(css = ".type span") %>%
      html_text()
    
  }
  scrapings <- as.data.frame(sapply(scrape_results, unlist))
  
  scrapings_transformed <- scrapings %>%
    mutate(
      id = NA_character_,
      date_crawled = Sys.Date(),
      time_crawled = Sys.time(),
      role = paste("Labourer"),
      opco = paste("CPE"),
      date_posted = as.Date(NA_character_),
      site = paste("Anderselite"),
      competitor_type = paste("Specialist")
    ) %>%
    separate("salaries", c("salary_min", "salary_max"), sep = " - ", extra = "merge", fill = "left") %>%
    separate("salary_max", c("salary_max", "currency"), sep = " ", extra = "merge", fill = "left") %>%
    separate("currency", c("currency", "salary_rate"), sep = " ", extra = "merge", fill = "left") %>%
    separate("locations", c("country", "county"), sep = ", ", extra = "merge", fill = "right") %>%
    separate("county", c("county", "town"), sep = ", ", extra = "merge", fill = "right") %>%
    dplyr::rename(jobtype = types) %>%
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
  
  return(scrapings_transformed)

}