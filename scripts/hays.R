scrape_hays <- function(){

  # Store web url
  url.seed <- "https://m.hays.co.uk/search/?q=labourer&p=" # p query is for page numbers
  hays_search <- read_html(url.seed)
  
  # Get number of results pages
  last_page <- hays_search %>%
    html_nodes(css = "#lastPage a") %>%
    html_attr("href") %>%
    strsplit("&p=")
  last_page <- unlist(last_page)[[2]] %>% as.numeric()
  
  # Get salaries from all results pages
  scrape_results <- vector('list')
  scrape_results$salaries <- vector('list', length = last_page)
  scrape_results$locations <- vector('list', length = last_page)
  
  for(i in 1:last_page) {
    print(paste0("Scraping URL: ", i, " of ", last_page))
    
    # Get salary data
    scrape_results$salaries[[i]] <- read_html(paste0(url.seed, i)) %>%
      html_nodes(css = ".hays-result-rate-value .value") %>%
      html_text()
    
    # Get location data
    scrape_results$locations[[i]] <- read_html(paste0(url.seed, i)) %>%
      html_nodes(css = ".hays-result-location .value") %>%
      html_text()
    
  }
  
  scrapings <- as.data.frame(sapply(scrape_results, unlist))
  
  # Salary rate groupings
  rates <- vector('list')
  rates$hour <- c("hr", "hour", "p/h")
  rates$day <- c("day", "daily")
  rates$week <- c("week")
  rates$month <- c("month", "pcm")
  rates$year <- c("year", "annual", "annum") # or matches regex "\\d+k|\\d{2,3},\\d{3}"
  
  
  scrapings_transformed <- scrapings %>%
    mutate(
      id = NA_character_,
      date_crawled = Sys.Date(),
      time_crawled = Sys.time(),
      role = paste("Labourer"),
      opco = paste("CPE"),
      date_posted = as.Date(NA_character_),
      site = paste("Hays"),
      competitor_type = paste("General"),
      salaries = gsub("£", "", salaries),
      county = NA_character_,
      country = NA_character_,
      currency = paste("GBP"),
      jobtype = NA_character_
    ) %>%
    separate("salaries", c("salary_min", "salary_other"), sep = "\\s|\\sto\\s|to|-|\\s-\\s|\\(|\\/", extra = "merge", fill = "left") %>%
    mutate(
      # extract currency-like strings e.g. 18.54 or 18 or 18,000
      salary_min = as.numeric(gsub(",", "", str_match(salary_min,   "\\d+(?:\\.\\d{2}|,\\d{3})?"))),
      salary_max = as.numeric(gsub(",", "", str_match(salary_other, "\\d+(?:\\.\\d{2}|,\\d{3})?")))
      ) %>% 
    rowwise() %>%
    mutate( # classify salaries into appropriate rates
      salary_rate = ifelse(TRUE %in% (str_match(tolower(salary_other), rates$hour) > 1) | salary_min <= 40, "per hour",
                           ifelse(TRUE %in% (str_match(tolower(salary_other), rates$day) > 1), "per day",
                                  ifelse(TRUE %in% (str_match(tolower(salary_other), rates$week) > 1), "per week",
                                         ifelse(TRUE %in% (str_match(tolower(salary_other), rates$month) > 1), "per month",
                                                ifelse(TRUE %in% (str_match(tolower(salary_other), rates$year) > 1) | grepl("\\d+k|\\d{2,3},\\d{3}", tolower(salary_other), perl = TRUE) | salary_min >= 15000 | salary_max >= 15000, "per year",
                                                       ifelse(grepl("competitive", tolower(salary_other)), "Competitive",
                                                              ifelse(grepl("negotiable", tolower(salary_other)), "Negotiable",
                                                                     NA_character_
                                                                     )
                                                              )
                                                       )
                                                )
                                         )
                                  )
                           ),
      # amend annual salaries originally inputted at abbreviated numbers
      salary_max = ifelse(salary_rate == "per year" & salary_max < 10000, salary_max * 1000, salary_max)
    ) %>%
    dplyr::rename(town = locations) %>%
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
  
  ## If salary columns contain only min OR max then copy across
  copy_salaries <- function(x){
    # final checks over salary information
    for(i in 1:nrow(x)){
      if(is.na(x[i, "salary_min"]) & !is.na(x[i, "salary_max"])) {
        x[i, "salary_min"] <- x[i, "salary_max"]
      }
      if(is.na(x[i, "salary_max"]) & !is.na(x[i, "salary_min"])) {
        x[i, "salary_max"] <- x[i, "salary_min"]
      }
      if(is.na(x[i, "salary_rate"]) & !is.na(x[i, "salary_min"]) & x[i, "salary_min"] > 12000) {
        x[i, "salary_rate"] <- "per year"
      }
    }
    return(x)
  }
  
  scrapings_transformed <- copy_salaries(scrapings_transformed)
    
  
  
  # Write salary classifer heatmap to file
  # wd <- "C:/Egnyte/Private/jwolman/My Documents/Randstad/Core Candidates/Scraping/Heatmaps/"
  # jpeg(file = paste0(wd, "hays_salary_heatmap_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S") , ".jpeg"), width = 900, height = 900)
  # heatmap(x = table(scrapings_transformed$salary_max, scrapings_transformed$salary_rate),
  #         Rowv = NA,
  #         Colv = NA,
  #         margins = c(7, 2),
  #         na.rm = TRUE,
  #         main = "This is how Hays' salaries have been classified")
  # dev.off()
  
  return(scrapings_transformed)

}