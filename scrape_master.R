scrape_randstad_competitors <- function(){

  print("Loading libraries")
  library(RMySQL) 
  library(rvest) # scraping
  library(RSelenium) # scraping
  library(stringr)
  library(dplyr)
  library(readr)
  library(tidyr)
  
  ## SCRAPE SITES
  
  # Source scraping scripts (i.e. "scrapts")
  wd <- "C:/Egnyte/Private/jwolman/My Documents/Randstad/Core Candidates/Scraping/scripts"
  scrapts <- list.files(path = wd, pattern = ".R")
  lapply(scrapts, function(x) source(paste(wd, x, sep = "/")))
  
  # Commence mass scrape
  print("Scraping Anderselite")
  anderselite  <- scrape_anderselite()
  print("Scraping Fusion People")
  fusionpeople <- scrape_fusionpeople()
  print("Scraping Hays")
  hays <- scrape_hays()
  
  print("Combining results")
  all_scrapes <- bind_rows(anderselite, fusionpeople, hays)
  
  #all_scrapes <- rbind(anderselite, fusionpeople, hays) 
  
  # Generate metadata
  print("Gathering metadata...")
  metadata <- vector('list')
  
  # totals
  metadata$totals <- all_scrapes %>%
    dplyr::group_by(date_crawled, site, opco, role) %>%
    summarize(jobs_found = length(site))
  
  # types
  metadata$types <- all_scrapes %>%
    dplyr::group_by(date_crawled, site, opco, role, jobtype) %>%
    summarize(jobs_found = length(site))
  
  # locations
  metadata$locations <- all_scrapes %>%
    dplyr::group_by(date_crawled, site, opco, role, town, county) %>%
    summarize(jobs_found = length(site))
  
  ##### UPLOAD TO LOCAL MYSQL DATABASE #####
  
  tbl_names <- c("totals", "types", "locations")
  
  print("Connecting to local MySQL database")
  mydb = dbConnect(
    MySQL(),
    user = Sys.getenv("id"),
    password = Sys.getenv("pw"),
    host = 'localhost',
    dbname = 'randstad_competitors'
  )
  
  rows_before_sql <- dbGetQuery(mydb, "select count(*) from raw_scrape_data;")
  
  dbWriteTable(
    conn = mydb,
    value = all_scrapes,
    name = 'raw_scrape_data',
    append = TRUE,
    row.names = FALSE,
    col.names = FALSE,
    overwrite = FALSE
    )
  
  rows_after_sql <- dbGetQuery(mydb, "select count(*) from raw_scrape_data;")
  rows_added <- as.numeric(rows_after_sql - rows_before_sql)
  print(paste(rows_added, "new rows added to the table"))
  
  # Upload metadata
  print("Uploading metadata tables...")
  for(i in seq_along(metadata)){
    dbWriteTable(
      conn = mydb,
      value = as.data.frame(metadata[[tbl_names[i]]]),
      name = paste0("metadata_", tbl_names[i]),
      append = TRUE,
      row.names = FALSE,
      col.names = FALSE,
      overwrite = FALSE
    )
  }
  
  print("Closing connections...")
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) dbDisconnect(con)
  
  
  ##### UPLOAD TO MYSQL DATABASE ON STAGING SERVER #####
  
  mydb = dbConnect(
    MySQL(),
    user = Sys.getenv("staging_id"),
    password = Sys.getenv("staging_pw"),
    host = '84.45.119.199',
    dbname = 'randstad_competitors'
  )
  
  #fetch all rows using:
  #res <- dbSendQuery(mydb, "SELECT * FROM raw_scrape_data")
  #data <- dbFetch(res, n = -1)
  
  dbWriteTable(
    conn = mydb,
    value = all_scrapes,
    name = 'raw_scrape_data',
    append = TRUE,
    row.names = FALSE,
    col.names = FALSE,
    overwrite = FALSE
    )
  
  # Upload metadata
  print("Uploading metadata tables...")
  for(i in seq_along(metadata)){
    dbWriteTable(
      conn = mydb,
      value = as.data.frame(metadata[[tbl_names[i]]]),
      name = paste0("metadata_", tbl_names[i]),
      append = TRUE,
      row.names = FALSE,
      col.names = FALSE,
      overwrite = FALSE
    )
  }
  
  print("Closing connections to pfstagebi...")
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons) dbDisconnect(con)

}

scrape_randstad_competitors()