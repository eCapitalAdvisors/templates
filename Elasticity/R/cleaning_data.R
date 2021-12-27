# Cleaning the data

## function for sales, price, and location data
input_descriptions <- function(descriptions_path) {
  
  # importing file
  descriptions_tbl <- read_excel(descriptions_path) %>%
    select(UPC, DESCRIP) %>%
    rename(description = DESCRIP) %>%
    mutate(description = recode(description, `CINNAMON TOAST CRUNC` = "Cinnamon Toast Crunch", `KIX` = "Kix", `WHEATIES` = "Wheaties"))
  
  saveRDS(object = descriptions_tbl, file = "descriptions_tbl.rds")
}  

input_prices <- function(prices_path) {
  
  # importing file
  prices_tbl <- read_excel(prices_path) %>%
    select(STORE, UPC, WEEK, MOVE, QTY, PRICE) %>%
    filter(PRICE > 0) %>%
    filter(MOVE > 0) %>%
    rename(store = STORE, week = WEEK, sales = MOVE, quantity = QTY, price = PRICE)
  
  saveRDS(object = prices_tbl, file = "prices_tbl.rds")
}

input_store_locations <- function(store_locations_path) {
  
  #importing file
  store_locations_tbl <- read_dta(store_locations_path) %>%
    select(city, zip, store) %>%
    filter(city != "") %>%
    mutate(city = str_to_title(city)) %>%
    filter(!is.na(zip)) %>%
    filter(!is.na(store))
  
  saveRDS(object = store_locations_tbl, file = "store_locations_tbl.rds")
}

input_us_locations <- function(us_locations_path) {
  
  #importing file
  us_locations_tbl <- read_excel(us_locations_path) %>%
    select(zip, state_name)
  
  saveRDS(object = us_locations_tbl, file = "us_locations_tbl.rds")
}

input_dates <- function() {
  
  d <- as_date(7196)
  e <- as_date(7202)
  
  week <- seq(1,400) 
  
  start <- vector()
  start<- append(start,d)
  
  end <- vector()
  end<- append(end, e)
  
  for (i in 1:399) {
    start<- append(start, d + 7)
    end <- append(end, e + 7)
    d <- d + 7
    e <- e + 7
    
    saveRDS(object = dates_tbl, file = "dates_tbl.rds")
  }
  
  dates_tbl <- data.frame(week = week, start = start, end = end)
}

input_illinois_map <- function(illinois_map_path) {
  
  illinois_map <- geojson_read(illinois_map_path, what = "sp")
  
  saveRDS(object = illinois_map, file = "illinois_map.rds")
}

get_store_locations <- function(store_locations_tbl, us_locations_tbl) {
  
  filtered_store_locations_tbl <- store_locations_tbl %>%
    left_join(us_locations_tbl)
  
  saveRDS(object = filtered_store_locations_tbl, file = "filtered_store_locations_tbl.rds")
}

get_top_three <- function(descriptions_tbl, prices_tbl) {
  
  # filtering for non-negatives and counting rows by UPCS
  top_three_brands_tbl <- prices_tbl %>%
    inner_join(descriptions_tbl) %>%
    group_by(description) %>%
    summarize(total_count = n()) %>%
    filter(total_count > 1000) %>%
    slice_max(total_count, n = 3)
  
  saveRDS(object = top_three_brands_tbl, file = "top_three_brands_tbl.rds")
}

