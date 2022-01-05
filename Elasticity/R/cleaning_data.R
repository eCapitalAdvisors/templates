input_descriptions <-
function(descriptions_path) {
  # importing file
  descriptions_tbl <- read_excel(descriptions_path) %>%
    select(UPC, DESCRIP) %>%
    rename(description = DESCRIP) %>%
    mutate(
      description = recode(
        description,
        `CINNAMON TOAST CRUNC` = "Cinnamon Toast Crunch",
        `KIX` = "Kix",
        `WHEATIES` = "Wheaties"
      )
    )
  
  return(descriptions_tbl)
}
input_prices <-
function(prices_path) {
  # importing file
  prices_tbl <- read_excel(prices_path) %>%
    select(STORE, UPC, WEEK, MOVE, PRICE) %>%
    filter(PRICE > 0) %>%
    filter(MOVE > 0) %>%
    rename(
      store = STORE,
      week = WEEK,
      sales = MOVE,
      price = PRICE
    )
  
  return(prices_tbl)
}
input_us_locations <-
function(us_locations_path) {
  #importing file
  us_locations_tbl <- read_excel(us_locations_path) %>%
    select(zip, state_name)
  
  return(us_locations_tbl)
}
input_dates <-
function() {
  d <- as_date(7196)
  e <- as_date(7202)
  
  week <- seq(1, 400)
  
  start <- vector()
  start <- append(start, d)
  
  end <- vector()
  end <- append(end, e)
  
  for (i in 1:399) {
    start <- append(start, d + 7)
    end <- append(end, e + 7)
    d <- d + 7
    e <- e + 7
  }
  
  dates_tbl <- data.frame(week = week,
                          start = start,
                          end = end)
  return(dates_tbl)
}
