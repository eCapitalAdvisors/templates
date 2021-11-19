# Cleaning the data

## function for sales and price data
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
    select(UPC, MOVE, PRICE) %>%
    filter(PRICE > 0) %>%
    filter(MOVE > 0) %>%
    rename(sales = MOVE, price = PRICE)
  
  saveRDS(object = prices_tbl, file = "prices_tbl.rds")
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
