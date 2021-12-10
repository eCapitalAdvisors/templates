# Cleaned data

get_sales <- function(descriptions_tbl, prices_tbl, filtered_store_locations_tbl, top_three_brands_tbl, dates_tbl) {
  
  # filtering to top three brands
  inner_join(descriptions_tbl) %>%
    inner_join(top_three_brands_tbl) %>%
    inner_join(filtered_store_locations_tbl) %>%
    inner_join(dates_tbl) %>%
    select(start, end, price, sales, description, quantity, city, zip, state_name)
  
  saveRDS(object = sales_tbl, file = "sales_tbl.rds")
}

get_sales_sample <- function(sales_tbl){
  
  #selecting a sample from each brand
  sales_sample_tbl <- sales_tbl %>%
    group_by(description) %>%
    sample_n(1000)
  
  saveRDS(object = sales_sample_tbl, file = "sales_sample_tbl.rds")
}
