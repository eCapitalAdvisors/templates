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

get_average <- function(sales_tbl) {
  
  #get average for revenue and prices
  average_tbl <- sales_tbl %>%
    group_by(description, year(start)) %>%
    summarize(avg_revenue = mean(price * sales), avg_price = mean(price))

  saveRDS(object = average_tbl, file = "average_tbl.rds")
}

get_revenue_store <- function(sales_tbl) {
  
  #get total revenue per year for each store
  revenue_store <- sales_tbl %>%
    group_by(city, description, year(start)) %>%
    summarise(sum_revenue = sum(price * sales), avg_price = mean(price))

  saveRDS(object = revenue_store, file = "revenue_store.rds")
}

