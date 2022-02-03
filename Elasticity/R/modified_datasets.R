get_sales_sample <-
function(sales_tbl) {
  #selecting a sample from each brand
  sales_sample_tbl <- sales_tbl %>%
    group_by(description) %>%
    sample_n(1000)
  
  saveRDS(object = sales_sample_tbl, file = "../R/sales_sample_tbl.rds")
  
  return(sales_sample_tbl)
}
get_total <-
function(sales_tbl) {
  #get total for revenue and average prices
  total_tbl <- sales_tbl %>%
    group_by(city, description, year(start)) %>%
    transmute(
      total_revenue = sum(revenue),
      avg_price = mean(price),
      sum_sales = sum(sales)
      
# you need to fix this table because it won't work with the graphics down below anymore. It doesn't 
# output 'city', 'description', or 'year(start)' plus it doesn't sum(revenue)
    )
  
  saveRDS(object = total_tbl, file = "../R/total_tbl.rds")
  
  return(total_tbl)
}
