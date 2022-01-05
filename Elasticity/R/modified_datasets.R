get_sales_sample <-
function(sales_tbl) {
  #selecting a sample from each brand
  sales_sample_tbl <- sales_tbl %>%
    group_by(description) %>%
    sample_n(1000)
  
  saveRDS(object = sales_sample_tbl, file = "../R/sales_sample_tbl.rds")
}
get_total <-
function(sales_tbl) {
  #get total for revenue and average prices
  total_tbl <- sales_tbl %>%
    group_by(city, description, year(start)) %>%
    summarize(
      total_revenue = sum(price * sales),
      avg_price = mean(price),
      sum_sales = sum(sales)
    ) %>%
    select(description,
           `year(start)`,
           total_revenue,
           avg_price,
           sum_sales)
  
  saveRDS(object = total_tbl, file = "../R/total_tbl.rds")
}
