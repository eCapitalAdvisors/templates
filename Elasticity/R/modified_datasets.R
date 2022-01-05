get_sales_sample <-
function(sales_tbl) {
  #selecting a sample from each brand
  sales_sample_tbl <- sales_tbl %>%
    group_by(description) %>%
    sample_n(1000)
  
  return(sales_sample_tbl)
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
  
  return(total_tbl)
}
