get_store_locations <-
function(store_locations_tbl, us_locations_tbl) {
    # This table tells us all the unique store locations
    filtered_store_locations_tbl <- store_locations_tbl %>%
      left_join(us_locations_tbl)
    
    saveRDS(object = filtered_store_locations_tbl, file = "../R/filtered_store_locations_tbl.rds")
    
    return(filtered_store_locations_tbl)
  }
get_top_three <-
function(descriptions_tbl, prices_tbl) {
  # filtering for non-negatives and counting rows by UPCS
  top_three_brands_tbl <- prices_tbl %>%
    inner_join(descriptions_tbl) %>%
    group_by(description) %>%
    summarize(total_count = n()) %>%
    filter(total_count > 1000) %>%
    slice_max(total_count, n = 3)
  
  saveRDS(object = top_three_brands_tbl, file = "../R/top_three_brands_tbl.rds")
  
  return(top_three_brands_tbl)
}
get_sales <-
function(descriptions_tbl,
           prices_tbl,
           filtered_store_locations_tbl,
           top_three_brands_tbl,
           dates_tbl) {
    # filtering to top three brands
    sales_tbl <- prices_tbl %>%
      inner_join(descriptions_tbl) %>%
      inner_join(top_three_brands_tbl) %>%
      inner_join(filtered_store_locations_tbl) %>%
      inner_join(dates_tbl) %>%
      mutate(revenue = price * sales, start_year = year(start)) %>%
      select(start,
             end,
             start_year,
             price,
             sales,
             revenue,
             description,
             city,
             zip,
             lat,
             lon,
             state_name)
    
    saveRDS(object = sales_tbl, file = "../R/sales_tbl.rds")
    
    return(sales_tbl)
  }
