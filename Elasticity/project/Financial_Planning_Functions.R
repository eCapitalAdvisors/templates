#' ---
#' title: "Template: Elasticity Financial Functions"
#' author: "Lila Sahar and Juan Malaver"
#' output: github_document
#' ---
#' 

# 1.0 LIBRARIES & DATA ----

# core
library(tidyverse)
library(lubridate)
library(readxl)

# visuals
library(plotly)

# data paths
descriptions_path <- "raw_data_cereal_descriptions.xlsx"
prices_path <- "raw_data_cereal_prices.xlsx"
store_locations_path <- "demo.dta"
us_locations_path <- "uszips.xlsx"


# 2.0 PREPROCESS DATA ----

# 2.1 Cleaning the Table ----

# selecting variables of choice, renaming the variables and standardizing label names
input_descriptions <- function(descriptions_path) {
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
      ))
  
  saveRDS(object = descriptions_tbl, file = "../R/descriptions_tbl.rds")
  
  return(descriptions_tbl)
} 

# selecting the variables of choice, renaming the variable names
input_prices <- function(prices_path) {
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
  
  saveRDS(object = prices_tbl, file = "../R/prices_tbl.rds")
  
  return(prices_tbl)
}

# selecting the variables of choice, changing the format of the data
input_store_locations <- function(store_locations_path) {
  #importing file
  store_locations_tbl <- read_dta(store_locations_path) %>%
    rename(lon = long) %>%
    select(city, zip, lat, lon, store) %>%
    filter(city != "") %>%
    mutate(city = str_to_title(city)) %>%
    mutate(lat = format(lat / 10000, nsmall = 4)) %>%
    mutate(lon = format(lon / -10000, nsmall = 4))
  
  saveRDS(object = store_locations_tbl, file = "../R/store_locations_tbl.rds")
  
  return(store_locations_tbl)
}

# selecting variables of choice
input_us_locations <- function(us_locations_path) {
  #importing file
  us_locations_tbl <- read_excel(us_locations_path) %>%
    select(zip, state_name)
  
  saveRDS(object = us_locations_tbl, file = "../R/us_locations_tbl.rds")
  
  return(us_locations_tbl)
}

# created a dates tibble to match up with the week numbers
input_dates <- function() {
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
  
  saveRDS(object = dates_tbl, file = "../R/dates_tbl.rds")
  
  return(dates_tbl)
}

# 2.2 Joining the Tibbles ----

# joining tibbes
get_store_locations <-
  function(store_locations_tbl, us_locations_tbl) {
    # This table tells us all the unique store locations
    filtered_store_locations_tbl <- store_locations_tbl %>%
      left_join(us_locations_tbl)
    
    saveRDS(object = filtered_store_locations_tbl, file = "../R/filtered_store_locations_tbl.rds")
    
    return(filtered_store_locations_tbl)
  }

# this tibble discovers the three brands that have the most data in the dataset
# to limit the amount of data in the model
get_top_three <- function(descriptions_tbl, prices_tbl) {
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


# this tibble joins all the relevant tables
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

# joins prices, description, and dates table for financial stats
product_detail_tbl <- prices_tbl %>%
  inner_join(dates_tbl, by = c("WEEK" = "week")) %>%
  inner_join(descriptions_tbl) %>%
  mutate(Year = year(end),
         Sales = PRICE * MOVE / QTY) %>%
  filter(OK == 1,
         Sales > 0) %>%
  select(Year, DESCRIP, MOVE, PRICE, QTY, Sales, PROFIT) %>%
  mutate(
    DESCRIP = recode(
      DESCRIP,
      `APPLE CINNAMON CHEER` = "Apple Cinnamon Cheerios",
      `APPLE CINNAMON CHERR` = "Apple Cinnamon Cheerios",
      CHEERIOS = "Cheerios",
      `CINNAMON TOAST CRUNC` = "Cinnamon Toast Crunch",
      `G.M. FIBER ONE` = "Fiber One",
      `HONEY NUT CHEERIOS` = "Honey Nut Cheerios",
      KIX = "Kix",
      `NABISCO WHEAT N BRAN` = "Wheat 'N Bran",
      `SMORES CRUNCH CEREAL` = "S'mores Crunch",
      TOTAL = "Total",
      `TOTAL CORN FLAKES` = "Total Corn Flakes",
      `WHEATIES` = "Wheaties",
      `WHOLE GRAIN TOTAL` = "Whole Grain Total"
    ))

# 2.4 Modified Datasets ----

# this tibble takes a sample of 1000 for the bootstrap
get_sales_sample <- function(sales_tbl) {
  #selecting a sample from each brand
  sales_sample_tbl <- sales_tbl %>%
    group_by(description) %>%
    sample_n(1000)
  
  saveRDS(object = sales_sample_tbl, file = "../R/sales_sample_tbl.rds")
  
  return(sales_sample_tbl)
}

# this tibble looks for the cereal brands that were used for the nine years of the experiment
product_lookup_tbl <- product_detail_tbl %>%
  group_by(DESCRIP) %>%
  summarize(Distinct_Year = n_distinct(Year),
            Sample_Size = n()) %>%
  filter(Distinct_Year == 9)

# this tibble joins the look-up table to the detail table to calculate sum of sales and profit
product_total_tbl <- product_detail_tbl %>%
  inner_join(product_lookup_tbl, by = "DESCRIP") %>%
  group_by(Year) %>%
  summarize(Total_Revenue = sum(Sales),
            Total_GM = sum(PROFIT))

# this tibble calculates our key statistics
product_summary_tbl <- product_detail_tbl %>%
  inner_join(product_lookup_tbl , by = "DESCRIP") %>%
  group_by(Year, DESCRIP) %>%
  summarize(Unit_Sales = sum(MOVE), 
            Avg_Retail_Price = mean(PRICE), 
            Bundle_Sales = sum(QTY),
            Revenue = sum(Sales),
            Gross_Margin = sum(PROFIT)) %>%
  arrange(DESCRIP, Year) %>%
  left_join(product_total_tbl) %>%
  ungroup() %>%
  mutate(Revenue_Pct = Revenue / Total_Revenue,
         Gross_Margin_Pct = Gross_Margin / Total_GM,
         Unit_Sales_Growth = (Unit_Sales/dplyr::lag(Unit_Sales) - 1)) %>%
  select(Year, DESCRIP, Unit_Sales, Unit_Sales_Growth, Avg_Retail_Price, Bundle_Sales, Revenue, Revenue_Pct, Gross_Margin, Gross_Margin_Pct)
  
# changes the format of the tibble
product_summary_wide_tbl <- product_summary_tbl %>%  
  pivot_longer(c(Unit_Sales:Gross_Margin_Pct), names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = Year, values_from = Value)

product_summary_tbl %>%
  ggplot(aes(x = Year, y = Revenue, fill = DESCRIP)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete(limits = seq(1989, 1997)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_discrete(name = "Cereal Brands")

product_summary_tbl %>%
  ggplot(aes(x = Year, y = Revenue, group = DESCRIP, color = DESCRIP)) +
  geom_line() +
  scale_x_discrete(limits = seq(1989, 1997)) +
  scale_color_discrete("Cereal Brands") +
  scale_y_continuous(labels = scales::dollar_format())

