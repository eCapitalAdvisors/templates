## loading libraries
library(tidyverse)
library(readxl)

## function for sales and price data
input_descriptions <- function(descriptions_path) {
  
  # importing file
  descriptions_tbl <- read_excel(descriptions_path) %>%
    select(UPC, DESCRIP) %>%
    rename(description = DESCRIP)
}  

input_prices <- function(prices_path) {
  
  # importing file
  prices_tbl <- read_excel(prices_path) %>%
    select(UPC, MOVE, PRICE) %>%
    filter(PRICE > 0) %>%
    filter(MOVE > 0) %>%
    rename(sales = MOVE, price = PRICE)
}

get_top_three <- function(descriptions_tbl, prices_tbl) {
  
  # filtering for non-negatives and counting rows by UPCS
  top_three_brands_tbl <- prices_tbl %>%
    inner_join(descriptions_tbl) %>%
    group_by(description) %>%
    summarize(total_count = n()) %>%
    filter(total_count > 1000) %>%
    slice_max(total_count, n = 3)
}

get_sales <- function(descriptions_tbl, prices_tbl, top_three_brands_tbl) {
  
  # filtering to top three brands
  sales_tbl <- prices_tbl %>%
    inner_join(descriptions_tbl) %>%
    inner_join(top_three_brands_tbl) %>%
    select(sales, price, description)
}

get_sales_sample <- function(sales_tbl){
  
  #selecting a sample from each brand
  sales_sample_tbl <- sales_tbl %>%
    group_by(description) %>%
    sample_n(1000)
}

plot_histogram <- function(sales_tbl, x_title, title_chart){
  
  #graphing a histogram
  ggplot(data = sales_tbl, aes(x = log(sales))) + 
    geom_density(adjust = 5, aes(fill = description), alpha = .8) +
    xlim(0, 2) + 
    labs(x = x_title, y = "Density", title = title_chart, caption = "The x-values are transformed on a log scale.") +
    scale_fill_discrete(name = "Brand Names", labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) + 
    theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"))
}

plot_scatter <- function(){}

# Testing Functions ----
## setting file paths
descriptions_path <- "raw_data_cereal_descriptions.xlsx"
prices_path <- "raw_data_cereal_prices.xlsx"

descriptions_tbl <- input_descriptions(descriptions_path)
prices_tbl <- input_prices(prices_path)

top_three_brands_tbl <- get_top_three(descriptions_tbl, prices_tbl)

sales_tbl <- get_sales(descriptions_tbl, prices_tbl, top_three_brands_tbl)

sales_sample_tbl <- get_sales_sample(sales_tbl)

plot_histogram(sales_tbl, "Number of Cereal Boxes Sold", "Distribution of Sales")


# Testing Function pt. 2 ----
hchart(density((sales_tbl %>%
                 filter(description == "CINNAMON TOAST CRUNC") %>%
                 mutate(sales = log(sales)))$sales),
       type = "area", name = "Cinammon Toast Crunch") %>%
  hc_add_series(
    density((sales_tbl %>%
              filter(description == "KIX") %>%
              mutate(sales = log(sales)))$sales),
    type = "area",
    name = "KIX"
  ) %>%
  hc_add_series(
    density((sales_tbl %>%
              filter(description == "WHEATIES") %>%
              mutate(sales = log(sales)))$sales),
    type = "area",
    name = "Wheaties"
  )
