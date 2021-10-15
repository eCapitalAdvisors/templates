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

plot_histogram_sales <- function(sales_tbl, x_title, title_chart){
  
  #graphing a histogram for sales
  ggplot(data = sales_tbl, aes(x = log(sales))) + 
    geom_density(adjust = 5, aes(fill = description), alpha = .8) +
    labs(x = x_title, y = "Density", title = title_chart, caption = "The x-values are transformed on a log scale.") +
    scale_fill_discrete(name = "Brand Names", labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) + 
    theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"))
}

plot_histogram_price <- function(sales_tbl, x_title, title_chart){
  
  #graphing a histogram for price
  ggplot(data = sales_tbl, aes(x = log(price))) + 
    geom_density(adjust = 5, aes(fill = description), alpha = .8) +
    xlim(0, 2) + 
    labs(x = x_title, y = "Density", title = title_chart, caption = "The x-values are transformed on a log scale.") +
    scale_fill_discrete(name = "Brand Names", labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) + 
    theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"))
}

plot_scatter <- function(sales_sample_tbl){
  ggplot(data = sales_sample_tbl, aes(x = log(price), y = log(sales))) +
    geom_point(aes(color = description), alpha = .8) +
    xlim(0, 1.75) +
    labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
         title = "Price vs. Box Sales", caption = "The x and y values
       are transformed on a log scale.") +
    scale_color_discrete(name = "Brand Names", labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) +
    theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"))
}

# Testing Functions ----
## setting file paths
descriptions_path <- "raw_data_cereal_descriptions.xlsx"
prices_path <- "raw_data_cereal_prices.xlsx"

descriptions_tbl <- input_descriptions(descriptions_path)
prices_tbl <- input_prices(prices_path)

top_three_brands_tbl <- get_top_three(descriptions_tbl, prices_tbl)

sales_tbl <- get_sales(descriptions_tbl, prices_tbl, top_three_brands_tbl)

sales_sample_tbl <- get_sales_sample(sales_tbl)

plot_histogram_sales(sales_tbl, "Sales of Cereal Boxes", "Distribution of Sales")
plot_histogram_price(sales_tbl, "Price of Cereal Boxes", "Distribution of Prices")

plot_scatter(sales_sample_tbl)

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
