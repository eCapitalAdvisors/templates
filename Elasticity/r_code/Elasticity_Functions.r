#' ---
#' title: "Template: Elasticity Functions"
#' author: "Lila Sahar and Juan Malaver"
#' date: "October 15, 2021"
#' output: github_document
#' ---
#' 

## loading libraries
library(tidyverse)
library(plotly)
library(broom)
library(infer)
library(lubridate)
library(readxl)
library(haven)

## function for sales and price and store location data
input_descriptions <- function(descriptions_path) {
  
  # importing file
  descriptions_tbl <- read_excel(descriptions_path) %>%
    select(UPC, DESCRIP) %>%
    rename(description = DESCRIP) %>%
    mutate(description = recode(description, `CINNAMON TOAST CRUNC` = "Cinnamon Toast Crunch", `KIX` = "Kix", `WHEATIES` = "Wheaties"))
  
}  

input_prices <- function(prices_path) {
  
  # importing file
  prices_tbl <- read_excel(prices_path) %>%
    select(STORE, UPC, WEEK, MOVE, QTY, PRICE) %>%
    filter(PRICE > 0) %>%
    filter(MOVE > 0) %>%
    rename(store = STORE, week = WEEK, sales = MOVE, quantity = QTY, price = PRICE)
}

input_store_locations <- function(store_locations_path) {
  
  #importing file
  store_locations_tbl <- read_dta(store_locations_path) %>%
    select(city, zip, store) %>%
    filter(city != "") %>%
    mutate(city = str_to_title(city)) %>%
    filter(!is.na(zip)) %>%
    filter(!is.na(store))
}

input_us_locations <- function(us_locations_path) {
  
  #importing file
  us_locations_tbl <- read_excel(us_locations_path) %>%
    select(zip, state_name)
}

input_dates <- function() {
  
  d <- as_date(7196)
  e <- as_date(7202)
  
  week <- seq(1,400) 
  
  start <- vector()
  start<- append(start,d)
  
  end <- vector()
  end<- append(end, e)
  
  for (i in 1:399) {
    start<- append(start, d + 7)
    end <- append(end, e + 7)
    d <- d + 7
    e <- e + 7
  }
  
  dates_tbl <- data.frame(week = week, start = start, end = end)
}

get_store_locations <- function(store_locations_tbl, us_locations_tbl) {
  
  filtered_store_locations_tbl <- store_locations_tbl %>%
    left_join(us_locations_tbl)
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

get_sales <- function(descriptions_tbl, prices_tbl, filtered_store_locations_tbl, top_three_brands_tbl, dates_tbl) {
  
  # filtering to top three brands
  sales_tbl <- prices_tbl %>%
    inner_join(descriptions_tbl) %>%
    inner_join(top_three_brands_tbl) %>%
    inner_join(filtered_store_locations_tbl) %>%
    inner_join(dates_tbl) %>%
    select(start, end, price, sales, description, quantity, city, zip, state_name)
}

get_sales_sample <- function(sales_tbl){
  
  #selecting a sample from each brand
  sales_sample_tbl <- sales_tbl %>%
    group_by(description) %>%
    sample_n(1000)
}

plot_boxplot_sales <- function(sales_tbl, x_title, y_title, title_chart) {
  
  #graphing a box plot
  p <- ggplot(data = sales_tbl, aes(x = description, y = log(sales), color = description)) +
    geom_boxplot() +
    labs(x = x_title, y = y_title, title = title_chart, caption = "The y-values are transformed on a log scale.") +
    theme(plot.title = element_text(hjust = .5, face = "bold"), plot.caption = element_text(hjust = .5)) +
    coord_flip()
  
  hide_legend(ggplotly(p))
  }

plot_boxplot_price <- function(sales_tbl, x_title, y_title, title_chart) {
  
  #graphing a box plot
  p <- ggplot(data = sales_tbl, aes(x = description, y = log(price), color = description)) +
    geom_boxplot() +
    labs(x = x_title, y = y_title, title = title_chart, caption = "The y-values are transformed on a log scale.") +
    theme(plot.title = element_text(hjust = .5, face = "bold"), plot.caption = element_text(hjust = .5)) +
    coord_flip()

  hide_legend(ggplotly(p))
}

plot_violin_sales <- function(sales_tbl, x_title, y_title, title_chart) {
  
  #graphing a box plot
  p <- ggplot(data = sales_tbl, aes(x = description, y = log(sales), color = description)) +
    geom_violin() +
    labs(x = x_title, y = y_title, title = title_chart, caption = "The y-values are transformed on a log scale.") +
    theme(plot.title = element_text(hjust = .5, face = "bold"), plot.caption = element_text(hjust = .5)) +
    coord_flip()
  
  hide_legend(ggplotly(p))
}

plot_violin_price <- function(sales_tbl, x_title, y_title, title_chart) {
  
  #graphing a box plot
  p <- ggplot(data = sales_tbl, aes(x = description, y = log(price), color = description)) +
    geom_violin() +
    labs(x = x_title, y = y_title, title = title_chart, caption = "The y-values are transformed on a log scale.") +
    theme(plot.title = element_text(hjust = .5, face = "bold"), plot.caption = element_text(hjust = .5)) +
    coord_flip()
  
  hide_legend(ggplotly(p))
}

plot_histogram_sales <- function(sales_tbl, x_title, title_chart){
  
  #graphing a histogram for sales
  p <- ggplot(data = sales_tbl, aes(x = log(sales), fill = description)) + 
    geom_density(adjust = 5, aes(x = log(sales), fill = description), alpha = .8) +
    labs(x = x_title, y = "Density", title = title_chart, caption = "The x-values are transformed on a log scale.") +
    scale_fill_discrete(name = "Brand Names") + 
    theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))

  ggplotly(p)
  }

plot_histogram_price <- function(sales_tbl, x_title, title_chart){
  
  #graphing a histogram for price
  p <- ggplot(data = sales_tbl, aes(x = log(price))) + 
    geom_density(adjust = 5, aes(fill = description), alpha = .8) +
    xlim(0, 2) + 
    labs(x = x_title, y = "Density", title = title_chart, caption = "The x-values are transformed on a log scale.") +
    scale_fill_discrete(name = "Brand Names") + 
    theme(plot.title = element_text(hjust = .5, face = "bold"), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))

  ggplotly(p)
  }

plot_scatter <- function(sales_sample_tbl, model = "none"){
  
  if (model == "RI") {
    mod <- glm(log(sales) ~ log(price) + description, data = sales_sample_tbl)
    
    p <- ggplot(data = cbind(sales_sample_tbl, pred = predict(mod)), aes(x = log(price), y = log(sales))) +
      geom_point(col = "gray", alpha = .8) +
      geom_line(aes(y = pred, color = description), size = 1) + 
      xlim(0, 1.75) +
      labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
           title = "Price vs. Box Sales", fill = "Brand Names", caption = "The x and y values
       are transformed on a log scale.") +
      theme(legend.position = "None", plot.title = element_text(hjust = .5, face = "bold"), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))
    
    ggplotly(p) 
  }
  else if (model == "RIRS") {
    mod <- glm(log(sales) ~ log(price) * description, data = sales_sample_tbl)
    
    p <- ggplot(data = cbind(sales_sample_tbl, pred = predict(mod)), aes(x = log(price), y = log(sales))) +
      geom_point(col = "gray", alpha = .8) +
      geom_line(aes(y = pred, color = description), size = 1) + 
      xlim(0, 1.75) +
      labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
           title = "Price vs. Box Sales", fill = "Brand Names", caption = "The x and y values
       are transformed on a log scale.") +
      theme(legend.position = "None", plot.title = element_text(hjust = .5, face = "bold"), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))
    
    ggplotly(p)
  }
  else {
    p <- ggplot(data = sales_sample_tbl, aes(x = log(price), y = log(sales))) +
      geom_point(aes(color = description), alpha = .8) +
      xlim(0, 1.75) +
      labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
           title = "Price vs. Box Sales", fill = "Brand Names", caption = "The x and y values
       are transformed on a log scale.") +
      theme(legend.position = "None", plot.title = element_text(hjust = .5, face = "bold"), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))
    
    ggplotly(p) 
  }
}

plot_fitted_vs_residual <- function(sales_sample_tbl, model = "none", method = "ML") {
  
  if (model == "REM") {
    lmfit <- lm(log(sales) ~ log(price) + description, data = sales_sample_tbl)
    
    p <- ggplot(sales_sample_tbl, aes(lmfit$fitted.values, lmfit$residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Fitted vs Residuals", x = "Residuals", y = "Fitted Values") +
      theme(plot.title = element_text(hjust = .5, face = "bold"))
    
    ggplotly(p)
  }
  
  else if (model == "MEM") {
    lmfit <- lm(log(sales) ~ log(price) * description, data = sales_sample_tbl)
    
    p <- ggplot(sales_sample_tbl, aes(lmfit$fitted.values, lmfit$residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Fitted vs Residuals", x = "Residuals", y = "Fitted Values") +
      theme(plot.title = element_text(hjust = .5, face = "bold"))
    
    ggplotly(p)
  }
}

## Bootstrapping Method

get_bootstrap <- function(sales_tbl) {
  
  # obtain betas for bootstrap
  bootstrap_tbl <- sales_tbl %>%
    mutate(sales = log(sales), price = log(price)) %>%
    specify(formula = sales ~ price + description) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    fit()
  
  saveRDS(object = bootstrap_tbl, file = "bootstrap_tbl.rds")
}


# obtain confidence interval for bootstrap
get_ci_for_bootstrap <- function(bootstrap_tbl) {
  
  ci <- bootstrap_tbl %>%
    group_by(term) %>%
    nest() %>%
    mutate(perc_ci = map(
      data,
      get_confidence_interval,
      level = 0.95,
      type = "percentile"
    )) %>%
    unnest(perc_ci)
  
}

# plot the bootstrap

plot_bootstrap <- function(bootstrap_tbl) {
  p <- ggplot(bootstrap_tbl %>% filter(term == "price") %>% select(replicate, estimate), aes(estimate)) +
    geom_density() + 
    geom_vline(xintercept = ci %>% filter(term == "price") %>% pull(lower_ci), linetype = "dotted", color = "red") +
    geom_vline(xintercept = ci %>% filter(term == "price") %>% pull(upper_ci), linetype = "dotted", color = "red") +
    labs(title = "Bootstrap of Means", x = "Estimates of Beta", y = "Count") +
    theme(plot.title = element_text(hjust = .5, face = "bold"))
  
  ggplotly(p)
}


# # Testing Functions ----
# ## setting file paths
# descriptions_path <- "raw_data_cereal_descriptions.xlsx"
# prices_path <- "raw_data_cereal_prices.xlsx"
# store_locations_path <- "demo.dta"
# us_locations_path <- "uszips.xlsx"
#
# descriptions_tbl <- input_descriptions(descriptions_path)
# prices_tbl <- input_prices(prices_path)
# store_locations_tbl <- input_store_locations(store_locations_path)
# us_locations_tbl <- input_us_locations(us_locations_path)
# filtered_store_locations_tbl <- get_store_locations(store_locations_tbl, us_locations_tbl)
# dates_tbl <- input_dates()
#
# top_three_brands_tbl <- get_top_three(descriptions_tbl, prices_tbl)
# 
# sales_tbl <- get_sales(descriptions_tbl, prices_tbl, filtered_store_locations_tbl, top_three_brands_tbl, dates_tbl)
# 
# sales_sample_tbl <- get_sales_sample(sales_tbl)
#
# plot_boxplot_sales(sales_tbl, "Brand Names", "Sales of Cereal Boxes", "Distribution of Sales by Brand")
# plot_boxplot_price(sales_tbl, "Brand Names", "Price of Cereal Boxes", "Distribution of Prices by Brand")
# 
# plot_violin_sales(sales_tbl, "Brand Names", "Sales of Cereal Boxes", "Distribution of Sales by Brand")
# plot_violin_price(sales_tbl, "Brand Names", "Price of Cereal Boxes", "Distribution of Prices by Brand")
#
# plot_histogram_sales(sales_tbl, "Sales of Cereal Boxes", "Distribution of Sales")
# plot_histogram_price(sales_tbl, "Price of Cereal Boxes", "Distribution of Prices")
# 
# plot_scatter(sales_sample_tbl)
# 
# plot_fitted_vs_residual(sales_sample_tbl, model = "REM")
# plot_fitted_vs_residual(sales_sample_tbl, model = "MEM")
# 
# bootstrap_tbl <- get_bootstrap(sales_tbl)
# bootstrap_tbl <- readRDS(file = "bootstrap_tbl.rds")
#
# ci <- get_ci_for_bootstrap(bootstrap_tbl)
# plot_bootstrap(bootstrap_tbl)
# 
# # Testing Function pt. 2 ----
# hchart(density((sales_tbl %>%
#                  filter(description == "CINNAMON TOAST CRUNC") %>%
#                  mutate(sales = log(sales)))$sales),
#        type = "area", name = "Cinammon Toast Crunch") %>%
#   hc_add_series(
#     density((sales_tbl %>%
#               filter(description == "KIX") %>%
#               mutate(sales = log(sales)))$sales),
#     type = "area",
#     name = "KIX"
#   ) %>%
#   hc_add_series(
#     density((sales_tbl %>%
#               filter(description == "WHEATIES") %>%
#               mutate(sales = log(sales)))$sales),
#     type = "area",
#     name = "Wheaties"
#   )
