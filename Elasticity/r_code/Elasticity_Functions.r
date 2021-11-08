#' ---
#' title: "Template: Elasticity Functions"
#' author: "Lila Sahar and Juan Malaver"
#' date: "October 15, 2021"
#' output: github_document
#' ---
#' 
#' 

## loading libraries
library(tidyverse)
library(plotly)
library(broom)
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

plot_boxplot_sales <- function(sales_tbl, x_title, y_title, title_chart) {
  
  #graphing a box plot
  p <- ggplot(data = sales_tbl, aes(x = description, y = log(sales), color = description)) +
    geom_boxplot() +
    labs(x = x_title, y = y_title, title = title_chart, caption = "The y-values are transformed on a log scale.") +
    scale_x_discrete(labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) +
    theme(plot.title = element_text(hjust = .5), plot.caption = element_text(hjust = .5)) +
    coord_flip()
  
  hide_legend(ggplotly(p))
  }

plot_boxplot_price <- function(sales_tbl, x_title, y_title, title_chart) {
  
  #graphing a box plot
  p <- ggplot(data = sales_tbl, aes(x = description, y = log(price), color = description)) +
    geom_boxplot() +
    labs(x = x_title, y = y_title, title = title_chart, caption = "The y-values are transformed on a log scale.") +
    scale_x_discrete(labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) +
    theme(plot.title = element_text(hjust = .5), plot.caption = element_text(hjust = .5)) +
    coord_flip()

  hide_legend(ggplotly(p))
  }

plot_histogram_sales <- function(sales_tbl, x_title, title_chart){
  
  #graphing a histogram for sales
  p <- ggplot(data = sales_tbl, aes(x = log(sales), fill = description)) + 
    geom_density(adjust = 5, aes(x = log(sales), fill = description), alpha = .8) +
    labs(x = x_title, y = "Density", title = title_chart, caption = "The x-values are transformed on a log scale.") +
    scale_fill_discrete(name = "Brand Names", labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) + 
    theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))

  ggplotly(p)
  }

plot_histogram_price <- function(sales_tbl, x_title, title_chart){
  
  #graphing a histogram for price
  p <- ggplot(data = sales_tbl, aes(x = log(price))) + 
    geom_density(adjust = 5, aes(fill = description), alpha = .8) +
    xlim(0, 2) + 
    labs(x = x_title, y = "Density", title = title_chart, caption = "The x-values are transformed on a log scale.") +
    scale_fill_discrete(name = "Brand Names", labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) + 
    theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))

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
           title = "Price vs. Box Sales", caption = "The x and y values
       are transformed on a log scale.") +
      theme(legend.position = "None", plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))
    
    ggplotly(p) 
  }
  else if (model == "RIRS") {
    mod <- glm(log(sales) ~ log(price) * description, data = sales_sample_tbl)
    
    p <- ggplot(data = cbind(sales_sample_tbl, pred = predict(mod)), aes(x = log(price), y = log(sales))) +
      geom_point(col = "gray", alpha = .8) +
      geom_line(aes(y = pred, color = description), size = 1) + 
      xlim(0, 1.75) +
      labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
           title = "Price vs. Box Sales", caption = "The x and y values
       are transformed on a log scale.") +
      theme(legend.position = "None", plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))
    
    ggplotly(p)
  }
  else {
    p <- ggplot(data = sales_sample_tbl, aes(x = log(price), y = log(sales))) +
      geom_point(aes(color = description), alpha = .8) +
      xlim(0, 1.75) +
      labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
           title = "Price vs. Box Sales", caption = "The x and y values
       are transformed on a log scale.") +
      theme(legend.position = "None", plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"), plot.caption = element_text(hjust = .5))
    
    ggplosalestly(p) 
  }
}

plot_fitted_vs_residual <- function(sales_sample_tbl, model = "none") {
  
  if (model == "REM") {
    lmfit <- glm(log(sales) ~ log(price) + description, data = sales_sample_tbl)
    
    p <- ggplot(sales_sample_tbl, aes(lmfit$fitted.values, lmfit$residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Fitted vs Residuals", x = "Residuals", y = "Fitted Values") +
      theme(plot.title = element_text(hjust = .5))
    
    ggplotly(p)
  }
  
  else if (model == "MEM") {
    lmfit <- glm(log(sales) ~ log(price) * description, data = sales_sample_tbl)
    
    p <- ggplot(sales_sample_tbl, aes(lmfit$fitted.values, lmfit$residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Fitted vs Residuals", x = "Residuals", y = "Fitted Values") +
      theme(plot.title = element_text(hjust = .5))
    
    ggplotly(p)
  }
}

#DO IT
#plot_bootstrap_coefficient <- function() {
#  plot()
#}

# # Testing Functions ----
# ## setting file paths
# descriptions_path <- "raw_data_cereal_descriptions.xlsx"
# prices_path <- "raw_data_cereal_prices.xlsx"
# 
# descriptions_tbl <- input_descriptions(descriptions_path)
# prices_tbl <- input_prices(prices_path)
# 
# top_three_brands_tbl <- get_top_three(descriptions_tbl, prices_tbl)
# 
# sales_tbl <- get_sales(descriptions_tbl, prices_tbl, top_three_brands_tbl)
# 
# sales_sample_tbl <- get_sales_sample(sales_tbl)
#
# plot_boxplot_sales(sales_tbl, "Brand Names", "Sales of Cereal Boxes", "Distribution of Sales by Brand")
# plot_boxplot_price(sales_tbl, "Brand Names", "Price of Cereal Boxes", "Distribution of Prices by Brand")
# 
# plot_histogram_sales(sales_tbl, "Sales of Cereal Boxes", "Distribution of Sales")
# plot_histogram_price(sales_tbl, "Price of Cereal Boxes", "Distribution of Prices")
# 
# plot_scatter(sales_sample_tbl)
# 
# plot_fitted_vs_residual(sales_sample_tbl, model = "REM")
# plot_fitted_vs_residual(sales_sample_tbl, model = "MEM")
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
