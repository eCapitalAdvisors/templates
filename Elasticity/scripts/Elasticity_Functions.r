#' ---
#' title: "Template: Elasticity Functions"
#' author: "Lila Sahar and Juan Malaver"
#' date: "December 17, 2021"
#' output: github_document
#' ---
#' 

# PRICE ELASTICITY APP ----
# BOOTSTRAP REGRESSION MODEL ----

# GOAL: BUILD PREDICTION MODEL FOR PRICING ALGORITHM


# 1.0 LIBRARIES & DATA ----

# standard
library(tidyverse)
library(broom)
library(rsample) #bootstrap function
library(infer)
library(lubridate)
library(readxl)

# visuals
library(plotly)
library(geojson)
library(geojsonio)
library(haven)
library(mapproj)
library(sp)
library(ggrepel)

## Read Data

descriptions_path <- "raw_data_cereal_descriptions.xlsx"
prices_path <- "raw_data_cereal_prices.xlsx"
store_locations_path <- "demo.dta"
us_locations_path <- "uszips.xlsx"
illinois_map_path <- "https://raw.githubusercontent.com/empet/Datasets/master/illinois-election.geojson"


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
    select(city, zip, lat, long, store) %>%
    filter(city != "") %>%
    mutate(city = str_to_title(city)) %>%
    mutate(lat = format(lat / 10000, nsmall = 4)) %>%
    mutate(long = format(long / 10000, nsmall = 4))
  
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

# creating a function to go along with the data according to Dominick's manual
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


# 2.2 Creating a Map Template ----

# reading in a map from online
input_illinois_map <- function(illinois_map_path) {
  illinois_map <- geojson_read(illinois_map_path, what = "sp")
  
  saveRDS(object = illinois_map, file = "../R/illinois_map.rds")
  
  return(illinois_map)
}


# 2.3 Joining the Tables ----

# joining tables
get_store_locations <-
  function(store_locations_tbl, us_locations_tbl) {
    # This table tells us all the unique store locations
    filtered_store_locations_tbl <- store_locations_tbl %>%
      left_join(us_locations_tbl)
    
    saveRDS(object = filtered_store_locations_tbl, file = "../R/filtered_store_locations_tbl.rds")
    
    return(filtered_store_locations_tbl)
  }

# this table discovers the three brands that have the most data in the dataset
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


# this table joins all the relevant tables
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
      select(start,
             end,
             price,
             sales,
             description,
             city,
             zip,
             lat,
             long,
             state_name) %>%
      mutate(revenue = price * sales)
    
    saveRDS(object = top_three_brands_tbl, file = "../R/sales_tbl.rds")
    
    return(sales_tbl)
  }


# 2.4 Modified Datasets ----

# this table takes a sample of 1000 for the bootstrap
get_sales_sample <- function(sales_tbl) {
  #selecting a sample from each brand
  sales_sample_tbl <- sales_tbl %>%
    group_by(description) %>%
    sample_n(1000)
  
  saveRDS(object = sales_sample_tbl, file = "../R/sales_sample_tbl.rds")
  
  return(sales_sample_tbl)
}

# this table is for the visuals; it contains descriptive data
get_total <- function(sales_tbl) {
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
  
  return(total_tbl)
}


# 2.5 Save Functions ----

dump(c("input_descriptions", "input_prices", "input_us_locations", "input_dates"),
     file = "../R/cleaning_data.R")

dump("input_illinois_map",
     file = "../R/map_template.R")

dump(c("get_store_locations", "get_top_three", "get_sales"),
     file = "../R/joining_tables.R")

dump(c("get_sales_sample", "get_total"),
     file = "../R/modified_datasets.R")


# 3.0 BOOTSTRAP MODEL ----

# 3.1 Create Model ----

get_bootstrap <- function(sales_tbl) {
  
  # bootstrap
  sales_tbl %>%
    mutate(sales = log(sales), price = log(price)) %>%
    specify(formula = sales ~ price) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "slope")
}

get_betas <- function(sales_tbl) {
  
  # obtain betas
  bootstrap_tbl <- sales_tbl %>%
    group_by(description) %>%
    nest() %>%
    mutate(bootstrap_slopes = purrr::map(data, get_bootstrap))
  
  saveRDS(object = bootstrap_tbl, file = "../R/bootstrap_tbl.rds")
  
  return(bootstrap_tbl)
}
  
# obtain confidence interval for bootstrap
get_ci_for_bootstrap <- function(bootstrap_tbl) {
  
  ci <- bootstrap_tbl %>%
    mutate(perc_ci = purrr::map(bootstrap_slopes, get_confidence_interval, level = 0.95, type = 'percentile')) %>%
    unnest(perc_ci)
  
  saveRDS(object = ci, file = "../R/ci.rds")
  
  return(ci)
}

# 3.2 Save Functions ----

dump(c("get_bootstrap", "get_betas", "get_ci_for_bootstrap"),
     file = "../R/creation_of_model.R")


# 4.0 VISUALS: UNDERSTAND THE DATA ----

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

plot_bootstrap <- function(bootstrap_tbl) {
  p <- ggplot(bootstrap_tbl %>% filter(term == "price") %>% select(replicate, estimate), aes(estimate)) +
    geom_density() + 
    geom_vline(xintercept = ci %>% filter(term == "price") %>% pull(lower_ci), linetype = "dotted", color = "red") +
    geom_vline(xintercept = ci %>% filter(term == "price") %>% pull(upper_ci), linetype = "dotted", color = "red") +
    labs(title = "Bootstrap of Means", x = "Estimates of Beta", y = "Count") +
    theme(plot.title = element_text(hjust = .5, face = "bold"))
  
  ggplotly(p)
}

# 4.1 Save Functions ----

dump(c("plot_boxplot_sales", "plot_boxplot_price", "plot_histogram_sales", 
       "plot_histogram_price", "plot_scatter", "plot_bootstrap"),
     file = "../R/visual_understand_data.R")


# 5.0 VISUALS: OVERVIEW OF THE DATA ----

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

plot_total_revenue_bar <- function(total_tbl) {
  ggplot(data = total_tbl, aes(x = city, y = total_revenue, color = description)) +
    geom_line() +
    geom_point() +
    labs(title = "Total Revenue by City", x = "City", y = "Total Revenue") +
    guides(color = guide_legend("Brand Name")) +
    theme(plot.title = element_text(hjust = .5, face = "bold"))
}

plot_total_revenue_line <- function(total_tbl) {
  ggplot(data = total_tbl, aes(x = `year(start)`, y = total_revenue, color = description)) +
    geom_line() +
    geom_point() +
    labs(title = "Total Revenue per Year", x = "Year", y = "Total Revenue") +
    guides(color = guide_legend("Brand Name")) +
    theme(plot.title = element_text(hjust = .5, face = "bold"))
}

illinois_map_fortified <- tidy(illinois_map)

ggplot() +
  geom_polygon(data = illinois_map_fortified, aes(x = long, y = lat, group = group), fill = "#69b3a2", color = "white") +
  geom_point(data = sales_tbl, aes(x = long, y = lat, size = revenue, color = revenue)) + 
  theme_void() +
  coord_map()

# 5.1 Save Functions ----

dump(c("plot_boxplot_sales", "plot_boxplot_price", "plot_histogram_sales", 
       "plot_histogram_price", "plot_scatter", "plot_bootstrap"),
     file = "../R/visual_understand_data.R")


# 6.0 VISUALS: ELASTICITY ANALYSIS ----

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

# 6.1 Save Functions ----

dump("plot_fitted_vs_residual",
     file = "../R/visual_elasticity_analysis.R")


# # Testing Functions ----
# ## setting file paths
# descriptions_path <- "raw_data_cereal_descriptions.xlsx"
# prices_path <- "raw_data_cereal_prices.xlsx"
# store_locations_path <- "demo.dta"
# us_locations_path <- "uszips.xlsx"
# illinois_map_path <- "https://raw.githubusercontent.com/empet/Datasets/master/illinois-election.geojson"
#
# descriptions_tbl <- input_descriptions(descriptions_path)
# prices_tbl <- input_prices(prices_path)
# store_locations_tbl <- input_store_locations(store_locations_path)
# us_locations_tbl <- input_us_locations(us_locations_path)
# filtered_store_locations_tbl <- get_store_locations(store_locations_tbl, us_locations_tbl)
# dates_tbl <- input_dates()
# illinois_map <- input_illinois_map(illinois_map_path)
#
# top_three_brands_tbl <- get_top_three(descriptions_tbl, prices_tbl)
# sales_tbl <- get_sales(descriptions_tbl, prices_tbl, filtered_store_locations_tbl, top_three_brands_tbl, dates_tbl)
# sales_sample_tbl <- get_sales_sample(sales_tbl)
# total_tbl <- get_total(sales_tbl)
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
# plot_avg_revenue_line(average_tbl)
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
