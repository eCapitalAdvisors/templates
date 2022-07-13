library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(lubridate)

# sample data
df <-  read_csv("C:/Users/AxelTorbenson/OneDrive - eCapital Advisors, LLC/Documents/templates/CapitalForecasting/Data/working_capital_fake_data.csv")

create_tsibble <- function(data, date) {
  data$date <- as.Date(as.character(data$date), format = "%m/%d/%Y")
  data %>%
    mutate(date = yearmonth(as.character(date))) %>%
    as_tsibble(index = date)
}

create_hierarchy <- function(data) {
  # working capital is aggregated from assets and liabilities
  data %>%
    pivot_longer(!date, values_to = "dollars") %>%
    aggregate_key(name, dollars = sum(dollars))
}

hierarchical_modeling <- function(data, train_split_date) {
  data %>% 
    filter(year(date) <= train_split_date) %>% 
    model(arima = ARIMA(dollars)) %>% 
    # asset forecast minus liabilities forecast equals working capital forecast
    reconcile(mint_arima = min_trace(arima, method = "mint_shrink")) %>% 
    refit(data)
}

# creates hierarchical data
hier_data <- create_tsibble(df, date) %>% 
  mutate(liabilities = -liabilities) %>% 
  create_hierarchy()

# modeling
full_fit <- hierarchical_modeling(hier_data, 2001)

# fits three year forcase and cleans up tsibble
three_yr_forecast <- full_fit %>%
  forecast(h = "3 years") %>%
  filter(.model == "mint_arima") %>%
  mutate(name = recode(as.factor(name), "<aggregated>" = "working_capital"),
         type = "prediction") %>%
  rename(distribution = dollars, dollars = .mean, model = .model)

# cleans up tsibble
hier_data <- hier_data %>% 
  mutate(name = recode(as.factor(name), "<aggregated>" = "working_capital"),
         model = NA,
         distribution = NA,
         type = "actual")

# full data including predictions and actuals
full_df <- union_all(hier_data, three_yr_forecast)
