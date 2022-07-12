library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(lubridate)

df <-  read_csv("C:/Users/AxelTorbenson/OneDrive - eCapital Advisors, LLC/Documents/templates/CapitalForecasting/Data/working_capital_fake_data.csv")

create_tsibble <- function(data, date){
  data$date <- as.Date(as.character(data$date), format = "%m/%d/%Y")
  data %>%
    mutate(date = yearmonth(as.character(date))) %>%
    as_tsibble(index = date)   
}

create_hierarchy <- function(data){
  data %>% 
    pivot_longer(!date, values_to = "dollars") %>% 
    aggregate_key(name, working_capital = sum(dollars))
}

hier_data <- create_tsibble(df, date) %>% 
  mutate(liabilities = -liabilities) %>% 
  create_hierarchy()

fit <- hier_data %>%
  filter(year(date) <= 2001) %>%
  model(arima = ARIMA(working_capital),
        ets = ETS(working_capital),
        snaive = SNAIVE(working_capital ~ drift())) %>%
  reconcile(mint_arima = min_trace(arima, method = "mint_shrink"),
            mint_ets = min_trace(ets, method = "mint_shrink"),
            mint_snaive = min_trace(snaive, method = "mint_shrink"))

three_yr_forecast <- fit %>% forecast(h = "3 years 3 months")

three_yr_forecast %>%
  autoplot(hier_data %>% filter(year(date) >= 2001))

accuracy(three_yr_forecast, hier_data, measures = c(crps = CRPS, mae = MAE, mape = MAPE)) %>% 
  arrange(desc(name), desc(crps), desc(mae))



