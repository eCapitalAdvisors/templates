library(tidyverse)
library(tsibble)
library(fable)
library(GGally)
library(feasts)

# assumes data is combined in Incorta
# df <- read_csv()
df <- data.frame(date, current_assets, current_liabilities)

# current assets and liabilities, not exhaustive
# df$current_assets <- df$cash + df$accounts_receivable + df$inventory
# df$current_liabilities <- df$accouts_payable + df$accrued_expenses
# + df$short_term_debt + df$current_long_term_debt

# convert to tsibble and add working capital and current ratio
work_cap <- df %>%
  as_tsibble(index = date) %>%
  mutate(
    working_capital = current_assets - current_liabilities,
    current_ratio = current_assets / current_liabilities,
  )

# plotting
ggplot(work_cap, mapping = aes(x = date, y = working_capital)) +
  geom_point()

# lag plot of time series data
gg_lag(work_cap, working_capital, geom = "point") +
  labs(x = "lag(Working Capital, k)")

# scatterplot matrix
ggpairs(work_cap, aes(color = "blue", alpha = 0.8))


# autocorrelation
# https://otexts.com/fpp3/acf.html#acf
work_cap %>%
  ACF(working_capital) %>%
  autoplot()

# checking seasonality and trend
# https://otexts.com/fpp3/stl.html
work_cap %>%
  model(STL(working_capital ~ trend(window = 21) +
              season(window = 13),
            robust = TRUE)) %>%
  components() %>%
  autoplot()

# more specific seasonality
work_cap %>%
  gg_tsdisplay(difference(working_capital),
               plot_type = 'partial',
               lag = 36) + labs(title = 'seasonal')

# differencing
ggplot(work_cap, aes(date, difference(working_capital))) +
  geom_line()

# simulating models
simulation <- generate(full_fit, h = 36, times = 15) %>%
  filter(.model == 'auto')
df %>%
  filter(row_number() >= n() - 150) %>%
  autoplot(working_capital) + autolayer(simulation, .vars = .sim) +
  geom_line() +
  theme(legend.position = "none") +
  ggtitle("Working Capital Simulation") +
  labs(y = "Working Capital (millions)", x = "Date")

# errors for model
forecast(arima_fit, h = "3 year") %>%
  accuracy(data = df, measures = c(crps = CRPS, rmse = RMSE))

# STL decomp individual
stl_decomp <- model(work_cap, STL(working_capital ~ trend(window = 21) + season(window=13), robust = TRUE))
autoplot(work_cap, working_capital) + autolayer(components(stl_decomp), trend + season_year, color = "blue")

# cross validation
cross_validate <- function(data, set_length, steps) {
  # set_length is the starting training set length
  data %>%
    stretch_tsibble(.init = set_length, .step = steps)
}

# plots 3 year hier forecast
three_yr_forecast %>%
  autoplot(hier_data %>% filter(year(date) >= 2001))
# provides accuracy measures for hier forecasts
accuracy(three_yr_forecast, hier_data, measures = c(crps = CRPS, mae = MAE, mape = MAPE)) %>% 
  arrange(desc(name), desc(crps), desc(mae))

# uses lots of different models on hierarchical data
train_fit <- hier_data %>%
  filter(year(date) <= 2001) %>%
  model(arima = ARIMA(working_capital),
        ets = ETS(working_capital),
        snaive = SNAIVE(working_capital ~ drift())) %>%
  reconcile(mint_arima = min_trace(arima, method = "mint_shrink"),
            mint_ets = min_trace(ets, method = "mint_shrink"),
            mint_snaive = min_trace(snaive, method = "mint_shrink"))






