library(tidyverse)
library(tsibble)
library(fable)
library(GGally)
library(feasts)


# making some fake data
set.seed(42)
date <- seq(as.Date("2010-01-01"), as.Date("2020-01-01"), "weeks")
# x <- rnorm(n = length(date), mean = 10, sd = 5)
# y <- x * 2 + 10 * rnorm(length(date))
time_series_data <- function(mean, sd) {
  # should make this more seasonal
  column <- c(0)
  for (i in 1:length(date)) {
    if (i %% 7 == 0) {
      column <- c(column, column[i - 1] + rnorm(1, 7, 2))
    } else{
      column <- c(column, column[i - 1] + rnorm(1, mean, sd))
    }
  }
  return(column)
}
current_assets <- time_series_data(2, 30)
current_liabilities <- time_series_data(1, 20)

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

































