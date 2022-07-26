library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(feasts)

google_gl <- read_csv("C:\\Users\\AxelTorbenson\\OneDrive - eCapital Advisors, LLC\\Documents\\templates\\CapitalForecasting\\Data\\goog_gl.csv")

gl <- google_gl %>%
  select(
    # date and totals
    fiscalDateEnding,
    totalCurrentAssets,
    totalCurrentLiabilities,
    # assets
    inventory,
    cashAndCashEquivalentsAtCarryingValue, 
    currentNetReceivables,
    shortTermInvestments,
    otherCurrentAssets,
    # liabilities
    shortTermDebt,
    currentAccountsPayable,
    otherCurrentLiabilities) %>%
  mutate(workingCapital = totalCurrentAssets - totalCurrentLiabilities,
         otherCurrentAssets = totalCurrentAssets
         - inventory
         - cashAndCashEquivalentsAtCarryingValue
         - currentNetReceivables 
         - shortTermInvestments,
         # otherCurrentLiabilities includes taxes, deferred revenue, capital lease obligations, and other
         otherCurrentLiabilities = totalCurrentLiabilities
         - shortTermDebt 
         - currentAccountsPayable,
         # NA is equivalent to 0
         across(where(is.numeric), ~replace(., is.na(.), 0)),
         # Make data in millions
         across(c(2:12), .fns = ~./1000000),
         # Liabilities are negative
         across(c("shortTermDebt", "currentAccountsPayable", "otherCurrentLiabilities"), .fns = ~. * -1)) %>% 
  select(-workingCapital, -totalCurrentAssets, -totalCurrentLiabilities) %>% 
  pivot_longer(!fiscalDateEnding, values_to = "dollars") %>%
  group_by(name) %>%
  mutate(accountType = case_when(
    name %in% c(
      "inventory",
      "cashAndCashEquivalentsAtCarryingValue",
      "currentNetReceivables",
      "shortTermInvestments",
      "otherCurrentAssets"
    ) ~ "asset",
    name %in% c(
      "shortTermDebt",
      "currentAccountsPayable",
      "otherCurrentLiabilities"
    ) ~ "liability")
  )

df <- gl %>%
  mutate(fiscalDateEnding = yearmonth(fiscalDateEnding)) %>%
  as_tsibble(key = dollars, index = fiscalDateEnding) %>%
  aggregate_key((accountType / name), dollars = sum(dollars)) 

ind_models <- df %>% 
  # withholding 5 quarters of data (training)
  filter(year(fiscalDateEnding) <= 2020) %>%
  # AICc is better for short time series
  model(arima = ARIMA(dollars, ic = "aicc"),
        ets = ETS(dollars, ic = "aicc"),
        naive = NAIVE(dollars ~ drift()))

# This combination model is just to get the error of the model
comb_forecast_accuracy <- ind_models %>%
  mutate(comb = (arima + ets + naive) / 3) %>%
  reconcile(mint_comb = min_trace(comb, method = "mint_shrink")) %>% 
  forecast(h = 5) %>% 
  filter(.model == "mint_comb") %>% 
  # MAPE is infinity when a series has 0 in it
  accuracy(data = df, measures = c(mape = MAPE, mae = MAE, mase = MASE))


refit_comb <- ind_models %>%
  refit(df) %>%
  mutate(comb = (arima + ets + naive) / 3) %>%
  reconcile(mint_comb = min_trace(comb, method = "mint_shrink")) %>% 
  select(accountType, name, mint_comb)

# refits forecast and uses newest data
refit_forecast <- refit_comb %>% 
  forecast(h = 5) %>% 
  hilo(95) %>%
  as_tibble() %>%
  select(fiscalDateEnding, accountType, name, .mean, '95%') %>%
  rename(dollars = .mean) %>%
  rename(conf_int = '95%') %>%
  unpack_hilo(conf_int) %>% 
  mutate(type = "forecast")

df_final <- df %>%
  mutate(conf_int_lower = NA,
         conf_int_upper = NA,
         type = "actual")


full_df = union_all(refit_forecast, df_final) %>% 
  mutate(accountType = replace(accountType, accountType == "<aggregated>", "aggregated"))


full_df
