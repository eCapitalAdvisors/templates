## loading libraries
library(tidyverse)
library(plotly)
library(broom)
library(infer)
library(readxl)

bootstrap_tbl <- sales_tbl %>%
  mutate(sales = log(sales), price = log(price)) %>%
  specify(formula = sales ~ price + description) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  fit()

bootstrap_tbl %>%
  group_by(term) %>%
  nest() %>%
  mutate(perc_ci = map(
   data,
   get_confidence_interval,
   level = 0.95,
   type = "percentile"
  )) %>%
  unnest(perc_ci)