
# loaded libraries
library(lubridate)

# data paths
prices_path <- "raw_data_cereal_prices.xlsx"
descriptions_path <- "raw_data_cereal_descriptions.xlsx"

# reading in the data paths
prices_tbl <- read_excel(prices_path)
descriptions_tbl <- read_excel(descriptions_path)

# created a dates tibble to match up with the week numbers
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

product_detail_tbl <- prices_tbl %>%
  inner_join(dates_tbl, by = c("WEEK" = "week")) %>%
  inner_join(descriptions_tbl) %>%
  mutate(Year = year(end),
         Sales = PRICE * MOVE / QTY) %>%
  filter(OK == 1,
         Sales > 0) %>%
  select(Year, DESCRIP, MOVE, PRICE, QTY, Sales, PROFIT) %>%
  mutate(
    DESCRIP = recode(
      DESCRIP,
      `APPLE CINNAMON CHEER` = "Apple Cinnamon Cheerios",
      `APPLE CINNAMON CHERR` = "Apple Cinnamon Cheerios",
      CHEERIOS = "Cheerios",
      `CINNAMON TOAST CRUNC` = "Cinnamon Toast Crunch",
      `G.M. FIBER ONE` = "Fiber One",
      `HONEY NUT CHEERIOS` = "Honey Nut Cheerios",
      KIX = "Kix",
      `NABISCO WHEAT N BRAN` = "Wheat 'N Bran",
      `SMORES CRUNCH CEREAL` = "S'mores Crunch",
      TOTAL = "Total",
      `TOTAL CORN FLAKES` = "Total Corn Flakes",
      `WHEATIES` = "Wheaties",
      `WHOLE GRAIN TOTAL` = "Whole Grain Total"
    ))

product_lookup_tbl <- product_detail_tbl %>%
  group_by(DESCRIP) %>%
  summarize(Distinct_Year = n_distinct(Year),
            Sample_Size = n()) %>%
  filter(Distinct_Year == 9)

product_total_tbl <- product_detail_tbl %>%
  inner_join(product_lookup_tbl, by = "DESCRIP") %>%
  group_by(Year) %>%
  summarize(Total_Revenue = sum(Sales),
            Total_GM = sum(PROFIT))

product_summary_tbl <- product_detail_tbl %>%
  inner_join(product_lookup_tbl , by = "DESCRIP") %>%
  group_by(Year, DESCRIP) %>%
  summarize(Unit_Sales = sum(MOVE), 
            Avg_Retail_Price = mean(PRICE), 
            Bundle_Sales = sum(QTY),
            Revenue = sum(Sales),
            Gross_Margin = sum(PROFIT)) %>%
  arrange(DESCRIP, Year) %>%
  left_join(product_total_tbl) %>%
  ungroup() %>%
  mutate(Revenue_Pct = Revenue / Total_Revenue,
         Gross_Margin_Pct = Gross_Margin / Total_GM,
         Unit_Sales_Growth = (Unit_Sales/dplyr::lag(Unit_Sales) - 1)) %>%
  select(Year, DESCRIP, Unit_Sales, Unit_Sales_Growth, Avg_Retail_Price, Bundle_Sales, Revenue, Revenue_Pct, Gross_Margin, Gross_Margin_Pct)
  
product_summary_wide_tbl <- product_summary_tbl %>%  
  pivot_longer(c(Unit_Sales:Gross_Margin_Pct), names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = Year, values_from = Value)

product_summary_tbl %>%
  ggplot(aes(x = Year, y = Revenue, fill = DESCRIP)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete(limits = seq(1989, 1997)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_discrete(name = "Cereal Brands")

product_summary_tbl %>%
  ggplot(aes(x = Year, y = Revenue, group = DESCRIP, color = DESCRIP)) +
  geom_line() +
  scale_x_discrete(limits = seq(1989, 1997)) +
  scale_color_discrete("Cereal Brands") +
  scale_y_continuous(labels = scales::dollar_format())

