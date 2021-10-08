#' ---
#' title: "R scripts can be rendered!"
#' author: "Jenny Bryan"
#' date: "April 1, 2014"
#' output: github_document
#' ---
#' 
#' 

## Created by lila sahar

## Loading Data

## install.packages("readxl")
## install.packages("tidyverse")
library(readxl)
library(tidyverse)

## I am importing the xlsx files into R.
my_data_qualitative <- read_excel("raw_data_cereal_descriptions.xlsx")
my_data_quantitative <- read_excel("raw_data_cereal_prices.xlsx")

## I want to get a better idea of what our data looks like.

## I got rid of the rows with no price and group the UPCs
count_upc <- my_data_quantitative %>%
  filter(PRICE > 0) %>%
  group_by(UPC) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

## I joined the quantitative and qualitative tables
count_upc %>%
  inner_join((my_data_qualitative %>%
                select(UPC, DESCRIP))) %>%
  summarize(total_count = sum(count)) %>%
  arrange(desc(total_count))

## I took the top three cereals and created a new variable
cereal <- my_data_quantitative %>%
  inner_join((my_data_qualitative %>%
                select(UPC, DESCRIP)%>%
                filter(DESCRIP %in% c("WHEATIES", "CINNAMON TOAST CRUNC", "KIX")))) %>%
  select(DESCRIP, MOVE, PRICE)

#' I want to see if this will be a good data set to work with by
#' creating a box plot. Looks good.
#+ Boxplot
ggplot(data = cereal, mapping = aes(x = DESCRIP, y = log(PRICE), color = DESCRIP)) + 
  geom_boxplot()