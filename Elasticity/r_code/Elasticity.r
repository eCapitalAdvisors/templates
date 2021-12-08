#' ---
#' title: "Template: Elasticity"
#' author: "Lila Sahar"
#' date: "October 8, 2021"
#' output: github_document
#' ---
#' 
#' 

## Loading Data

## install.packages("tidyverse")
## install.packages("readxl")

library(tidyverse)
library(readxl)

## I am importing the xlsx files into R.
import_data <- function(filename) {
  read_excel(filename)
}

my_data_qualitative <- import_data("raw_data_cereal_descriptions.xlsx")
my_data_quantitative <- import_data("raw_data_cereal_prices.xlsx")

## I want to filter the data, so that I have smaller sample to work with.

## This steps is to figure out which cereal brand has the most UPC values.
 count_upc <- my_data_quantitative %>%
  filter(PRICE > 0) %>%
  filter(MOVE > 0) %>%
  group_by(UPC) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

## I have to join the count_upc and my_data_qualitative tables to figure out the names of the cereal brands, because
## before I only had the UPC.
count_upc %>%
  inner_join((my_data_qualitative %>%
                select(UPC, DESCRIP))) %>%
  summarize(total_count = sum(count)) %>%
  arrange(desc(total_count))

## I took the top three cereals that I found above and created a data sample that I wanted to work with for the rest
## of my project.
cereal <- my_data_quantitative %>%
  inner_join((my_data_qualitative %>%
                select(UPC, DESCRIP)%>%
                filter(DESCRIP %in% c("WHEATIES", "CINNAMON TOAST CRUNC", "KIX")))) %>%
                filter(PRICE > 0, NUM_BOXES_SOLD > 0)
  select(DESCRIP, MOVE, PRICE)

## I wanted the variable "MOVE" and "DESCRIP" to be more intuitive.
Num_Boxes_Sold <- cereal$MOVE
Brand_Name <- cereal$DESCRIP

#' I want to see if this will be a good data set to work with by
#' creating a box plot. Looks good.
#+ Boxplot
ggplot(data = cereal, mapping = aes(x = Brand_Name, y = log(PRICE), color = Brand_Name)) + 
  geom_boxplot()

#' I want to look at the distribution of the variables and the correlation between them.
#' The graphs will be histograms and scatter plot.
#' 

## Here I am picking 1000 random points to make this easier to look at the trend on the graphs.
cereal_sample <- cereal %>%
  group_by(Brand_Name) %>%
  sample_n(1000)

#+ Histogram of Number of Boxes Sold
ggplot(data = cereal, aes(x = log(Num_Boxes_Sold))) +
  geom_density(adjust = 5, aes(fill = Brand_Name), alpha = .8) + 
  labs(x = "Number of Cereal Boxes Sold", y = "Density", title = "Distribution of the Number of
       Cereal Boxes Sold", caption = "The x-values are transformed on a log scale.") +
  scale_fill_discrete(name = "Brand Names", labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) +
  theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"))

#+ Histogram of Prices of the Cereal Boxes
ggplot(data = cereal, aes(x = log(PRICE))) + 
  geom_density(adjust = 5, aes(fill = Brand_Name), alpha = .8) + 
  xlim(0, 2) +
  labs(x = "Price of Cereal Boxes Sold", y = "Density", title = "Distribution of the Price of
       Cereal Boxes Sold", caption = "The x-values are transformed on a log scale.") +
  scale_fill_discrete(name = "Brand Names", labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) +
  theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"))

#+ Scatter Plot
ggplot(data = cereal_sample, aes(x = log(PRICE), y = log(MOVE))) +
  geom_point(aes(color = DESCRIP), alpha = .8) +
  xlim(0, 1.75) +
  labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
  title = "Price vs. Box Sales", caption = "The x and y values
       are transformed on a log scale.") +
  scale_color_discrete(name = "Brand Names", labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) +
  theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"))
