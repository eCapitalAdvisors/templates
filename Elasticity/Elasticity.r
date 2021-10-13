#' ---
#' title: "Template: Elasticity"
#' author: "Lila Sahar"
#' date: "October 8, 2021"
#' output: github_document
#' ---
#' 
#' 

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
  filter(MOVE > 0) %>%
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
                filter(log(PRICE) > 0, log(NUM_BOXES_SOLD) > 0)
  select(DESCRIP, MOVE, PRICE)

## I wanted the variable "MOVE" to be more intuitive.
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

cereal_sample <- cereal %>%
  group_by(Brand_Name) %>%
  sample_n(1000)

ggplot(data = cereal, aes(x = log(Num_Boxes_Sold))) +
  geom_density(adjust = 5, aes(fill = Brand_Name), alpha = .8) + 
  labs(x = "Number of Cereal Boxes Sold", y = "Density", title = "Distribution of the Number of
       Cereal Boxes Sold", caption = "The x-values are transformed on a log scale.") +
  scale_fill_discrete(name = "Brand Names", labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) +
  theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"))

ggplot(data = cereal, aes(x = log(PRICE))) + 
  geom_density(adjust = 5, aes(fill = Brand_Name), alpha = .8) + 
  xlim(0, 2) +
  labs(x = "Price of Cereal Boxes Sold", y = "Density", title = "Distribution of the Price of
       Cereal Boxes Sold", caption = "The x-values are transformed on a log scale.") +
  scale_fill_discrete(name = "Brand Names", labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) +
  theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"))

ggplot(data = cereal_sample, aes(x = log(PRICE), y = log(MOVE))) +
  geom_point(aes(color = DESCRIP), alpha = .8) +
  xlim(0, 1.75) +
  labs(x = "Price of Cereal Box", y = "Number of Cereal Boxes Sold",
  title = "Price vs. Box Sales", caption = "The x and y values
       are transformed on a log scale.") +
  scale_color_discrete(name = "Brand Names", labels = c("Cinnamon Toast Crunch", "KIX", "Wheaties")) +
  theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold"))
