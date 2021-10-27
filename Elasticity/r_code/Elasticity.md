R scripts can be rendered!
================
Jenny Bryan
April 1, 2014

``` r
## Created by lila sahar

## Loading Data

## install.packages("readxl")
## install.packages("tidyverse")
library(readxl)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.5     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.2     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
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
```

    ## Joining, by = "UPC"

    ## # A tibble: 1 x 1
    ##   total_count
    ##         <int>
    ## 1      752080

``` r
## I took the top three cereals and created a new variable
cereal <- my_data_quantitative %>%
  inner_join((my_data_qualitative %>%
                select(UPC, DESCRIP)%>%
                filter(DESCRIP %in% c("WHEATIES", "CINNAMON TOAST CRUNC", "KIX")))) %>%
  select(DESCRIP, MOVE, PRICE)
```

    ## Joining, by = "UPC"

I want to see if this will be a good data set to work with by creating a
box plot. Looks good.

``` r
ggplot(data = cereal, mapping = aes(x = DESCRIP, y = log(PRICE), color = DESCRIP)) + 
  geom_boxplot()
```

    ## Warning: Removed 12470 rows containing non-finite values (stat_boxplot).

![](Elasticity_files/figure-gfm/Boxplot-1.png)<!-- -->
