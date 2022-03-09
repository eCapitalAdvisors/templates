# libraries
library(tidyverse)
library(lubridate)
library(haven)

# file paths
prices_path <- "../data/wcer.csv"
descriptions_path <- "../data/upccer.csv"
store_locations_path <- "../data/demo.dta"

# importing xlxs files
descriptions_tbl <- read_csv(descriptions_path)
store_locations_tbl <- read_dta(store_locations_path)

# week date lookup table
date_tbl <- list("START" = seq(ymd("1989-09-14"),
                               ymd("1997-05-08"),
                               by = "weeks")) %>%
  as_tibble() %>%
  mutate(END = START + days(6),
         YEAR = year(END),
         WEEK = row_number()) %>%
  relocate(WEEK)

# product transactional detail table
read_csv(prices_path) %>%
  inner_join(descriptions_tbl, by = "UPC") %>%
  inner_join(store_locations_tbl %>% select(store, city), by = c("STORE" = "store")) %>%
  inner_join(date_tbl, by = "WEEK") %>%
  rename(CITY = city) %>%
  mutate(REVENUE = PRICE * MOVE / QTY,
         CITY = str_to_title(CITY)) %>%
  filter(OK == 1, REVENUE > 0, CITY != "") %>%
  select(YEAR, DESCRIP, CITY, MOVE, PRICE, QTY, REVENUE, PROFIT) %>%
  mutate(
    DESCRIP = recode(
      DESCRIP,
      `~NUTRI GRAIN ALMOND` = "Nutri Grain Almond",
      `APPLE CINNAMON CHEER` = "Apple Cinnamon Cheerios",
      `APPLE CINNAMON CHERR` = "Apple Cinnamon Cheerios",
      `BITE SIZE FRSTD MINI` = "Bite Size Frosted Mini-Wheats",
      `CAP'N CRUNCH CHRISTM` = "Cap'n Crunch Christmas",
      `CAPN CRUNCH CEREAL` = "Cap'n Crunch",
      `CAPN CRUNCH JUMBO CR` = "Cap'n Crunch Jumbo",
      `CAPTAIN CRUNCH JUMBO` = "Cap'n Crunch Jumbo",
      CHEERIOS = "Cheerios",
      `CINNAMON TOAST CRUNC` = "Cinnamon Toast Crunch",
      `COCOA PUFFS` = "Cocoa Puffs",
      `COOKIE CRISP CHOCOLA` = "Cookie Crisp",
      `COUNT CHOCULA CEREAL` = "Count Chocula",
      `CRISPY WHEATS & RAIS` = "Crispy Wheats & Raisins",
      `DOM OAT HON RAISIN` = "Dominick's Oat Honey Raisin",
      `DOM OAT HONEY` = "Dominick's Oat Honey",
      `G.M. FIBER ONE` = "Fiber One",
      `GENERAL MILLS KABOOM` = "Kaboom",
      `GOLDEN GRAHAMS` = "Golden Grahams",
      `HONEY BUNCHES OATS-A` = "Honey Bunches of Oats with Almonds",
      `HONEY BUNCHES OATS R` = "Honey Bunches of Oats with Raisins",
      `HONEY NUT CHEERIOS` = "Honey Nut Cheerios",
      `KELL FROST MINI WHTS` = "Frosted Mini-Wheats",
      `KELLOGG'S CORN POPS` = "Corn Pops",
      `KELLOGG'S CRACKLIN O` = "Cracklin' Oat Bran",
      `KELLOGG'S CRISPIX` = "Crispix",
      `KELLOGG'S FROSTED FL` = "Frosted Flakes",
      `KELLOGG'S RAISIN BRA` = "Raisin Bran",
      `KELLOGG FROOT LOOPS` = "Froot Loops",
      `KELLOGG FROSTED FLAK` = "Frosted Flakes",
      `KELLOGGS ALL BRAN` = "All-Bran",
      `KELLOGGS APPLE CINNA` = "Special K Apple Cinnamon",
      `KELLOGGS APPLE JACKS` = "Apple Jacks",
      `KELLOGGS BLUEBERRY S` = "Special K Blueberry",
      `KELLOGGS COCOA KRISP` = "Cocoa Krispies",
      `KELLOGGS CORN FLAKE` = "Corn Flakes",
      `KELLOGGS CORN FLAKES` = "Corn Flakes",
      `KELLOGGS CORN POPS` = "Corn Pops",
      `KELLOGGS CRISPIX` = "Crispix",
      `KELLOGGS FRUIT LOOPS` = "Froot Loops",
      `KELLOGGS HONEY SMACK` = "Honey Smacks",
      `KELLOGGS JUST RT FRT` = "Just Right",
      `KELLOGGS NUT & HONEY` = "Nut & Honey Crunch",
      `KELLOGGS NUTRI GRAIN` = "Nutri-Grain",
      `KELLOGGS PRODUCT 19` = "Product 19",
      `KELLOGGS RAISIN BRAN` = "Raisin Bran",
      `KELLOGGS RICE KRISPI` = "Rice Krispies",
      `KELLOGGS SPECIAL K` = "Special K",
      `KELLOGGS STRAWBERRY` = "Special K Strawberry",
      `KELLOGGS SUGAR FROST` = "Frosted Flakes",
      `KELLOGGS VARIETY PAC` = "Kellogg's Variety Pack",
      `KELLOGS RAISIN SQUAR` = "Raisin Squares",
      KIX = "Kix",
      `KLG JST RT FIBER NGT` = "Just Right",
      `KLLGG SGR FRSTD MINI` = "Frosted Mini-Wheats",
      `LUCKY CHARMS` = "Lucky Charms",
      `NAB SPOON SIZE SHRED` = "Spoon Size Shredded Wheat",
      `NABISCO SHREDDED WHE` = "Shredded Wheat",
      `NABISCO WHEAT N BRAN` = "Wheat 'N Bran",
      `OATMEAL RAISIN CRISP` = "Oatmeal Crisp",
      `POPEYE PUFFED RICE` = "Popeye Puffed Rice",
      `POPEYE PUFFED WHEAT` = "Popeye Puffed Wheat",
      `POST ALPHA BITS` = "Alpha-Bits",
      `POST COCOA PEBBLES` = "Cocoa PEBBLES",
      `POST FRT&FIBRE DATE/` = "Fruit & Fibre",
      `POST FRUITY PEBBLES` = "Fruity PEBBLES",
      `POST GRAPE-NUTS CERE` = "Grape-Nuts",
      `POST GRAPE NUTS` = "Grape-Nuts",
      `POST GRAPENUT FLAKES` = "Grape-Nuts Flakes",
      `POST HONEY COMB` = "Honeycomb",
      `POST NATURAL RAISIN` = "Natural Raisin Bran",
      `POST RAISIN BRAN` = "Post Raisin Bran",
      `POST SUGAR CRISP` = "Sugar Crisp",
      `POST/NATURAL BRAN FL` = "Natural Bran Flakes",
      `QUAKER 100% CEREAL H` = "100% Natural Cereal",
      `QUAKER 100% NATURAL` = "100% Natural Cereal",
      `QUAKER LIFE CEREAL` = "Life",
      `QUAKER LIFE CINNAMON` = "Life Cinnamon",
      `QUAKER OAT SQUARES` = "Oatmeal Squares",
      `QUAKER P.B. CAPTAIN` = "Peanut Butter Squares",
      `QUAKER PUFFED RICE` = "Puffed Rice",
      `QUAKER RTE OAT BRAN` = "Oat Bran",
      `RALSTON CORN CHEX` = "Corn Chex",
      `RALSTON RICE CHEX` = "Rice Chex",
      `RALSTON/WHEAT CHEX` = "Wheat Chex",
      `TOTAL RAISIN BRAN` = "Total Raisin Bran",
      `SMORES CRUNCH CEREAL` = "S'mores Crunch",
      TOTAL = "Total",
      `TOTAL CORN FLAKES` = "Total Corn Flakes",
      `TRIX` = "Trix",
      `WHEATIES` = "Wheaties",
      `WHOLE GRAIN TOTAL` = "Total Whole Grain"
    )
  ) %>%
  write_rds("../data/product_detail.RDS")

product_detail_tbl <- readRDS("../data/product_detail.RDS")

product_lookup_tbl <- product_detail_tbl %>%
  group_by(DESCRIP) %>%
  summarize(DISTINCT_YEAR = n_distinct(YEAR),
            SAMPLE_SIZE = n()) %>%
  filter(DISTINCT_YEAR == 9) %>%
  mutate(
    MANU = case_when(
      DESCRIP %in% c("Dominick's Oat Honey",
                     "Dominick's Oat Honey Raisin") ~ "Dominick's",
      DESCRIP %in% c(
        "Apple Cinnamon Cheerios",
        "Cheerios",
        "Cinnamon Toast Crunch",
        "Cocoa Puffs",
        "Cookie Crisp",
        "Count Chocula",
        "Crispy Wheats & Raisins",
        "Fiber One",
        "Kaboom",
        "Golden Grahams",
        "Honey Nut Cheerios",
        "Kix",
        "Lucky Charms",
        "Oatmeal Crisp",
        "Corn Chex",
        "Rice Chex",
        "Wheat Chex",
        "Total Raisin Bran",
        "S'mores Crunch",
        "Total",
        "Total Corn Flakes",
        "Trix",
        "Wheaties",
        "Total Whole Grain"
      ) ~ "General Mills",
      DESCRIP %in% c(
        "Nutri Grain Almond",
        "All-Bran",
        "Apple Jacks",
        "Bite Size Frosted Mini-Wheats",
        "Cocoa Krispies",
        "Corn Flakes",
        "Corn Pops",
        "Cracklin' Oat Bran",
        "Crispix",
        "Froot Loops",
        "Honey Smacks",
        "Just Right",
        "Nut & Honey Crunch",
        "Nutri-Grain",
        "Product 19",
        "Raisin Bran",
        "Rice Krispies",
        "Special K",
        "Special K Strawberry",
        "Special K Apple Cinammon",
        "Special K Blueberry",
        "Frosted Flakes",
        "Kellogg's Variety Pack",
        "Raisin Squares",
        "Just Right",
        "Frosted Mini-Wheats"
      ) ~ "Kellogg's",
      DESCRIP %in% c("Spoon Size Shredded Wheat",
                   "Shredded Wheat",
                   "Wheat 'N Bran") ~ "Nabisco",
      DESCRIP %in% c(
        "Honey Bunches of Oats with Almonds",
        "Honey Bunches of Oats with Raisins",
        "Alpha-Bits",
        "Cocoa PEBBLES",
        "Fruit & Fibre",
        "Fruity PEBBLES",
        "Grape-Nuts",
        "Grape-Nuts Flakes",
        "Honeycomb",
        "Natural Raisin Bran",
        "Post Raisin Bran",
        "Sugar Crisp",
        "Natural Bran Flakes"
      ) ~ "Post",
      DESCRIP %in% c(
        "Cap'n Crunch Christmas",
        "Cap'n Crunch",
        "Cap'n Crunch Jumbo",
        "Popeye Puffed Rice",
        "Popeye Puffed Wheat",
        "100% Natural Cereal",
        "Life",
        "Life Cinnamon",
        "Oatmeal Squares",
        "Peanut Butter Squares",
        "Puffed Rice",
        "Oat Bran"
      ) ~ "Quaker Oats"
    )
  )

product_total_tbl <- product_detail_tbl %>%
  inner_join(product_lookup_tbl, by = "DESCRIP") %>%
  group_by(YEAR) %>%
  summarize(TOTAL_REVENUE = sum(REVENUE),
            TOTAL_GROSS_MARGIN = sum(PROFIT))

product_summary_tbl <- product_detail_tbl %>%
  inner_join(product_lookup_tbl , by = "DESCRIP") %>%
  group_by(DESCRIP,
           MANU,
           CITY,
           YEAR) %>%
  summarize(
    UNIT_SALES = sum(MOVE),
    AVG_RETAIL_PRICE = mean(PRICE),
    BUNDLE_SALES = sum(QTY),
    REVENUE = sum(REVENUE),
    GROSS_MARGIN = sum(PROFIT)
  ) %>%
  arrange(DESCRIP, CITY, YEAR) %>%
  left_join(product_total_tbl) %>%
  ungroup() %>%
  group_by(DESCRIP, CITY) %>%
  mutate(UNIT_SALES_GROWTH = (UNIT_SALES / lag(UNIT_SALES) - 1) * 100) %>%
  ungroup() %>%
  mutate(
    YEAR = YEAR + 25,
    REVENUE_PCT = REVENUE / TOTAL_REVENUE * 100,
    GROSS_MARGIN_PCT = GROSS_MARGIN / TOTAL_GROSS_MARGIN * 100
  ) %>%
  select(
    MANU,
    DESCRIP,
    CITY,
    YEAR,
    UNIT_SALES,
    UNIT_SALES_GROWTH,
    AVG_RETAIL_PRICE,
    BUNDLE_SALES,
    REVENUE,
    REVENUE_PCT,
    GROSS_MARGIN,
    GROSS_MARGIN_PCT
  )

product_summary_tbl %>%
  write_rds("../data/product_summary.RDS")

product_detail_tbl %>%
  inner_join(product_lookup_tbl , by = "DESCRIP") %>%
  mutate(LOG_QUANTITY = log(MOVE / QTY),
         LOG_PRICE = log(PRICE),
         YEAR = YEAR + 25) %>%
  select(DESCRIP, CITY, YEAR, LOG_QUANTITY, LOG_PRICE) %>%
  write_rds("../data/product_analysis.RDS")

read_rds("../data/product_analysis.RDS")
