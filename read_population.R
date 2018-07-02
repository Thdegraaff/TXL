library("readr")
library("tidyverse")

bev_15_19 <- read_delim("data/src/Bev_15_19.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE, 
                        skip = 4)
bev_15_19 <- bev_15_19[-nrow(bev_15_19),]
bev_20_29 <- read_delim("data/src/Bev_20_29.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE, 
                        skip = 4)
bev_20_29 <- bev_20_29[-nrow(bev_20_29),]
bev_30_39 <- read_delim("data/src/Bev_30_39.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE, 
                        skip = 4)
bev_30_39<- bev_30_39[-nrow(bev_30_39),]
bev_40_49 <- read_delim("data/src/Bev_40_49.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE, 
                        skip = 4)
bev_40_49<- bev_40_49[-nrow(bev_40_49),]
bev_50_59 <- read_delim("data/src/Bev_50_59.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE, 
                        skip = 4)
bev_50_59<- bev_50_59[-nrow(bev_50_59),]
bev_60_64 <- read_delim("data/src/Bev_60_64.csv", 
                        ";", escape_double = FALSE, trim_ws = TRUE, 
                        skip = 4)
bev_60_64<- bev_60_64[-nrow(bev_60_64),]

sum_15_19 <- bev_15_19 %>% 
  group_by(`Regio's`, Perioden) %>%
  summarize( population = sum(aantal) ) %>%
  spread( Perioden, population)

sum_20_29 <- bev_20_29 %>% 
  group_by(`Regio's`, Perioden) %>%
  summarize( population = sum(aantal) ) %>%
  spread( Perioden, population)

sum_30_39 <- bev_30_39 %>% 
  group_by(`Regio's`, Perioden) %>%
  summarize( population = sum(aantal) ) %>%
  spread( Perioden, population)

sum_40_49 <- bev_40_49 %>% 
  group_by(`Regio's`, Perioden) %>%
  summarize( population = sum(aantal) ) %>%
  spread( Perioden, population)

sum_50_59 <- bev_50_59 %>% 
  group_by(`Regio's`, Perioden) %>%
  summarize( population = sum(aantal) ) %>%
  spread( Perioden, population)

sum_60_64 <- bev_60_64 %>% 
  group_by(`Regio's`, Perioden) %>%
  summarize( population = sum(aantal) ) %>%
  spread( Perioden, population)
