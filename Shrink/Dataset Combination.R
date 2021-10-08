library(dplyr)
library(tidyr)

## Read in Root Dataset

levy <- read.csv("levy-rates_totals-2006-2020.csv")

## Add corr to levy

corr <- read.csv("correlation-dash.csv")
corr1 <- corr %>%
  select(c(2, 3, 6:12, 26))

levy_corr <- levy %>%
  left_join(corr1, by = c("CITY.NAME" = "CITY.NAME", "Year" = "Year"))

## Add crop diversity to levy

crop_div <- read.csv("crop-diversity_metrics.csv") %>%
  distinct()

crop_div$city_name <- sapply(crop_div$city_name, toupper)

levy_all <- levy_corr %>%
  left_join(crop_div, by = c("CITY.NAME" = "city_name", "Year" = "year"))


write.csv(levy_all, file = "levy_all.csv")
