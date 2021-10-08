library(dplyr)
library(tidyr)

## Read in Root Dataset

levy <- read.csv("levy-rates_totals-2006-2020.csv")

levy$CITY.SIZE <- factor(levy$CITY.SIZE)
levy$CITY.SIZE <-
  reorder(levy$CITY.SIZE, levy$Population, na.rm = TRUE)
levels(levy$CITY.SIZE)[4] <- "NANO- and MICROPOLITAN"

## Add corr to levy

corr <- read.csv("correlation-dash.csv")
corr1 <- corr %>%
  select(c(2, 3, 6:12, 26))

levy_corr <- levy %>%
  left_join(corr1, by = c("CITY.NAME" = "CITY.NAME", "Year" = "Year"))

## Add crop diversity to levy

crop_div <- read.csv("crop-diversity_metrics.csv") %>%
  rename(Long = "longitude",
         Lat = "latitude",
         County = "county") %>%
  distinct()

crop_div$city_name <- sapply(crop_div$city_name, toupper)

levy_all <- levy_corr %>%
  left_join(crop_div, by = c("CITY.NAME" = "city_name", "Year" = "year")) %>%
  select(-c(Long, Lat, County, population, city_size))


write.csv(levy_all, file = "levy_all.csv")
