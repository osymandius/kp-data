library(countrycode)
library(tidyverse)

fy21 <- read_tsv("~/Imperial College London/HIV Inference Group - WP - Documents/Data/PEPFAR/FY21Q2/MER_Structured_Datasets_Site_IM_FY19-21_20210618_v2_1_Mozambique.zip")

fy21_kp <- filter(fy21, str_detect(otherdisaggregate, "FSW|MSM|PWID|TG"),
                  indicator %in% c("HTS_TST", "TX_CURR")
                  )
int <- fy21_kp %>%
  pivot_longer(qtr1:qtr4, names_to = "fiscal_quarter") %>%
  mutate(
    year = if_else(fiscal_quarter == "qtr1", fiscal_year - 1, fiscal_year),
    quarter = recode(fiscal_quarter, "qtr1" = 4, "qtr2" = 1, "qtr3" = 2, "qtr4" = 3),
    calendar_quarter = paste0("CY", year, "Q", quarter),
    value = replace_na(value, 0),
    iso3 = countrycode(fy21$countryname[1], "country.name", "iso3c")
  ) %>%
  select(iso3, snu1, year, indicator, otherdisaggregate, kp = otherdisaggregate_sub, statushiv, value)
  
hiv_prev <- int %>%
  filter(indicator == "HTS_TST") %>%
  group_by(iso3, year, indicator, kp, statushiv) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(names_from = statushiv, values_from = value) %>%
  mutate(prevalence = Positive/(Positive + Negative),
         denominator = Negative + Positive) %>%
  select(-c(Negative, Positive)) %>%
  arrange(kp, year)

int %>%
  filter(indicator == "TX_CURR") %>%
  group_by(iso3, snu1, year, indicator, kp) %>%
  summarise(value = sum(value)) %>%
  arrange(kp, year) %>%
  View()
