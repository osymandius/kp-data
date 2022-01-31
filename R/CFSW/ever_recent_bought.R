library(countrycode)

ever_sex <- read_csv("~/Dropbox/Work Streams/2021/Key populations/Data consolidation/MMG/everpaidforsex-bycountry.csv")
recent_sex <- read_csv("~/Dropbox/Work Streams/2021/Key populations/Data consolidation/MMG/recentpaidforsex-bycountry.csv")

bought_sex <- recent_sex %>%
  mutate(iso3 = countrycode(Country, "country.name", "iso3c")) %>%
  select(iso3, estimate = PaidSexLast12Months, lower = CI.LowerBound, upper = CI.UpperBound) %>%
  mutate(indicator = "recent_sex") %>%
  bind_rows(
    ever_sex %>%
      mutate(iso3 = countrycode(Country, "country.name", "iso3c")) %>%
      select(iso3, estimate = EverPaidSex, lower = CI.LowerBound, upper = CI.UpperBound) %>%
      mutate(indicator = "ever_sex")
  )


est_pse  <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_estimates.csv")

est_pse %>%
  filter(kp == "FSW") %>%
  left_join(bought_sex %>% select(iso3, estimate, indicator)) %>%
  filter(!is.na(indicator)) %>%
  ggplot(aes(x=median, y=estimate)) +
    geom_point() +
    geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
    geom_smooth(method = "lm") +
    labs(x="Estimated PSE proportion", y="Bought sex proportion") +
    facet_wrap(~indicator) 
