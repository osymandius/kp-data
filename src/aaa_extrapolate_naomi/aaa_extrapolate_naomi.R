indicators <- read_output_package("depends/naomi_output.zip")$indicators
spectrum <- extract_pjnz_naomi("depends/spectrum_file.zip")

prev <- read.csv("depends/prev_assigned_province.csv") %>%
  mutate(indicator = "HIV prevalence")
art <- read.csv("depends/art_assigned_province.csv") %>%
  mutate(indicator = "ART coverage")

dat <- list("prev" = prev, "art" = art)

lvl_map <- read.csv("resources/iso_mapping_fit.csv")
admin1_lvl <- lvl_map$admin1_level[lvl_map$iso3 == iso3]

time_point <- unique(indicators$calendar_quarter)[2]
  
filtered_indicators <- indicators %>%
  filter(sex != "both",
         area_level <= admin1_lvl,
         indicator %in% c("prevalence", "art_coverage", "population"),
         age_group_label %in% c("15+", "15-49", "15-24", "20-24", "25-49"),
         calendar_quarter == time_point)

spectrum_ratio <- spectrum %>%
  filter(age %in% 15:49, year > 1999) %>%
  group_by(sex, year) %>%
  summarise(prevalence = sum(hivpop)/sum(totpop),
            art_coverage = sum(artpop)/sum(hivpop),
            population = sum(totpop)) %>%
  pivot_longer(-c(sex, year), names_to = "indicator") %>%
  group_by(sex, indicator) %>%
  mutate(ratio = value/value[year == 2020])

df <- spectrum_ratio %>%
  left_join(filtered_indicators, by=c("sex", "indicator")) %>%
  mutate(mean = ratio * mean,
         lower = ratio * lower,
         upper = ratio * upper,
         indicator = case_when(
           indicator == "prevalence" ~ "HIV prevalence",
           indicator == "art_coverage" ~ "ART coverage",
           indicator == "population" ~ "Population",
           TRUE ~ indicator)) %>%
  select(area_level:area_name,
         sex:indicator,
         age_group_label,
         lower, mean, upper)

out <- lapply(dat, function(x) {
  
  if(nrow(x)) {
    anonymised_dat <- x %>%
      mutate(sex = case_when(
        kp %in% c("FSW", "SW", "TGW", "TG") ~ "female",
        kp %in% c("MSM", "PWID", "TGM") ~ "male"
      )) %>%
      left_join(df %>% filter(age_group_label == "15-49")) %>%
      mutate(ratio = value/mean) %>%
      select(year, kp, value, mean, ratio) %>%
      rename(provincial_value = mean)
  } else {
    data.frame(x = character())
  }
  
})

write.csv(df, "extrapolated_naomi.csv")
write.csv(out$prev, "anonymised_prev.csv")
write.csv(out$art, "anonymised_art.csv")