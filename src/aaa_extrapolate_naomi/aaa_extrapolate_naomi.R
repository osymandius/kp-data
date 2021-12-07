# indicators <- read_output_package("depends/naomi_output.zip")
spectrum <- extract_pjnz_naomi("depends/spectrum_file.zip")

list.files(file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/Spectrum files/2021 naomi"))

sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))
folder <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = "Shared Documents/Data/Spectrum files/2021 naomi")
path <- grep(iso3, folder$list()$name, value=TRUE, ignore.case = TRUE)
path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/Spectrum files/2021 naomi", path)

indicators <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = path)
indicators <- read_output_package(indicators)
indicators <- add_output_labels(indicators) %>%
  dplyr::left_join(indicators$meta_age_group, by = c("age_group", "age_group_label"))

prev <- read.csv("depends/prev_assigned_province.csv") %>%
  mutate(indicator = "HIV prevalence",
         iso3 = iso3)
art <- read.csv("depends/art_assigned_province.csv") %>%
  mutate(indicator = "ART coverage",
         iso3 = iso3)

dat <- list("prev" = prev, "art" = art)

lvl_map <- read.csv("resources/iso_mapping_fit.csv")
admin1_lvl <- lvl_map$admin1_level[lvl_map$iso3 == iso3]

time_point <- unique(indicators$calendar_quarter)[2]
  
filtered_indicators <- indicators %>%
  filter(area_level <= admin1_lvl,
         indicator %in% c("prevalence", "art_coverage", "population"),
         age_group_label %in% c("15+", "15-49", "15-24", "20-24", "25-29", "25-49", "15-64"),
         calendar_quarter == time_point)

spectrum <- spectrum %>%
  filter(age %in% 15:49, year > 1999)

spectrum_ratio <- spectrum %>%
  bind_rows(
    spectrum %>%
      group_by(year) %>%
      summarise(hivpop = sum(hivpop),
                totpop = sum(totpop),
                artpop = sum(artpop)) %>%
      mutate(sex = "both")
  ) %>%
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
         age_group,
         lower, mean, upper)

out <- lapply(dat, function(x) {
  
  if(nrow(x)) {
    anonymised_dat <- x %>%
      mutate(sex = case_when(
        kp %in% c("FSW", "SW", "TGW", "TG") ~ "female",
        kp %in% c("MSM", "TGM") ~ "male",
        kp == "PWID" ~ "both"
        ),
        has_age = ifelse(!is.na(age_group), 1, 0),
        age_group = ifelse(is.na(age_group) | !age_group %in% unique(filtered_indicators$age_group), "Y015_049", as.character(age_group))
      ) %>%
      left_join(df) %>%
      mutate(ratio = value/mean) %>%
      select(iso3, year, kp, age_group, has_age, value, denominator, mean, ratio) %>%
      rename(provincial_value = mean)
  } else {
    data.frame(x = character())
  }
  
})

write.csv(df, "extrapolated_naomi.csv")
write.csv(out$prev, "prev.csv")
write.csv(out$art, "art.csv")