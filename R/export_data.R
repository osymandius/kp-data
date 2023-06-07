pse <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final_sourced.csv")

prev_id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_extrapolate_naomi", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022)'), draft = FALSE)
})

prev_data_input <- lapply(prev_id, function(x) read_csv(paste0("archive/aaa_extrapolate_naomi/", x, "/data_sharing_prev.csv"), show_col_types = FALSE)) %>%
  bind_rows()

art_data_input <- lapply(prev_id, function(x) read_csv(paste0("archive/aaa_extrapolate_naomi/", x, "/data_sharing_art.csv"), show_col_types = FALSE)) %>%
  bind_rows()

dat <- bind_rows(
  pse %>% 
    mutate(indicator = "pse",
           proportion_lower = count_lower/population,
           proportion_upper = count_upper/population,
           age_group_analysis = "Y015_049") %>%
    rename(area = area_name,
           country = country.name,
           proportion_estimate = prop_estimate),
  prev_data_input %>% 
    mutate(indicator = "prevalence"),
  art_data_input %>% 
    mutate(indicator = "art_coverage")
)

dat <- dat %>%
  select(study_idx, 
         iso3,
         country,
         kp,
         year,
         indicator,
         method,
         area,
         province,
         age_group,
         age_group_analysis,
         count_estimate,
         count_lower,
         count_upper,
         population,
         proportion_estimate,
         proportion_lower,
         proportion_upper,
         sample_size,
         provincial_value,
         ratio) %>%
  mutate(kp = ifelse(kp == "TG", "TGW", kp),
         across(c(starts_with("proportion"), provincial_value, ratio), ~round(.x, 3)),
         sample_size = round(sample_size, 0),
         population = round(population, 0))

write_csv(dat, "~/Downloads/sharing_dat_test.csv", na = "")

ethic <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1murnx2dH0W_94gFFuJ-oSW0ugdmv8WPinmog0_V-0FI/edit#gid=1735078680")

sources <- ethic %>%
  filter(`Study idx` %in% unique(dat$study_idx)) %>%
  mutate(country = countrycode::countrycode(iso3, "iso3c", "country.name")) %>%
  select(study_idx = `Study idx`,
         iso3,
         country,
         year,
         study = Study,
         link = Link,
         public_report = `Public Report/Study`,
         has_ethics = Ethics,
         ethical_review = `Who from`) %>%
  distinct()

sources$year <- unlist(sources$year)

write_csv(sources, "~/Downloads/sharing_sources_test.csv", na = "")

areas <- lapply(paste0("archive/aaa_extrapolate_naomi/", prev_id, "/depends/naomi_areas.geojson"), read_sf) %>%
  bind_rows()

admin1_lvl <- read_csv("global/iso_mapping_fit.csv", show_col_types = FALSE)

province_meta <- areas %>%
  st_drop_geometry() %>%
  separate(area_id, into = c("iso3", NA), sep = 3) %>%
  distinct(iso3, area_level, area_level_label) %>%
  left_join(admin1_lvl) %>%
  filter(area_level == admin1_level) %>%
  mutate(country = countrycode::countrycode(iso3, "iso3c", "country.name")) %>%
  select(country, area_level, area_level_label)


int <- tempdir()

write_csv(province_meta, file.path(int, "province_meta.csv"))
write_sf(areas, file.path(int, "naomi_areas.geojson"))

grump <- read_sf("archive/aaa_download_constrained_worldpop/20230123-162942-a6966a85/merge_cities.geojson")

write_sf(grump, file.path(int, "grump_city_boundaries.geojson"))
