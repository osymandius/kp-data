library(XLConnect)
library(tidyverse)
library(stringi)

ssa_iso3 <- moz.utils::ssa_iso3()

export_data_sharing <- function(purpose) {
  
  zip_name <- paste0(Sys.Date(), "_", purpose)
  
  pse_id <- lapply(ssa_iso3, function(x){
    orderly::orderly_search(name = "aaa_assign_populations", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022)'), draft = FALSE)
  })
  
  pse <- lapply(pse_id, function(x) read_csv(paste0("archive/aaa_assign_populations/", x, "/pse_prevalence.csv"), show_col_types = FALSE)) %>%
    bind_rows()
  
  prev_id <- lapply(ssa_iso3, function(x){
    orderly::orderly_search(name = "aaa_extrapolate_naomi", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022)'), draft = FALSE)
  })
  
  prev_data_input <- lapply(prev_id, function(x) read_csv(paste0("archive/aaa_extrapolate_naomi/", x, "/data_sharing_prev.csv"), show_col_types = FALSE)) %>%
    bind_rows()
  
  art_data_input <- lapply(prev_id, function(x) read_csv(paste0("archive/aaa_extrapolate_naomi/", x, "/data_sharing_art.csv"), show_col_types = FALSE)) %>%
    bind_rows()
  
  orderly_id <- data.frame(iso3 = ssa_iso3, 
                           task = "aaa_extrapolate_naomi",
                           id = unlist(prev_id)) %>%
    bind_rows(data.frame(
      task = "aaa_assign_populations",
      iso3 = ssa_iso3,
      id = unlist(pse_id))
    )
  
  dat <- bind_rows(
    pse %>% 
      mutate(indicator = "pse",
             proportion_lower = count_lower/population,
             proportion_upper = count_upper/population,
             age_group_analysis = "Y015_049") %>%
      rename(country = country.name,
             proportion_estimate = prop_estimate,
             matched_province = province,
             matched_province_area_id = province_area_id,
             # study_area = area_name
      ),
    prev_data_input %>% 
      mutate(indicator = "prevalence"),
    art_data_input %>% 
      mutate(indicator = "art_coverage")
  )
  
  
  dat <- dat %>%
    mutate(iso3 = countrycode::countrycode(country, "country.name", "iso3c")) %>%
    select(study_idx, 
           iso3,
           country,
           kp,
           year,
           indicator,
           method,
           study_area = area_name,
           matched_area_id = area_id,
           matched_province,
           matched_province_area_id, 
           age_group_reported = age_group,
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
           population = round(population, 0),
           across(where(is.character), ~iconv(.x, to="UTF-8")))
  
  ### ETHICS INFO
  
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
    distinct() %>%
    mutate(across(where(is.character), ~iconv(.x, to="UTF-8")))
  
  sources$year <- as.character(sources$year)
  
  ### BOUNDARY FILES AND AREA HIERARCHIES
  
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
  
  #### MODELLED ESTIMATES
  
  kplhiv_art <- readRDS("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/kplhiv_art.rds")
  
  country_estimates <- kplhiv_art %>%
    lapply("[[", "country") %>%
    bind_rows(.id = "kp") %>%
    mutate(country = countrycode(iso3, "iso3c", "country.name"),
           lower = ifelse(indicator %in% c("kplhiv", "kpart", "pse_total_count", "pse_urban_count"), round(lower, 0), lower),
           median = ifelse(indicator %in% c("kplhiv", "kpart", "pse_total_count", "pse_urban_count"), round(median, 0), median),
           upper = ifelse(indicator %in% c("kplhiv", "kpart", "pse_total_count", "pse_urban_count"), round(upper, 0), upper)
    ) %>%
    select(iso3, country, kp, indicator, lower:upper)
  
  ### WRITING TO ZIP FOLDER
  
  
  int <- tempdir()
  
  lapply(file.path(tempdir(), c("orderly_id.csv", "naomi_areas.geojson", "grump_city_boundaries.geojson", "province_meta.csv", "dataset.xlsx")), file.remove)
  
  write_csv(orderly_id, file.path(int, "orderly_id.csv"))
  write_csv(province_meta, file.path(int, "province_meta.csv"))
  write_sf(areas, file.path(int, "naomi_areas.geojson"))
  
  grump <- read_sf("archive/aaa_download_constrained_worldpop/20230607-100426-f4cd9d69/merge_cities.geojson")
  write_sf(grump, file.path(int, "grump_city_boundaries.geojson"))
  
  wb <- XLConnect::loadWorkbook("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Aggregate data/Data sharing/data_sharing_template.xlsx")
  
  writeWorksheet(wb, zip_name, "Process Overview", startRow = 3, startCol = 1, header = F)
  writeWorksheet(wb, dat, "Data", header = F)
  writeWorksheet(wb, country_estimates, "Estimates")
  writeWorksheet(wb, sources, "Source List")
  
  saveWorkbook(wb, file.path(int, "dataset.xlsx"))
  
  path <- file.path("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Aggregate data/Data sharing", zip_name)
  
  setwd(int)
  zip(path, c("orderly_id.csv", "naomi_areas.geojson", "grump_city_boundaries.geojson", "province_meta.csv", "dataset.xlsx"))
  
  
}

setwd("~/Documents/GitHub/kp-data/")
export_data_sharing("Imperial feedback II")


