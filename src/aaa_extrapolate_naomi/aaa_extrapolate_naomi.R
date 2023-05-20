iso3_c <- iso3
spectrum <- extract_pjnz_naomi("depends/spectrum_file.zip")

areas <- read_sf("depends/naomi_areas.geojson")

admin1_lvl <- filter(read_csv("resources/iso_mapping_fit.csv", show_col_types = FALSE), iso3 == iso3_c)$admin1_level

prev <- read_csv("depends/prev_assigned_province.csv", show_col_types = FALSE) %>%
  mutate(indicator = "HIV prevalence",
         iso3 = iso3) %>%
  select(-province)

if(nrow(prev))
  prev <- prev %>%
    left_join(areas %>% select(area_id, province = area_name) %>% st_drop_geometry())

art <- read_csv("depends/art_assigned_province.csv", show_col_types = FALSE) %>%
  mutate(indicator = "ART coverage",
         iso3 = iso3)

if(nrow(art))
  art <- art %>%
    left_join(areas %>% select(area_id, province = area_name) %>% st_drop_geometry()) %>%
    group_by(row_id) %>%
    mutate(province = ifelse(length(unique(province)) > 1, NA_character_, province))

dat <- list("prev" = prev %>% select(-area_level), "art" = art %>% select(-area_level))

###########

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
  mutate(ratio = value/value[year == 2021],
         age_group = "Y015_049")

if(iso3 != "SSD") {
  
  sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))
  folder <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = "Shared Documents/Data/Spectrum files/2022 naomi preliminary")
  
  if(iso3 == "MOZ") {
    folder <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = "Shared Documents/Data/Spectrum files/2021 naomi")
  }
  
  path <- filter(folder$list(),
         str_detect(name, fixed(iso3, ignore_case=TRUE)),
         str_detect(name, "zip"))$name
  
  # path <- grep(iso3, folder$list()$name, value=TRUE, ignore.case = TRUE)
  
  if(length(path) > 1)
    stop("More than one Naomi fit found")
  
  if(length(path) == 0)
    stop("No Naomi fit found")
  
  if(iso3 != "MOZ") {
    path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/Spectrum files/2022 naomi preliminary", path)
  } else {
    path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/Spectrum files/2021 naomi", path)
  }
    
  indicators <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = path)
  # indicators <- read_output_package(indicators)
  
  tmpd <- tempfile()
  on.exit(unlink(tmpd))
  utils::unzip(indicators, exdir = tmpd)
  out <- list()
  out$indicators <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "indicators.csv"))
  out$meta_age_group <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "meta_age_group.csv"))
  out$meta_period <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "meta_period.csv"))
  out$meta_area <- read_sf(list.files(tmpd, full.names = TRUE, pattern = "boundaries.geojson"))
  out$meta_indicator <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "meta_indicator.csv"))
  class(out) <- "naomi_output"
  
  indicators <- add_output_labels(out)
  
  time_point <- unique(indicators$calendar_quarter)[2]
  
  filtered_indicators <- indicators %>%
    filter(area_level <= admin1_lvl,
           indicator %in% c("population", "plhiv", "art_current_residents"),
           age_group_label %in% c("15+", "15-49", "15-24", "20-24", "25-29", "25-49", "15-64"),
           calendar_quarter == time_point) %>%
    select(area_level:age_group_label, indicator, mean) %>%
    pivot_wider(names_from = indicator, values_from = mean) %>%
    ungroup()
  
  filtered_indicators <- bind_rows(filtered_indicators, 
                                   filtered_indicators %>%
                                     filter(age_group_label %in% c("15-24", "25-29")) %>%
                                     group_by(across(-c(age_group, age_group_label))) %>%
                                     summarise(plhiv = sum(plhiv),
                                               population = sum(population),
                                               art_current_residents = sum(art_current_residents)) %>%
                                     mutate(age_group_label = "15-29",
                                            age_group = "Y015_029") 
                                   
  ) %>%
    ungroup() %>%
    mutate(prevalence = plhiv/population,
           art_coverage = art_current_residents/plhiv) %>%
    select(-c(plhiv, art_current_residents))
  
  filtered_indicators <- filtered_indicators %>%
    pivot_longer(cols = c(prevalence, population, art_coverage), names_to = "indicator", values_to = "mean")
  
  df <- spectrum_ratio %>%
    select(-age_group) %>%
    left_join(filtered_indicators, by=c("sex", "indicator")) %>%
    mutate(mean = ratio * mean,
           # lower = ratio * lower,
           # upper = ratio * upper,
           indicator = case_when(
             indicator == "prevalence" ~ "HIV prevalence",
             indicator == "art_coverage" ~ "ART coverage",
             indicator == "population" ~ "Population",
             TRUE ~ indicator)) %>%
    select(area_level:area_name,
           sex:indicator,
           age_group_label,
           age_group,
           # lower, 
           mean, 
           # upper
    )
  
  out <- lapply(dat, function(x) {
    
    # x <- dat$art
    
    if(nrow(x)) {
      anonymised_dat <- x %>%
        select(!contains(c("Column", "..."))) %>%
        mutate(sex = case_when(
          kp %in% c("FSW", "SW", "TGW", "TG") ~ "female",
          kp %in% c("MSM", "TGM") ~ "male",
          kp == "PWID" ~ "both"
        ),
        has_age = ifelse(!is.na(age_group), 1, 0),
        age_group = case_when(
          (is.na(age_group) | !age_group %in% unique(filtered_indicators$age_group)) & !kp %in% c("TG", "TGW", "MSM") ~ "Y015_049",
          (is.na(age_group) | !age_group %in% unique(filtered_indicators$age_group)) & kp %in% c("TG", "TGW", "MSM") ~ "Y015_029"
          # is.na(age_group) | !age_group %in% unique(filtered_indicators$age_group) ~ "Y015_049",
          # TRUE ~ as.character(age_group)
        )) %>%
        left_join(df %>% select(-any_of("area_name"))) %>%
        group_by(row_id) %>%
        mutate(province = ifelse(length(unique(province)) > 1, NA_character_, province)) %>%
        rename(provincial_value = mean) %>%
        group_by(across(-provincial_value)) %>%
        summarise(provincial_value = median(provincial_value)) %>%
        mutate(ratio = value/provincial_value) %>%
        ungroup()
    } else {
      data.frame(x = character())
    }
    
  })
  
  area_indicators <- indicators %>%
    filter(age_group == "Y015_049",
           indicator %in% c("prevalence", "art_coverage"),
           area_level %in% c(0, admin1_lvl),
           calendar_quarter == unique(indicators$calendar_quarter)[2]) %>%
    select(area_id, sex, indicator, mean)
  
} else {
  
  dat$prev$area_id <- "SSD"
  dat$art$area_id <- "SSD"
  
  df <- spectrum_ratio %>%
    mutate(indicator = case_when(
             indicator == "prevalence" ~ "HIV prevalence",
             indicator == "art_coverage" ~ "ART coverage",
             indicator == "population" ~ "Population",
             TRUE ~ indicator)) %>%
    rename(provincial_value = value)
  
  out <- lapply(dat, function(x) {
    
    if(nrow(x)) {
      anonymised_dat <- x %>%
        select(!contains(c("Column", "..."))) %>%
        mutate(sex = case_when(
          kp %in% c("FSW", "SW", "TGW", "TG") ~ "female",
          kp %in% c("MSM", "TGM") ~ "male",
          kp == "PWID" ~ "both"
        ),
        has_age = ifelse(!is.na(age_group), 1, 0),
        age_group = case_when(
          is.na(age_group) ~ "Y015_049",
          TRUE ~ as.character(age_group))
        ) %>%
        left_join(df)
    } else {
      data.frame(x = character())
    }
    
  })
  
  area_indicators <- data.frame()
}

if(nrow(out$prev)) {
  out_prev_model <- out$prev %>%
    filter(across(any_of("is_aggregate"), is.na)) %>%
    select(iso3, area_id, year, kp, age_group, method, denominator, has_age, value, provincial_value, ratio, ref)
} else {
  out_prev_model <- data.frame(x = character()) 
}

if(nrow(out$art)) {
  out_art_model <- out$art %>%
    filter(across(any_of("is_aggregate"), is.na)) %>%
    select(iso3, area_id, year, kp, method, age_group, has_age, value, denominator, provincial_value, ratio, ref)
} else {
  out_art_model <- data.frame(x = character()) 
}

write_csv(df, "extrapolated_naomi.csv")
write_csv(out_prev_model, "prev.csv")
write_csv(out_art_model, "art.csv")
write_csv(area_indicators, "area_indicators.csv")

if(nrow(prev)) {
  workbook_export_prev <- prev %>%
    distinct(iso3, data_checked, country.name, method, kp, area_name, province, year, prop_lower, value, prop_upper, denominator, ref, link) %>%
    mutate(indicator = "HIV prevalence") %>%
    rename(prop_estimate = value) %>%
    mutate(
      surveillance_type = NA,
      count_lower = NA,
      count_estimate = NA,
      count_upper = NA,
      population = NA,
      sex= case_when(
        kp %in%  c("FSW", "TG") ~ "female",
        kp == "MSM" ~ "male",
        kp == "PWID" ~ "both"
      ),
      age_group = NA,
      notes = NA,
      link = NA,
      prop_lower = round(prop_lower, 3),
      prop_estimate = round(prop_estimate, 3),
      prop_upper = round(prop_upper, 3)
    ) %>%
    select(all_of(c("country.name", "data_checked", "surveillance_type", "indicator", "method", "kp", "sex", "age_group", "area_name", "province", "year", "count_lower", "count_estimate", "count_upper", "population", "prop_lower", "prop_estimate", "prop_upper", "denominator", "notes", "ref", "link")))
} else {
  workbook_export_prev <- data.frame(x = character())
}

if(nrow(art)) {
  workbook_export_art <- art %>%
    ungroup() %>%
    distinct(iso3, data_checked, country.name, method, kp, area_name, province, year, prop_lower, value, prop_upper, denominator, ref, link) %>%
    mutate(indicator = "ART coverage") %>%
    rename(prop_estimate = value) %>%
    mutate(
      surveillance_type = NA,
      count_lower = NA,
      count_estimate = NA,
      count_upper = NA,
      population = NA,
      sex= case_when(
        kp %in%  c("FSW", "TG") ~ "female",
        kp == "MSM" ~ "male",
        kp == "PWID" ~ "both"
      ),
      age_group = NA,
      notes = NA,
      link = NA,
      prop_lower = round(prop_lower, 3),
      prop_estimate = round(prop_estimate, 3),
      prop_upper = round(prop_upper, 3)
    ) %>%
    select(all_of(c("country.name", "data_checked", "surveillance_type", "indicator", "method", "kp", "sex", "age_group", "area_name", "province", "year", "count_lower", "count_estimate", "count_upper", "population", "prop_lower", "prop_estimate", "prop_upper", "denominator", "notes", "ref", "link")))
} else {
  workbook_export_art <- data.frame(x = character())
}


write_csv(workbook_export_prev, "workbook_export_prev.csv", na = "")
write_csv(workbook_export_art, "workbook_export_art.csv", na = "")
