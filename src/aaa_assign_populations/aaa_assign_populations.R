iso3_c <- iso3

areas <- read_sf("depends/naomi_areas.geojson") %>%
  mutate(iso3 = iso3_c)

population <- read.csv("depends/interpolated_population.csv") 

if(iso3 == "BEN") {
  areas <- read_sf("ben_areas.geojson")%>%
    mutate(iso3 = iso3_c)
  
  population <- read.csv("ben_pop.csv")
}

if(iso3 == "COG") {
  areas <- read_sf("cog_areas.geojson") %>%
    mutate(iso3 = iso3_c)
  
  population <- read.csv("cog_pop.csv")
}

if(iso3 == "GMB") {
  areas <- read_sf("gmb_areas.geojson") %>%
    mutate(iso3 = iso3_c)
  
  population <- read.csv("gmb_pop.csv")
}

if(iso3 == "MLI") {
  areas <- read_sf("mli_areas.geojson") %>%
    mutate(iso3 = iso3_c)
  
  population <- read.csv("mli_pop.csv")
}

if(iso3 == "NGA") {
  areas <- read_sf("nga_areas.geojson")%>%
    mutate(iso3 = iso3_c)
  
  population <- read.csv("nga_pop.csv")
}

if(iso3 == "ZAF") {
  areas <- read_sf("zaf_areas.geojson")%>%
    mutate(iso3 = iso3_c)
  
  population <- read.csv("zaf_pop.csv")
}

area_lvl_mapping <- read_csv("resources/iso_mapping_fit.csv", show_col_types = FALSE)
admin1_lvl <- area_lvl_mapping$admin1_level[area_lvl_mapping$iso3 == iso3_c]

population <- population %>%
  left_join(areas %>% dplyr::select(area_id, area_name) %>% st_drop_geometry()) %>%
  mutate(area_name = str_to_sentence(area_name))

if(iso3 != "SSD") {
  city_population <- read.csv("depends/interpolated_city_population.csv") %>%
    mutate(area_name = str_to_sentence(area_name),
           iso3 = iso3_c
    )
} else {
  city_population <- data.frame(area_id = NA_character_,
                                area_name = NA_character_,
                                year = NA_integer_,
                                sex = NA_character_,
                                age_group = NA_character_,
                                population = NA_integer_,
                                iso3 = NA_character_)
}

if(iso3 == "BFA") {
  
  humd_pop <- read_csv("bfa-humd-pop-scaled.csv", show_col_types = F) %>%
    mutate(area_name = str_to_sentence(area_name),
           iso3 = iso3_c
    )
  
  city_population <- city_population %>%
    bind_rows(humd_pop)
  
  bfa_humd_areas <- read_sf("bfa_humd_areas.geojson") %>%
    st_make_valid() %>%
    st_join(areas %>% filter(area_level == 1) %>% select(province_area_id = area_id, province = area_name), largest = T) %>%
    mutate(area_name = str_replace_all(adm2_name1, "é|è", "e")) %>%
    left_join(humd_pop %>% distinct(area_id, area_name)) %>%
    select(area_id, area_name, province, province_area_id)
}


population <- population %>%
  bind_rows(city_population)

population <- population %>%
  mutate(msm_age = ifelse(age_group %in% c("Y015_019", "Y020_024", "Y025_029"), 1, 0)) %>%
  filter(msm_age == 1) %>%
  group_by(area_id, area_name, year, sex) %>%
  summarise(population = sum(population)) %>%
  mutate(age_group = "Y015_029") %>%
  bind_rows(
    population %>%
      five_year_to_15to49("population") %>%
      sex_aggregation("population")
  )

# population <- population %>%
#   five_year_to_15to49("population") %>%
#   sex_aggregation("population")

extrap_pop <- population %>%
  filter(year %in% 2015:2020, !is.na(population)) %>%
  group_by(area_id, area_name, age_group, sex) %>%
  mutate(
    population = log(population),
    population = exp(Hmisc::approxExtrap(year, population, xout = 2020:2025)$y),
    year = year + 5)

population <- bind_rows(
  population %>% filter(year<2020),
  extrap_pop
)

# pse_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Analytical datasets/key-populations/PSE", "pse_spreadsheet_cleaned_sourced.csv")
# pse <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = pse_path)
# pse <- read_csv(pse)
# pse <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE", "pse_spreadsheet_cleaned_sourced.csv") 
pse <- read_csv("pse_cleaned_sourced_data.csv", show_col_types = FALSE)

pse <- pse %>%
  filter(iso3 == iso3_c) %>%
  dplyr::select(iso3:count_upper, prop_lower, prop_estimate, prop_upper, area_level, study_idx, ref, data_checked, observation_idx) %>%
  mutate(sex = case_when(
    kp %in% c("FSW", "TG", "TGW") ~ "female",
    kp %in% c("MSM", "PWID") ~ "male",
    # kp == "PWID" ~ "both"
  ))
  # mutate(observation_idx = row_number())

pse <- pse %>%
    mutate(area_name = case_when(
      iso3 == "BFA" & area_name == "Sanmentenga" ~  "Sanmatenga",
      iso3 == "BFA" & area_name == "Toy" ~  "Tuy",
      iso3 == "BFA" & str_detect(area_name, "Centre ") & study_idx == 314 ~  str_replace(area_name, "Centre ", "Centre-"),
      iso3 == "BFA" & area_name == "Sud Ouest" ~  "Sud-Ouest",
      
      iso3 == "BWA" & area_name == "Ngami" ~ "Ngamiland",
      
      iso3 == "COD" & area_name == "Bas Congo" ~ "Kongo Central",
      iso3 == "COD" & area_name == "Katanga" ~ "Tanganyika; Haut-Lomami; Lualaba; Haut-Katanga",
      iso3 == "COD" & area_name %in% c("Oriental", "Orientale") ~ "Ituri; Haut-Uele; Tshopo; Bas-Uele",
      
      iso3 == "GNB" & str_detect(area_name, "Sector Autonomo de Bissau") ~ str_replace(area_name, "Sector Autonomo de Bissau", "SAB"),
      
      iso3 == "ETH" & area_name == "Adama" ~ "Adama Town",
      
      iso3 == "KEN" & area_name == "Dagoretti" ~ "Dagoretti South; Dagoretti North",
      iso3 == "KEN" & area_name == "Embakasi" ~ "Embakasi South; Embakasi North; Embakasi West; Embakasi East; Embakasi Central",
      iso3 == "KEN" & area_name == "Mavoko" ~ "Athiriver",
      iso3 == "KEN" & area_name == "Nairobi" & study_idx == 124 ~ "Nairobi (County)",
      iso3 == "KEN" & area_name == "Tharaka" & study_idx == 124 ~ "Tharaka-Nithi",
      
      iso3 == "MLI" & area_name == "GUEGNEKA (FANA)" & study_idx == 134 ~ "Guegneka",
      iso3 == "MLI" & area_name == "KAYES" & study_idx == 134 ~ "Kayes Commune",
      iso3 == "MLI" & area_name == "Kita" & study_idx == 134 ~ "Kita Commune",
      iso3 == "MLI" & area_name == "NIORO" & study_idx == 134 ~ "Nioro Commune",
      iso3 == "MLI" & area_name == "Kaladougou (Dioila)" & study_idx == 134 ~ "Kaladougou",
      iso3 == "MLI" & area_name == "Benkadi (Kouremale)" & study_idx == 134 ~ "Benkadi-Kan",
      iso3 == "MLI" & area_name == "BADJANGARA" & study_idx == 134 ~ "Bandiagara",
      iso3 == "MLI" & area_name == "BADJANGA" & study_idx == 134 ~ "Bandiagara",
      iso3 == "MLI" & area_name == "CINCINA" & study_idx == 134 ~ "Sincina",
      
      iso3 == "MOZ" & area_name == "Maputo City" ~ "Cidade de Maputo",
      iso3 == "MOZ" & area_name == "Maputo Province" ~ "Maputo Provincia",
      
      iso3 == "NAM" & area_name == "Khomas region" ~ "Khomas",
      
      iso3 == "NGA" & area_name == "Calabar Municipality" ~ "Calabar Municipal",
      
      iso3 == "SLE" & area_name == "Western rural" ~ "Western Area Rural",
      iso3 == "SLE" & area_name == "Western Urban" ~ "Western Area Urban",
      
      iso3 == "TGO" & area_name == "Grand Lome" ~ "Lome",
      
      iso3 == "ZAF" & area_name == "NM Molema DM" ~ "Ngaka Modiri Molema DM",
      iso3 == "ZAF" & area_name == "Nelson Mandela Bay Metro" ~ "N Mandela Bay MM",
      iso3 == "ZAF" & area_name == "Mangaung" ~ "Mangaung MM",
      iso3 == "ZAF" & area_name == "Manguang DM" ~ "Mangaung MM",
      iso3 == "ZAF" & area_name == "Capricorn DM (Polokwane)" ~ "Capricorn DM",
      
      TRUE ~ area_name
    ))

if(nrow(pse)) {
    
    pse_areas <- pse %>%
      dplyr::select(iso3, area_name, area_level, year, kp, observation_idx) %>%
      mutate(
        area_name = str_replace_all(area_name, "\\,\\,|\\,|\\/", "\\;")) %>%
      distinct() %>%
      separate(area_name, sep = ";", into = paste0("area_split", 1:20), remove=FALSE) %>%
      mutate(across(starts_with("area_split"), ~str_trim(.x))) %>%
      pivot_longer(-c(iso3, area_name, area_level, year, observation_idx, kp)) %>%
      filter(!is.na(value)) %>%
      mutate(idx = row_number(),
             value = tolower(value)) %>%
      rename(given_area = area_name)
    
    min_dist_area_level <- pse_areas %>%
      filter(!is.na(area_level)) %>%
      full_join(areas %>% 
                  mutate(iso3 = iso3_c) %>%
                  dplyr::select(iso3, area_name, area_level, area_id) %>% 
                  st_drop_geometry(), by=c("iso3", "area_level"))  %>%
      mutate(dist = stringdist(value, tolower(area_name))) %>%
      group_by(idx) %>%
      filter(dist == min(dist)) 
    
    ## GRUMP PRIORITY
    
    min_dist_cities <- pse_areas %>%
        filter(is.na(area_level)) %>%
        dplyr::select(-area_level) %>%
        full_join(city_population %>% distinct(iso3, area_id, area_name),
                  by="iso3") %>%
        mutate(dist = stringdist::stringdist(value, tolower(area_name))) %>%
        group_by(idx) %>%
        filter(dist == min(dist))
  
    grump_match <- min_dist_cities %>%
      filter(n() == 1, dist<3)
    
    # assigned_areas_idx <- grump_match$idx
  
    min_dist_naomi <- pse_areas %>%
      filter(is.na(area_level)) %>%
      dplyr::select(-area_level) %>%
      filter(!idx %in% grump_match$idx) %>%
      full_join(areas %>%
                  mutate(iso3 = iso3_c) %>%
                  dplyr::select(iso3, area_name, area_level, area_id) %>%
                  st_drop_geometry(), by="iso3")  %>%
      mutate(dist = stringdist::stringdist(value, tolower(area_name))) %>%
      group_by(idx) %>%
      filter(dist == min(dist))
    
    ## NAOMI PRIORITY
    
    # min_dist_naomi <- pse_areas %>%
    #     filter(is.na(area_level)) %>%
    #     dplyr::select(-area_level) %>%
    #     full_join(areas %>%
    #               mutate(iso3 = iso3_c) %>%
    #               dplyr::select(iso3, area_name, area_level, area_id) %>%
    #               st_drop_geometry(), by="iso3")  %>%
    #     mutate(dist = stringdist::stringdist(value, tolower(area_name))) %>%
    #     group_by(idx) %>%
    #     filter(dist == min(dist))
    
    ####
    
    best_matches_naomi <- min_dist_naomi %>%
      bind_rows(min_dist_area_level) %>%
      filter(n() == 1, dist<3) %>%
      ungroup
    
    level_check <- min_dist_naomi %>%
      filter(dist==0, n()>1, !is.na(area_id)) %>%
      count(idx) %>%
      filter(n > 1)
    
    if(nrow(level_check)) {
      best_matches_naomi <- best_matches_naomi %>%
        bind_rows(
          # min_dist %>%
          #   filter(dist==0, n()>1, !is.na(area_id)) %>%
          #   filter(idx %in% level_check$idx),
          min_dist_naomi %>%
            filter(dist==0, n()>1, !is.na(area_id)) %>%
            left_join(areas %>% dplyr::select(area_id, area_level) %>% st_drop_geometry()) %>%
            filter(area_level == max(area_level))
        ) %>%
        ungroup
      
      warning("\nArea name matched several Naomi area IDs. The finest area level has been chosen\nThis is likely a district sharing the same name as its province. Check.\n")
      
    }
  
    ## NAOMI PRIORITY
    # assigned_areas_idx <- sort(best_matches_naomi$idx)
  
    # min_dist_cities <- pse_areas %>%
    #   filter(is.na(area_level)) %>%
    #   dplyr::select(-area_level) %>%
    #   filter(!idx %in% assigned_areas_idx) %>%
    #   # dplyr::select(iso3, given_area, year, kp, observation_idx, name, value, idx) %>%
    #   full_join(city_population %>% distinct(iso3, area_id, area_name),
    #             by="iso3") %>%
    #   mutate(dist = stringdist::stringdist(value, tolower(area_name))) %>%
    #   group_by(idx) %>%
    #   filter(dist == min(dist))
    
    # min_dist <- bind_rows(min_dist_area_level, min_dist_cities, min_dist_naomi)
    
    best_matches_cities <- min_dist_cities %>%
      filter(n() == 1, dist<3) %>%
      ungroup
    
    bad_match <- min_dist_cities %>%
      filter(dist>=3, !idx %in% best_matches_naomi$idx) %>%
      bind_rows(
        min_dist_naomi %>%
          select(-area_level) %>%
          filter(dist>=3, !idx %in% best_matches_cities$idx)
      ) %>%
      ungroup() %>%
      distinct(iso3, kp, year, given_area, area_name, area_id, dist)
    
    if (nrow(bad_match)) {
      bad_match_error <- bad_match %>%
        ungroup %>%
        mutate(iso3 = iso3_c) %>%
        dplyr::select(iso3, given_area, attempted_match = area_name, attempted_area_id = area_id, string_distance = dist)
      
      warning("\nString match is bad:\n", 
              paste0(utils::capture.output(bad_match_error), collapse = "\n"))
      
      
    } else {
      bad_match_error <- data.frame(iso3 = iso3_c, x = "No bad matches")
    }
    
    best_matches <- bind_rows(best_matches_cities, best_matches_naomi)
    
    province_name <- paste0("area_name", admin1_lvl)
    province_id <- paste0("area_id", admin1_lvl)
    
    area_reshape <- spread_areas(areas) %>%
      dplyr::select(all_of(province_name), starts_with("area_id")) %>%
      st_drop_geometry() %>%
      mutate(province_area_id = eval(parse(text=province_id))) %>%
      pivot_longer(-c(province_area_id, all_of(province_name))) %>%
      dplyr::select(-name, area_id = value, province = all_of(province_name), province_area_id) %>%
      distinct(province, province_area_id, area_id) %>%
      left_join(areas %>% select(area_id, area_level) %>% st_drop_geometry()) %>%
      filter(area_level >= admin1_lvl) %>%
      select(-area_level) %>%
      bind_rows(read_csv("depends/city_province_map.csv") %>%
                  dplyr::select(-area_name) %>%
                  rename(
                    province_area_id = area_id,
                    area_id = city_id)
      )
    
    if(admin1_lvl > 1) {
      area_reshape <- area_reshape %>%
        bind_rows(areas %>%
                    st_drop_geometry() %>%
                    filter(area_level < admin1_lvl) %>%
                    rename(province_area_id = parent_area_id) %>%
                    select(area_id, province_area_id) %>%
                    mutate(province_area_id = ifelse(is.na(province_area_id), iso3, province_area_id)) %>%
                    left_join(areas %>%
                                st_drop_geometry() %>%
                                select(area_id, area_name),
                              by = c("province_area_id" = "area_id")) %>%
                    rename(province = area_name)
        )
    }
    
    row_populations <- best_matches %>%
      left_join(area_reshape) %>%
      mutate(sex = case_when(
        # kp %in% c("PWID") ~ "both",
        kp %in% c("MSM", "TGM", "PWID") ~ "male",
        kp %in% c("FSW", "SW", "TG", "TGW") ~ "female"
      ),
      # age_group = case_when(
      #   kp %in% c("PWID", "FSW", "SW") ~ "Y015_049",
      #   kp %in% c("MSM", "TG", "TGW", "TGM") ~ "Y015_029"
      # )
      age_group = "Y015_049"
      ) %>%
      type_convert() %>%
      left_join(population %>% ungroup() %>% dplyr::select(area_id, year, sex, age_group, population) %>% type_convert()) %>%
      group_by(observation_idx) %>%
      mutate(province = ifelse(length(unique(province)) > 1, NA_character_, province),
             province_area_id = ifelse(length(unique(province)) > 1, NA_character_, province_area_id)) %>%
      group_by(observation_idx, area_id, province, province_area_id) %>%
      summarise(population = sum(population))
    
    
    if(nrow(row_populations) > 0) {
      
      row_populations <- row_populations %>%
        group_by(observation_idx) %>%
        mutate(population = sum(population)) %>%
        group_by(observation_idx) %>%
        mutate(rn = paste0("split_", row_number())) %>%
        pivot_wider(names_from = rn, values_from = area_id) %>%
        unite("area_id", starts_with("split"), sep = "; ") %>%
        mutate(area_id = str_remove_all(area_id, "; NA|NA; ")) %>%
        mutate(area_match = case_when(
          !str_detect(area_id, iso3) & str_detect(area_id, "GRUMP") ~ "GRUMP",
          str_detect(area_id, iso3) & !str_detect(area_id, "GRUMP") ~ "Naomi",
          str_detect(area_id, iso3) & str_detect(area_id, "GRUMP") ~ "Both"
        ))
      
      
      if(all.equal(row_populations$observation_idx, unique(row_populations$observation_idx)) != TRUE) {
        stop("Mixing Naomi and GRUMP populations or multiple area names")
      }
    } else {
      row_populations <- row_populations %>%
        mutate(area_match = NA)
    }
    
    pse <- pse %>%
      filter(is.na(prop_estimate)) %>%
      select(-c(prop_lower:prop_upper)) %>%
      left_join(row_populations) %>%
      mutate(prop_estimate = count_estimate/population,
             prop_lower = NA,
             prop_upper = NA) %>%
      bind_rows(
        pse %>%
          filter(!is.na(prop_estimate)) %>%
          left_join(row_populations)
      )
    
    if(iso3 == "BFA")
      pse <- pse %>% 
        filter(study_idx == 29) %>%
        select(-c(province, province_area_id)) %>%
        left_join(bfa_humd_areas %>% select(-area_name) %>% st_drop_geometry()) %>%
        bind_rows(pse %>%
                    filter(study_idx != 29))
    
    pse <- pse %>%
          mutate(province_area_id = case_when(
            province_area_id == "CMR_1_3" ~ "CMR_1_5", # Douala merged into Littoral
            province_area_id == "CMR_1_12" ~ "CMR_1_2", # Yaounde merged into Centre
            province_area_id == "CMR_1_4" ~ "CMR_1_3", 
            province_area_id == "CMR_1_5" ~ "CMR_1_4", 
            province_area_id == "CMR_1_6" ~ "CMR_1_5", 
            province_area_id == "CMR_1_7" ~ "CMR_1_6", 
            province_area_id == "CMR_1_8" ~ "CMR_1_7", 
            province_area_id == "CMR_1_9" ~ "CMR_1_8", 
            province_area_id == "CMR_1_11" ~ "CMR_1_9",
            
            province_area_id == "ZMB_1_10" ~ "ZMB_1_10gh",
            province_area_id == "ZMB_1_17" ~ "ZMB_1_17xt",
            province_area_id == "ZMB_1_12" ~ "ZMB_1_12sg",
            province_area_id == "ZMB_1_14" ~ "ZMB_1_14fz",
            province_area_id == "ZMB_1_15" ~ "ZMB_1_15pi",
            
            TRUE ~ province_area_id
          ))
    
    pse <- pse %>%
      # bind_rows(pse_already_assigned) %>%
      mutate(
        country.name = countrycode(iso3_c, "iso3c", "country.name"),
        surveillance_type = NA,
        indicator = "Population size estimate",
        sample = NA,
        age_group = NA,
        notes = NA,
        link = NA) %>%
      mutate(simple_method = case_when(
        simple_method == "PLACE/mapping" & method != "PLACE" ~ "mapping", 
        method == "PLACE" ~ "PLACE",
        TRUE ~ simple_method)) %>%
      select(-method) %>%
      rename(method = simple_method) %>%
      dplyr::select(all_of(c("country.name", "data_checked", "surveillance_type", "indicator", "method", "kp", "sex", "age_group", "area_name", "area_match", "area_id", "province", "province_area_id", "year", "count_lower", "count_estimate", "count_upper", "population", "prop_lower", "prop_estimate", "prop_upper", "sample", "notes", "study_idx", "observation_idx", "ref", "link")))
      # arrange(country.name, kp, year)
    
    
    
    
} else {
  
  pse <- data.frame()
  bad_match_error <- data.frame()
  
}


write_csv(pse, "pse_prevalence.csv", na = "")
write_csv(bad_match_error, "bad_match_error.csv")