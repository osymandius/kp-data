iso3_c <- iso3

areas <- read_sf("depends/naomi_areas.geojson")%>%
  mutate(iso3 = iso3_c)

# merge_cities <- read_sf("depends/")

population <- read.csv("depends/interpolated_population.csv") %>%
  left_join(areas %>% dplyr::select(area_id, area_name) %>% st_drop_geometry()) %>%
  mutate(area_name = str_to_sentence(area_name))

# naomi_names <- unique(population$area_name)

if(iso3 != "SSD") {
  city_population <- read.csv("depends/interpolated_city_population.csv") %>%
    mutate(area_name = str_to_sentence(area_name),
           iso3 = iso3_c
    )
} else {
  city_population <- data.frame(iso3 = iso3_c)
}


population <- population %>%
  bind_rows(city_population)

# population <- population %>%
#   mutate(msm_age = ifelse(age_group %in% c("Y015_019", "Y020_024", "Y025_029"), 1, 0)) %>%
#   filter(msm_age == 1) %>%
#   group_by(area_id, area_name, year, sex) %>%
#   summarise(population = sum(population)) %>%
#   mutate(age_group = "Y015_029") %>%
#   bind_rows(
#     population %>%
#       five_year_to_15to49("population") %>%
#       sex_aggregation("population")
#   )

population <- population %>%
  five_year_to_15to49("population") %>%
  sex_aggregation("population")

extrap_pop <- population %>%
  filter(year %in% 2015:2020, !is.na(population)) %>%
  group_by(area_id, area_name, age_group, sex) %>%
  mutate(
    population = log(population),
    population = exp(Hmisc::approxExtrap(year, population, xout = 2020:2025)$y),
    year = year + 5)

population <- bind_rows(
  population,
  extrap_pop
)

pse_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Analytical datasets/key-populations/PSE", "pse_spreadsheet_cleaned_sourced.csv")
pse <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = pse_path)
pse <- read_csv(pse) 

pse <- pse %>%
  mutate(iso3 = countrycode(country.name, "country.name", "iso3c")) %>%
  filter(iso3 == iso3_c) %>%
  dplyr::select(iso3:pse_upper, area_level, ref, data_checked, uid) %>%
  mutate(sex = case_when(
    kp %in% c("FSW", "TG") ~ "female",
    kp == "MSM" ~ "male",
    kp == "PWID" ~ "both"
  )) %>%
  # distinct(kp, area_name, year, pse, pse_lower, pse_upper, .keep_all=TRUE) %>%
  mutate(row_id = row_number())
         # iso3 = countrycode(country.name, "country.name", "iso3c")) 

if(nrow(pse)) {
  
  pse_areas <- pse %>%
    dplyr::select(iso3, area_name, area_level, year, kp, row_id) %>%
    mutate(
      area_name = str_replace_all(area_name, "\\,\\,|\\,|\\/", "\\;")) %>%
    distinct() %>%
    separate(area_name, sep = ";", into = paste0("area_split", 1:20), remove=FALSE) %>%
    mutate(across(starts_with("area_split"), ~str_trim(.x))) %>%
    pivot_longer(-c(iso3, area_name, area_level, year, row_id, kp)) %>%
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
    mutate(dist = stringdist::stringdist(value, tolower(area_name))) %>%
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

  assigned_areas_idx <- min_dist_cities %>%
    filter(n() == 1, dist<3) %>%
    pull(idx)

  min_dist_naomi <- pse_areas %>%
    filter(is.na(area_level)) %>%
    dplyr::select(-area_level) %>%
    filter(!idx %in% assigned_areas_idx) %>%
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
  #   # dplyr::select(iso3, given_area, year, kp, row_id, name, value, idx) %>%
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
  
  area_reshape <- spread_areas(areas) %>%
    dplyr::select(area_name1, starts_with("area_id")) %>%
    st_drop_geometry() %>%
    pivot_longer(-area_name1) %>%
    dplyr::select(-name, area_id = value, province = area_name1) %>%
    distinct(province, area_id) %>%
    filter(area_id != iso3) %>%
    bind_rows(read_csv("depends/city_province_map.csv") %>%
                dplyr::select(-area_name))
  
  row_populations <- best_matches %>%
    left_join(area_reshape) %>%
    mutate(sex = case_when(
      kp %in% c("PWID") ~ "both",
      kp %in% c("MSM", "TGM") ~ "male",
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
    group_by(row_id) %>%
    mutate(province = ifelse(length(unique(province)) > 1, NA_character_, province)) %>%
    group_by(row_id, province) %>%
    summarise(population = sum(population))
  
  pse <- pse %>%
    filter(iso3 == iso3_c) %>%
    left_join(row_populations) %>%
    # filter(!is.na(population)) %>%
    # rename(notes = method) %>%
    mutate(population_proportion = pse/population,
           # method = NA,
           country.name = countrycode(iso3_c, "iso3c", "country.name"),
           surveillance_type = NA,
           indicator = "Population size estimate",
           prop_lower = NA,
           prop_upper = NA,
           sample = NA,
           age_group = NA,
           notes = NA,
           link = NA) %>%
    mutate(
      method = case_when(
        method == "" ~ NA_character_,
        method %in% c("Programmatic mapping", "Hotspot mapping", "PLACE", "Enumeration/mapping", "Mapping", "Mapping and enumeration") ~ "PLACE/Mapping",
        method %in% c("Unique object", "Unique object multiplier", "Unique Object Multiploer") ~ "Object multiplier",
        method %in% c("Service Multiplier", "Service multiplier", "Multiplier") ~ "Service multiplier",
        method %in% c("Unique event multiplier", "Unique event", "Event multiplier") ~ "Event multiplier",
        method %in% c("Capture - recapture", "CRC", "2S-CEC") ~ "2S-CRC",
        method %in% c("Wisdom of Crowds, unique object distribution, social event and successive-sampling methods",
                      "Service multiplier, Unique Object , Literature SS-PSE(RDS-A), Unique event & Consensus approach-Modified Delphi",
                      "Mapping, Enumeration and literature review",
                      "Consensus and mapping",
                      "Unique object, WODM, service multiplier, social multiplier, 2S-CRC",
                      "Wisdom of Crowds, unique object multiplier, social events and SS-PSE",
                      "WODC, unique object, service, social, 2S-CRC",
                      "Wisdom of the masses and capture-recapture",
                      "Wisdom of the masses and social multiplier",
                      "Triangulation of the follow methods unique object multiplier, wisdom of the masses,  capture â€“ recapture  and multiplier",
                      "Unique Object multiplier et wisdom of the masses, NSUM and capture-recapture",
                      "Unique object multiplier, Census, Respondent driven sampling survey and service multiplier method",
                      "Unique object multiplier, social event, wisdom of the masses and NSUM",
                      "WODC, unique object, service, social",
                      "Literature review (meta-analysis model for surveys) and Delphi method",
                      "Literature review (meta-analysis) and Delphi method",
                      "Literature review and individual interviews",
                      "Delphi method and consensus",
                      "Consensus of % adult population and population growth",
                      "Consensus (service, literature, unique object, mapping, WOTC)",
                      "Median of unique object, WOTC",
                      "Median of unique object, WOTC, 2S-CRC",
                      "Programme data",
                      "RDS",
                      "Snowball",
                      "Triangulation",
                      "Wisdom of the masses, unique object multiplier and social event"
                      
        ) ~ "Multiple methods - mixture",
        method %in% c("Service multiplier, unique object multiplier,  literature review, RDSAnalyst  SS-PSE Method",
                      "Unique object multiplier, service multiplier, event multiplier",
                      "Bayesian synthesis - multiplier and SS-PSE",
                      "Unique object and special event multiplier",
                      "Unique object, social, service, 2S-CRC",
                      "Capture-recapture, unique object multiplier and register multiplier",
                      "Capture-recapture and NSUM",
                      "Multiplier, capture-recapture and social event",
                      "Unique object, event and service multipliers, SS-PSE, and a synthesis of the methods using the Anchored Multiplier",
                      "Unique object, service multiplier and NSUM",
                      "Unique object, social, service",
                      "Consensus (SS-PSE, unique object, multiplier, 1% recommendation)",
                      "Consensus (SS-PSE, unique object, multiplier)",
                      "Bayesian synthesis (SS-PSE, literature, something else)"
                      
                      
        ) ~ "Multiple methods - empirical",
        TRUE ~ method
      )) %>%
    dplyr::select(all_of(c("country.name", "data_checked", "surveillance_type", "indicator", "method", "kp", "sex", "age_group", "area_name", "province", "year", "pse_lower", "pse", "pse_upper", "population", "prop_lower", "population_proportion", "prop_upper", "sample", "notes", "ref", "link")))
    # arrange(country.name, kp, year)
  
  
  
  
} else {
  
  pse <- data.frame(iso3 = iso3_c, x = "No KP PSE available")
  bad_match_error <- data.frame(iso3 = iso3_c, x = "No KP PSE available")
  
}


write_csv(pse, "pse_prevalence.csv", na = "")
write_csv(bad_match_error, "bad_match_error.csv")

# pop_search <- best_matches %>%
#   ungroup %>%
#   filter(is.na(area_id)) %>%
#   mutate(city_num  = group_indices(., area_name),
#          area_id = paste0(iso3_c, "_city_", city_num)
#   )
# 
# if(nrow(pop_search)) {
#   
#   pop_search_unique <- pop_search %>%
#     filter(!area_id %in% areas$area_id) %>%
#     dplyr::select(area_id, geometry) %>%
#     distinct() %>%
#     mutate(source = "WorldPop")
#   
#   class(pop_search_unique) <- c("sf", "tbl_df", "tbl", "data.frame")
#   attr(pop_search_unique, "sf_column") <- "geometry"
#   
#   pop_search_res <- extract_kp_worldpop(pop_search_unique, iso3_c) 
#   
#   extrapolate_id <- pop_search_unique %>%
#     dplyr::select(area_id, source) %>%
#     bind_rows(
#       best_matches %>%
#         filter(area_id %in% areas$area_id) %>%
#         dplyr::select(area_id) %>%
#         distinct() %>%
#         mutate(source = "Naomi")
#     ) %>% 
#     st_drop_geometry()
#   
#   pop_search_res <- pop_search_res %>%
#     separate(calendar_quarter, remove=FALSE, sep = c(2,6), into=c(NA, "year", NA), convert = TRUE) %>%
#     dplyr::select(area_id, year, sex, age_group, population)
#     
#   merged_populations <- crossing(area_id = pop_search_res$area_id,
#              year = 2000:2020,
#              age_group = "Y015_049",
#              sex = c("female", "male")) %>%
#     left_join(pop_search_res) %>%
#     group_by(area_id, sex) %>%
#     mutate(population = log(population),
#            population = zoo::na.approx(population, na.rm=FALSE),
#            population = exp(population)) %>%
#     bind_rows(
#       population %>%
#         dplyr::select(area_id, year, sex, age_group, population)
#     )
#   
# } else {
#   
#   extrapolate_id <- best_matches %>%
#     filter(area_id %in% areas$area_id) %>%
#     dplyr::select(area_id) %>%
#     distinct() %>%
#     mutate(source = "Naomi")
#   
#   merged_populations <- population %>%
#     dplyr::select(area_id, year, sex, age_group, population)
#   
# }
# 
# #### Extrapolate city populations
# 
# ## Aggregate Naomi populations
# 
# 
# # extrapolated_populations <- crossing(extrapolate_id,
# #                                      sex = c("female", "male"),
# #                                      year = 1970:2025
# # ) %>%
# #   left_join(spectrum_population_change %>% dplyr::select(year, sex, source, change)) %>%
# #   left_join(merged_populations) %>%
# #   mutate(population = population*change) %>%
# #   group_by(area_id, sex, year) %>%
# #   summarise(population = sum(population)) %>%
# #   ungroup
# 
# merged_populations <- merged_populations %>%
#   bind_rows(
#     merged_populations %>%
#       group_by(area_id, year, age_group) %>%
#       summarise(population = sum(population)) %>%
#       mutate(sex = "both") %>%
#       ungroup()
#   )
# 
# row_populations <- pop_search %>%
#   bind_rows(best_matches %>% filter(!is.na(area_id))) %>%
#   dplyr::select(row_id, area_id, year, kp) %>%
#   mutate(sex = case_when(
#     kp %in% c("PWID") ~ "both",
#     kp %in% c("MSM", "TGM") ~ "male",
#     kp %in% c("FSW", "SW", "TG", "TGW") ~ "female"
#   )) %>%
#   type.convert() %>%
#   left_join(merged_populations) %>%
#   group_by(row_id) %>%
#   summarise(population = sum(population))


