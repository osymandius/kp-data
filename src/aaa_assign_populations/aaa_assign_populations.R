# extract_kp_worldpop <- function(areas, iso3, 
#                                 years = c(2000, 2005, 2010, 2015, 2020)) {
#                                 # years = c(2015)) {
#   stopifnot(inherits(areas, "sf"))
#   stopifnot(grepl("^[A-Z]{3}$", iso3))
#   stopifnot(years %in% 2000:2020)
#   wp_ages <- c(`15` = "Y015_019", `20` = "Y020_024",
#                `25` = "Y025_029", `30` = "Y030_034", `35` = "Y035_039",
#                `40` = "Y040_044", `45` = "Y045_049"
#   )
#   wp_sexes <- c(m = "male", f = "female")
#   # wp_sexes <- c(m = "male")
#   grid <- expand.grid(iso3 = iso3, year = years, wp_age = names(wp_ages), 
#                       wp_sex = names(wp_sexes), stringsAsFactors = FALSE)
#   pop_list <- do.call(Map, c(f = naomi.utils:::worldpop_extract_one, areas = list(list(areas)), 
#                              grid))
#   pop <- dplyr::bind_rows(pop_list)
#   pop$age_group <- dplyr::recode(pop$wp_age, !!!wp_ages)
#   pop$sex <- dplyr::recode(pop$wp_sex, !!!wp_sexes)
#   pop$calendar_quarter <- paste0("CY", pop$year, "Q2")
#   pop$source <- "WorldPop"
#   # pop <- pop %>% dplyr::left_join(dplyr::select(sf::st_drop_geometry(areas), 
#   # area_id, area_name), by = "area_id")
#   pop <- dplyr::count(pop, area_id, source, calendar_quarter, 
#                       sex, age_group, wt = population, name = "population")
#   # pop$asfr <- NA_real_
#   pop$age_group <- "Y015_049"
#   # validate_naomi_population(pop, areas, unique(areas$area_level))
#   pop
# }

iso3_c <- iso3

areas <- read_sf("depends/naomi_areas.geojson")%>%
  mutate(iso3 = iso3_c)

population <- read.csv("depends/interpolated_population.csv") %>%
  left_join(areas %>% dplyr::select(area_id, area_name) %>% st_drop_geometry()) %>%
  mutate(area_name = str_to_sentence(area_name))

naomi_names <- unique(population$area_name)

if(iso3 != "SSD") {
  city_population <- read.csv("depends/interpolated_city_population.csv") %>%
    mutate(area_name = str_to_sentence(area_name)) %>%
    filter(!area_name %in% naomi_names)
} else {
  city_population <- NULL
}


# merge_cities <- read_sf("merge_cities.geojson")

# sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))
# 
# pse_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Analytical datasets/key-populations/PSE", "pse.csv")
# pse <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = pse_path)
# pse <- read_csv(pse)
# 
# gf_pse_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Analytical datasets/key-populations/PSE", "pse_gf_flib.csv")
# gf_pse <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = gf_pse_path)
# gf_pse <- read_csv(gf_pse)
# 
# pse <- pse %>%
#   mutate(iso3 = countrycode(country.name, "country.name", "iso3c")) %>%
#   filter(iso3 == iso3_c) %>%
#   rename(notes = method)
# 
# gf_pse <- gf_pse %>%
#   mutate(iso3 = countrycode(country.name, "country.name", "iso3c")) %>%
#   filter(iso3 == iso3_c)

population <- population %>%
  bind_rows(city_population) %>%
  left_join(get_age_groups() %>% select(age_group, age_group_sort_order)) %>%
  filter(age_group_sort_order %in% 16:22) %>%
  group_by(area_id, area_name, year, sex) %>%
  summarise(population = sum(population)) %>%
  mutate(age_group = "Y015_049",
         iso3 = iso3) %>%
  ungroup()

population <- population %>%
  bind_rows(
    population %>%
      group_by(iso3, area_id, area_name, year, age_group) %>%
      summarise(population = sum(population)) %>%
      mutate(sex = "both") %>%
      ungroup()
  )


# cities_areas <- merge_cities %>%
#   bind_rows(areas)

pse_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Analytical datasets/key-populations/PSE", "pse_spreadsheet_cleaned.csv")
pse <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = pse_path)
pse <- read_csv(pse) 

pse <- pse %>%
  # bind_rows(gf_pse) %>%
  mutate(iso3 = countrycode(country.name, "country.name", "iso3c")) %>%
  filter(iso3 == iso3_c) %>%
  # select(iso3:pse_upper, uid) %>%
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
    select(iso3, area_name, year, kp, row_id) %>%
    mutate(area_name = str_replace_all(area_name, "\\,|\\/", "\\;")) %>%
    distinct() %>%
    separate(area_name, sep = ";", into = paste0("area_split", 1:20), remove=FALSE) %>%
    mutate(across(starts_with("area_split"), ~str_trim(.x))) %>%
    pivot_longer(-c(iso3, area_name, year, row_id, kp)) %>%
    filter(!is.na(value)) %>%
    mutate(idx = row_number(),
           value = tolower(value)) %>%
    rename(given_area = area_name)
  
  min_dist <- pse_areas %>%
    full_join(population %>% select(iso3, area_name, area_id) %>% distinct(), by="iso3")  %>%
    mutate(dist = stringdist::stringdist(value, tolower(area_name))) %>%
    group_by(idx) %>%
    filter(dist == min(dist)) 
  
  best_matches <- min_dist %>%
    filter(n() == 1, dist<3) %>%
    ungroup
  
  level_check <- min_dist %>%
    filter(dist==0, n()>1, !is.na(area_id)) %>%
    count(idx) %>%
    filter(n > 1)
  
  if(nrow(level_check)) {
    best_matches <- best_matches %>%
      bind_rows(
        # min_dist %>%
        #   filter(dist==0, n()>1, !is.na(area_id)) %>%
        #   filter(idx %in% level_check$idx),
        min_dist %>%
          filter(dist==0, n()>1, !is.na(area_id)) %>%
          left_join(areas %>% select(area_id, area_level) %>% st_drop_geometry()) %>%
          filter(area_level == max(area_level))
      ) %>%
      ungroup
    
    warning("\nArea name matched several Naomi area IDs. The finest area level has been chosen\nThis is likely a district sharing the same name as its province. Check.\n")
    
  }
  
  bad_match <- min_dist %>%
    filter(dist>=3)
  
  if (nrow(bad_match)) {
    bad_match_error <- bad_match %>%
      ungroup %>%
      mutate(iso3 = iso3_c) %>%
      select(iso3, given_name = value, attempted_match = area_name, attempted_area_id = area_id, string_distance = dist)
    
    warning("\nString match is bad:\n", 
            paste0(utils::capture.output(bad_match_error), collapse = "\n"))
    
    
  } else {
    bad_match_error <- data.frame(iso3 = iso3_c, x = "No bad matches")
  }
  
  area_reshape <- spread_areas(areas) %>%
    select(area_name1, starts_with("area_id")) %>%
    st_drop_geometry() %>%
    pivot_longer(-area_name1) %>%
    select(-name, area_id = value, province = area_name1) %>%
    distinct(province, area_id) %>%
    filter(area_id != iso3)
  
  row_populations <- best_matches %>%
    left_join(area_reshape) %>%
    mutate(sex = case_when(
      kp %in% c("PWID") ~ "both",
      kp %in% c("MSM", "TGM") ~ "male",
      kp %in% c("FSW", "SW", "TG", "TGW") ~ "female"
    )) %>%
    type_convert() %>%
    left_join(population %>% select(area_id, year, sex, population) %>% type_convert()) %>%
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
        method %in% c("Unique object", "Unique object multiplier", "Unique Object Multiploer") ~ "Object/event multiplier",
        method %in% c("Service Multiplier", "Service multiplier", "Multiplier") ~ "Service multiplier",
        method %in% c("Unique event multiplier", "Unique event", "Event multiplier") ~ "Object/event multiplier",
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
    select(all_of(c("country.name", "data_checked", "surveillance_type", "indicator", "method", "kp", "sex", "age_group", "area_name", "province", "year", "pse_lower", "pse", "pse_upper", "population", "prop_lower", "population_proportion", "prop_upper", "sample", "notes", "ref", "link")))
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
#     select(area_id, geometry) %>%
#     distinct() %>%
#     mutate(source = "WorldPop")
#   
#   class(pop_search_unique) <- c("sf", "tbl_df", "tbl", "data.frame")
#   attr(pop_search_unique, "sf_column") <- "geometry"
#   
#   pop_search_res <- extract_kp_worldpop(pop_search_unique, iso3_c) 
#   
#   extrapolate_id <- pop_search_unique %>%
#     select(area_id, source) %>%
#     bind_rows(
#       best_matches %>%
#         filter(area_id %in% areas$area_id) %>%
#         select(area_id) %>%
#         distinct() %>%
#         mutate(source = "Naomi")
#     ) %>% 
#     st_drop_geometry()
#   
#   pop_search_res <- pop_search_res %>%
#     separate(calendar_quarter, remove=FALSE, sep = c(2,6), into=c(NA, "year", NA), convert = TRUE) %>%
#     select(area_id, year, sex, age_group, population)
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
#         select(area_id, year, sex, age_group, population)
#     )
#   
# } else {
#   
#   extrapolate_id <- best_matches %>%
#     filter(area_id %in% areas$area_id) %>%
#     select(area_id) %>%
#     distinct() %>%
#     mutate(source = "Naomi")
#   
#   merged_populations <- population %>%
#     select(area_id, year, sex, age_group, population)
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
# #   left_join(spectrum_population_change %>% select(year, sex, source, change)) %>%
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
#   select(row_id, area_id, year, kp) %>%
#   mutate(sex = case_when(
#     kp %in% c("PWID") ~ "both",
#     kp %in% c("MSM", "TGM") ~ "male",
#     kp %in% c("FSW", "SW", "TG", "TGW") ~ "female"
#   )) %>%
#   type.convert() %>%
#   left_join(merged_populations) %>%
#   group_by(row_id) %>%
#   summarise(population = sum(population))


