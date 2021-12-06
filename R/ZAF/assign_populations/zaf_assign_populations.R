library(spud)

pop_id <- orderly::orderly_search("aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', "ZAF", '")'), draft = FALSE)

iso3 <- iso3_c <- "ZAF"

areas <- read_sf("archive/zaf_data_areas/20210419-091902-c45916f6/zaf_areas.geojson")%>%
  mutate(iso3 = iso3_c) %>%
  filter(area_level < 3)

population <- read_csv(paste0("archive/aaa_scale_pop/", pop_id, "/interpolated_population.csv")) %>%
  left_join(areas %>% dplyr::select(area_id, area_name) %>% st_drop_geometry()) %>%
  mutate(area_name = str_to_sentence(area_name)) %>%
  filter(!is.na(area_name))

naomi_names <- unique(population$area_name)

city_population <- read_csv("R/ZAF/download_worldpop/interpolated_city_population.csv") %>%
  mutate(area_name = str_to_sentence(area_name)) %>%
  filter(!area_name %in% naomi_names)

# merge_cities <- read_sf("merge_cities.geojson")

# sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))
# 
# pse_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Analytical datasets/key-populations", "pse_surveillance_only.csv")
# pse <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = pse_path)
# pse <- read_csv(pse)

pse <- read_csv("src/aaa_assign_populations/2021_11_27_mapped_place_pse.csv") %>%
  bind_rows(read_csv("src/aaa_assign_populations/2021_11_27_checked_and_unknown_pse.csv"))

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

pse <- pse %>%
  distinct(kp, area_name, year, pse, .keep_all=TRUE) %>%
  mutate(row_id = row_number(),
         iso3 = countrycode(country.name, "country.name", "iso3c")) %>%
  filter(iso3 == iso3_c) %>%
  select(iso3:pse_upper, uid) %>%
  mutate(sex = case_when(
    kp %in% c("FSW", "TG") ~ "female",
    kp == "MSM" ~ "male",
    kp == "PWID" ~ "both"
  ),
  row_id = row_number())

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
  
  row_populations <- best_matches %>%
    mutate(sex = case_when(
      kp %in% c("PWID") ~ "both",
      kp %in% c("MSM", "TGM") ~ "male",
      kp %in% c("FSW", "SW", "TG", "TGW") ~ "female"
    )) %>%
    type_convert() %>%
    left_join(population %>% type_convert()) %>%
    group_by(row_id) %>%
    summarise(population = sum(population))
  
  pse <- pse %>%
    filter(iso3 == iso3_c) %>%
    left_join(row_populations) %>%
    # filter(!is.na(population)) %>%
    mutate(population_proportion = pse/population) %>%
    select(any_of(c("country.name", "surveillance_type", "indicator", "method", "kp", "sex", "age_group", "area_name", "province", "year", "pse_lower", "pse", "pse_upper", "population", "prop_lower", "population_proportion", "prop_upper", "sample", "notes", "ref", "link", "uid"))) %>%
    arrange(country.name, kp, year)
  
  
  
  
} else {
  
  pse <- data.frame(iso3 = iso3_c, x = "No KP PSE available")
  bad_match_error <- data.frame(iso3 = iso3_c, x = "No KP PSE available")
  
}


write_csv(pse, "R/ZAF/assign_populations/deduplicated_pse_proportions.csv")
write_csv(bad_match_error, "R/ZAF/assign_populations/bad_match_error.csv")

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


