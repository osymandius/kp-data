urban_boundaries <- read_sf("~/Downloads/grump-v1-urban-ext-polygons-rev02-shp/grump-v1-urban-ext-polygons-rev02-shp/global_urban_extent_polygons_v1.01.shp")
urban_points <- read_sf("~/Downloads/grump-v1-settlement-points-rev01-shp/global_settlement_points_v1.01.shp")

urban_boundaries <- urban_boundaries %>%
  select(ISO3, NAME) %>%
  filter(ISO3 %in% ssa_iso3) %>%
  mutate(idx = row_number())

urban_points <- urban_points %>%
  filter(ISO3 %in% ssa_iso3)

merge_cities <- st_intersection(urban_points, urban_boundaries %>% st_make_valid())

merge_cities <- merge_cities %>% 
  select(ISO3, NAME, idx) %>%
  st_drop_geometry() %>%
  left_join(urban_boundaries) %>%
  select(-idx) %>%
  rename(iso3 = ISO3, area_name = NAME) %>%
  mutate(area_name = tolower(area_name))

cities_areas <- merge_cities %>%
  bind_rows(areas)

pse <- pse %>%
  mutate(row_id = row_number())
  
pse_areas <- pse %>%
  select(iso3, area_name, year, kp, row_id) %>%
  mutate(area_name = str_replace(area_name, "\\,", "\\;")) %>%
  distinct() %>%
  separate(area_name, sep = ";", into = paste0("area_split", 1:20), remove=FALSE) %>%
  mutate(across(starts_with("area_split"), ~str_trim(.x))) %>%
  pivot_longer(-c(iso3, area_name, year, row_id, kp)) %>%
  filter(!is.na(value)) %>%
  mutate(idx = row_number(),
         value = tolower(value)) %>%
  rename(given_area = area_name)

min_dist <- pse_areas %>%
  filter(iso3 == iso3_c) %>%
  full_join(cities_areas %>% filter(iso3 == iso3_c), by="iso3")  %>%
  mutate(dist = stringdist::stringdist(value, area_name)) %>%
  group_by(idx) %>%
  filter(dist == min(dist)) 

best_matches <- min_dist %>%
  filter(n() == 1, dist<3) %>%
  ungroup

level_check <- min_dist %>%
  filter(dist==0, n()>1) %>%
  count(idx, area_level) %>%
  filter(n() == 1)

if(nrow(level_check))
  best_matches <- best_matches %>%
  bind_rows(
    min_dist %>%
      filter(dist==0, n()>1) %>%
      filter(idx %in% level_check$idx),
    min_dist %>%
      filter(dist==0, n()>1) %>%
      filter(!idx %in% level_check$idx) %>%
      filter(area_level == max(area_level))
  ) %>%
  ungroup

bad_match <- min_dist %>%
  filter(n() == 1, dist>=3)

if (nrow(bad_match)) {
  bad_match_error <- bad_match %>%
    select(given_area, area_name, area_id, dist) %>%
    ungroup
  
  warning("\nString match is bad:\n", 
          paste0(utils::capture.output(bad_match_error), collapse = "\n"))
}

pop_search <- best_matches %>%
  ungroup %>%
  filter(is.na(area_id)) %>%
  mutate(city_num  = group_indices(., area_name),
         area_id = paste0(iso3_c, "_city_", city_num)
  )

pop_search_unique <- pop_search %>%
  filter(!area_id %in% areas$area_id) %>%
  select(area_id, geometry) %>%
  distinct()

class(pop_search_unique) <- c("sf", "tbl_df", "tbl", "data.frame")
attr(pop_search_unique, "sf_column") <- "geometry"

pop_search_res <- extract_kp_worldpop(pop_search_unique, iso3_c) 

pop_search_res <- pop_search_res %>%
  group_by(area_id, calendar_quarter, sex) %>%
  summarise(population = sum(population)) %>%
  mutate(age_group = "Y015_049") %>%
  ungroup()

#### Extrapolate city populations

if(unique(spectrum$spectrum_region_code)) {
  spectrum <- spectrum %>%
    left_join(areas %>% 
                filter(area_level == 1) %>%
                select(spectrum_region_code, area_id, area_level) %>% 
                st_drop_geometry()
    )
  
  working_level <- 1
  stop("Subnational file!!")
} else {
  spectrum <- spectrum %>%
    left_join(areas %>% 
                filter(area_level == 0) %>%
                select(spectrum_region_code, area_id, area_level) %>% 
                st_drop_geometry()
    )
  working_level <- 0
}

spectrum <- spectrum %>%
  filter(age %in% 15:49) %>%
  group_by(area_id, area_level, sex, year) %>%
  summarise(population = sum(totpop)) %>%
  group_by(area_id, sex) %>%
  mutate(age_group = "Y015_049") %>%
  ungroup()

spectrum_population_change_city <- spectrum %>%
  mutate(change = population/population[year==2010]) %>% ungroup## If extract multiple years of worldpop, deal with this line

extrapolated_city_populations <- crossing(pop_search_unique$area_id,
                                     sex = c("female", "male"),
                                     year = 1970:2025
) %>%
  left_join(spectrum_population_change_city %>% select(year, sex, change)) %>%
  left_join(pop_search_res %>% select(area_id, sex, population)) %>%
  mutate(population = population*change) %>%
  group_by(area_id, sex, year) %>%
  summarise(population = sum(population)) %>%
  ungroup

extrapolated_city_populations <- extrapolated_city_populations %>%
  bind_rows(
    extrapolated_city_populations %>%
      group_by(area_id, year) %>%
      summarise(population = sum(population)) %>%
      mutate(sex = "both") %>%
      ungroup()
  )

row_populations <- pop_search %>%
  select(row_id, area_id, year, kp) %>%
  mutate(sex = case_when(
    kp %in% c("PWID", "TG") ~ "both",
    kp == "MSM" ~ "male",
    kp %in% c("FSW", "SW") ~ "female"
  )) %>%
  left_join(extrapolated_city_populations) %>%
  group_by(row_id) %>%
  summarise(population = sum(population))

### Extrapolate Naomi populations

best_matches %>%
  filter(area_id %in% areas$area_id)

pse %>%
  left_join(row_populations) %>%
  filter(!is.na(population)) %>%
  select(iso3, kp, area_name, year, pse_lower, pse, pse_upper, population)
