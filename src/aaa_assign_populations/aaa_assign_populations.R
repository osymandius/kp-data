extract_kp_worldpop <- function(areas, iso3, 
                                years = c(2000, 2005, 2010, 2015, 2020)) {
                                # years = c(2015)) {
  stopifnot(inherits(areas, "sf"))
  stopifnot(grepl("^[A-Z]{3}$", iso3))
  stopifnot(years %in% 2000:2020)
  wp_ages <- c(`15` = "Y015_019", `20` = "Y020_024",
               `25` = "Y025_029", `30` = "Y030_034", `35` = "Y035_039",
               `40` = "Y040_044", `45` = "Y045_049"
  )
  wp_sexes <- c(m = "male", f = "female")
  # wp_sexes <- c(f = "female")
  grid <- expand.grid(iso3 = iso3, year = years, wp_age = names(wp_ages), 
                      wp_sex = names(wp_sexes), stringsAsFactors = FALSE)
  pop_list <- do.call(Map, c(f = naomi.utils:::worldpop_extract_one, areas = list(list(areas)), 
                             grid))
  pop <- dplyr::bind_rows(pop_list)
  pop$age_group <- dplyr::recode(pop$wp_age, !!!wp_ages)
  pop$sex <- dplyr::recode(pop$wp_sex, !!!wp_sexes)
  pop$calendar_quarter <- paste0("CY", pop$year, "Q2")
  pop$source <- "WorldPop"
  # pop <- pop %>% dplyr::left_join(dplyr::select(sf::st_drop_geometry(areas), 
  # area_id, area_name), by = "area_id")
  pop <- dplyr::count(pop, area_id, source, calendar_quarter, 
                      sex, age_group, wt = population, name = "population")
  # pop$asfr <- NA_real_
  # pop$age_group <- "Y015_049"
  # validate_naomi_population(pop, areas, unique(areas$area_level))
  pop
}

iso3_c <- iso3

areas <- read_sf("depends/naomi_areas.geojson")%>%
  mutate(iso3 = iso3_c)
population <- read_csv("depends/interpolated_population.csv")
# spectrum <- extract_pjnz_naomi("depends/spectrum_file.zip")
merge_cities <- read_sf("merge_cities.geojson")

sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))

pse_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Analytical datasets/key-populations", "pse.csv")
pse <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = pse_path)
pse <- read_csv(pse)

population <- population %>%
  left_join(get_age_groups() %>% select(age_group, age_group_sort_order)) %>%
  filter(age_group_sort_order %in% 16:22) %>%
  group_by(area_id, year, sex) %>%
  summarise(population = sum(population)) %>%
  mutate(age_group = "Y015_049") %>%
  ungroup()

# naomi_pop_year <- unique(population$year)
# 
# if(unique(spectrum$spectrum_region_code)) {
#   spectrum <- spectrum %>%
#     left_join(areas %>% 
#                 filter(area_level == 1) %>%
#                 select(spectrum_region_code, area_id, area_level) %>% 
#                 st_drop_geometry()
#     )
#   
#   working_level <- 1
#   stop("Subnational file!!")
# } else {
#   spectrum <- spectrum %>%
#     left_join(areas %>% 
#                 filter(area_level == 0) %>%
#                 select(spectrum_region_code, area_id, area_level) %>% 
#                 st_drop_geometry()
#     )
#   working_level <- 0
# }
# 
# spectrum <- spectrum %>%
#   filter(age %in% 15:49) %>%
#   group_by(area_id, area_level, sex, year) %>%
#   summarise(population = sum(totpop)) %>%
#   group_by(area_id, sex) %>%
#   mutate(age_group = "Y015_049") %>%
#   ungroup()
# 
# spectrum_population_change <- spectrum %>%
#   mutate(change = population/population[year==2010],
#          source = "WorldPop") %>%
#   bind_rows(
#     spectrum %>%
#       mutate(change = population/population[year==naomi_pop_year],
#              source = "Naomi")
#   )


cities_areas <- merge_cities %>%
  bind_rows(areas)

pse <- pse %>%
  mutate(row_id = row_number(),
         iso3 = countrycode(country.name, "country.name", "iso3c"))

pse_areas <- pse %>%
  select(iso3, area_name, year, kp, row_id) %>%
  mutate(area_name = str_replace(area_name, "\\,|\\/", "\\;")) %>%
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
  mutate(dist = stringdist::stringdist(value, tolower(area_name))) %>%
  group_by(idx) %>%
  filter(dist == min(dist)) 

best_matches <- min_dist %>%
  filter(n() == 1, dist<3) %>%
  ungroup

level_check <- min_dist %>%
  filter(dist==0, n()>1, !is.na(area_id)) %>%
  count(idx, area_level) %>%
  filter(n() == 1)

if(nrow(level_check)) {
  best_matches <- best_matches %>%
  bind_rows(
    min_dist %>%
      filter(dist==0, n()>1, !is.na(area_id)) %>%
      filter(idx %in% level_check$idx),
    min_dist %>%
      filter(dist==0, n()>1, !is.na(area_id)) %>%
      filter(!idx %in% level_check$idx) %>%
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

pop_search <- best_matches %>%
  ungroup %>%
  filter(is.na(area_id)) %>%
  mutate(city_num  = group_indices(., area_name),
         area_id = paste0(iso3_c, "_city_", city_num)
  )

if(nrow(pop_search)) {
  
  pop_search_unique <- pop_search %>%
    filter(!area_id %in% areas$area_id) %>%
    select(area_id, geometry) %>%
    distinct() %>%
    mutate(source = "WorldPop")
  
  class(pop_search_unique) <- c("sf", "tbl_df", "tbl", "data.frame")
  attr(pop_search_unique, "sf_column") <- "geometry"
  
  pop_search_res <- extract_kp_worldpop(pop_search_unique, iso3_c) 
  
  extrapolate_id <- pop_search_unique %>%
    select(area_id, source) %>%
    bind_rows(
      best_matches %>%
        filter(area_id %in% areas$area_id) %>%
        select(area_id) %>%
        distinct() %>%
        mutate(source = "Naomi")
    ) %>% 
    st_drop_geometry()
  
  pop_search_res <- pop_search_res %>%
    separate(calendar_quarter, remove=FALSE, sep = c(2,6), into=c(NA, "year", NA), convert = TRUE) %>%
    select(area_id, year, sex, age_group, population)
    
  merged_populations <- crossing(area_id = pop_search_res$area_id,
             year = 2000:2020,
             age_group = "Y015_049",
             sex = c("female", "male")) %>%
    left_join(pop_search_res) %>%
    group_by(area_id, sex) %>%
    mutate(population = log(population),
           population = zoo::na.approx(population, na.rm=FALSE),
           population = exp(population)) %>%
    bind_rows(
      population %>%
        select(area_id, year, sex, age_group, population)
    )
  
} else {
  
  extrapolate_id <- best_matches %>%
    filter(area_id %in% areas$area_id) %>%
    select(area_id) %>%
    distinct() %>%
    mutate(source = "Naomi")
  
  merged_populations <- population %>%
    select(area_id, year, sex, age_group, population)
  
}

#### Extrapolate city populations

## Aggregate Naomi populations


# extrapolated_populations <- crossing(extrapolate_id,
#                                      sex = c("female", "male"),
#                                      year = 1970:2025
# ) %>%
#   left_join(spectrum_population_change %>% select(year, sex, source, change)) %>%
#   left_join(merged_populations) %>%
#   mutate(population = population*change) %>%
#   group_by(area_id, sex, year) %>%
#   summarise(population = sum(population)) %>%
#   ungroup

merged_populations <- merged_populations %>%
  bind_rows(
    merged_populations %>%
      group_by(area_id, year, age_group) %>%
      summarise(population = sum(population)) %>%
      mutate(sex = "both") %>%
      ungroup()
  )

row_populations <- pop_search %>%
  bind_rows(best_matches %>% filter(!is.na(area_id))) %>%
  select(row_id, area_id, year, kp) %>%
  mutate(sex = case_when(
    kp %in% c("PWID", "TG") ~ "both",
    kp == "MSM" ~ "male",
    kp %in% c("FSW", "SW") ~ "female"
  )) %>%
  type.convert() %>%
  left_join(merged_populations) %>%
  group_by(row_id) %>%
  summarise(population = sum(population))

pse <- pse %>%
  filter(iso3 == iso3_c) %>%
  left_join(row_populations) %>%
  # filter(!is.na(population)) %>%
  mutate(population_proportion = pse/population) %>%
  select(country.name, surveillance_type, indicator, method, kp, sex, age_group, area_name, province, year, pse_lower, pse, pse_upper, prop_lower, population_proportion, prop_upper, sample, ref) %>%
  arrange(country.name, kp, year)
  

write_csv(pse, "pse_prevalence.csv")
write_csv(bad_match_error, "bad_match_error.csv")
