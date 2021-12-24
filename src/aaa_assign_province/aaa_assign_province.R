iso3_c <- iso3

areas <- read_sf("depends/naomi_areas.geojson")%>%
  mutate(iso3 = iso3_c)

# areas <- read_sf("archive/bdi_data_areas/20201024-115335-68b89939/bdi_areas.geojson") %>%
#   mutate(iso3 = iso3_c)
# merge_cities <- read_sf("src/aaa_assign_populations/merge_cities.geojson") %>%
#   filter(iso3 == iso3_c)

merge_cities <- read_sf("merge_cities.geojson") %>%
  filter(iso3 == iso3_c)

merge_cities <- merge_cities %>%         
  filter(!tolower(area_name) %in% tolower(areas$area_name)) %>%
  st_make_valid()

cities_areas <- merge_cities %>%
  st_join(areas %>% filter(area_level == 1) %>% select(area_id) %>% st_make_valid(), largest=TRUE) %>%
  bind_rows(areas)
  
sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))

prev_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Analytical datasets/key-populations/HIV prevalence", "prev_clean.csv")
prev <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = prev_path)
prev <- read_csv(prev) %>%
  rename(value = prev) %>%
  mutate(iso3 = countrycode(country.name, "country.name", "iso3c"))

art_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Analytical datasets/key-populations/ART coverage", "art_clean.csv")
art <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = art_path)
art <- read_csv(art) %>%
  rename(value = art) %>%
  mutate(iso3 = countrycode(country.name, "country.name", "iso3c")) %>%
  left_join(naomi::get_age_groups() %>% select(age_group_label, age_group)) %>%
  select(-age_group_label)

dat <- list("prev" = prev, "art" = art)

out <- lapply(dat, function(x) {

  # cities_areas <- merge_cities %>%
  #   bind_rows(areas)
  
  # x <- data.frame(
  #   iso3 = "BEN",
  #   area_name = "Alibori, Atacora",
  #   year = 2010,
  #   kp = "FSW"
  # )
  
  x <- x %>%
    mutate(row_id = row_number()) %>%
    filter(iso3 == iso3_c)
  
  # min_dist <- x %>%
  #   select(iso3, area_name, year, kp, row_id) %>%
  #   # filter(!str_detect(area_name, "\\,|\\;|\\&|\\+\\/")) %>%
  #   mutate(area_name = tolower(area_name)) %>%
  #   rename(given_area = area_name) %>%
  #   left_join(cities_areas, by="iso3") %>%
  #   mutate(dist = stringdist::stringdist(given_area, tolower(area_name))) %>%
  #   group_by(row_id) %>%
  #   filter(dist == min(dist))
  
  min_dist <- x %>%
    select(iso3, area_name, year, kp, row_id) %>%
    mutate(area_name = str_replace_all(area_name, "\\,|\\/| and ", "\\;")) %>%
    distinct() %>%
    separate(area_name, sep = ";", into = paste0("area_split", 1:20), remove=FALSE) %>%
    mutate(across(starts_with("area_split"), ~str_trim(.x))) %>%
    pivot_longer(-c(iso3, area_name, year, row_id, kp)) %>%
    filter(!is.na(value)) %>%
    mutate(idx = row_number(),
           value = tolower(value)) %>%
    rename(given_area = area_name) %>%
    left_join(cities_areas, by="iso3") %>%
    mutate(dist = stringdist(value, tolower(area_name))) %>%
    group_by(idx) %>%
    filter(dist == min(dist))
  
  best_matches <- min_dist %>%
    filter(n() == 1, dist<3) %>%
    ungroup
  
  level_check <- min_dist %>%
    filter(dist==0, n()>1, !is.na(area_id)) %>%
    count(row_id, area_id, area_level) %>%
    filter(n() == 1)
  
  if(nrow(level_check)) {
    best_matches <- best_matches %>%
      bind_rows(
        min_dist %>%
          filter(dist==0, n()>1, !is.na(area_id)) %>%
          filter(row_id %in% level_check$row_id),
        min_dist %>%
          filter(dist==0, n()>1, !is.na(area_id)) %>%
          filter(!row_id %in% level_check$row_id) %>%
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
      select(iso3, given_area = value, attempted_match = area_name, attempted_area_id = area_id, string_distance = dist)
    
    warning("\nString match is bad:\n", 
            paste0(utils::capture.output(bad_match_error), collapse = "\n"))
    
    
  } else {
    bad_match_error <- data.frame(iso3 = iso3_c, x = "No bad matches")
  }
  
  
  
  best_matches <- best_matches %>%
    select(row_id, area_id, area_level, geometry) %>%
    mutate(area_level = case_when(
      str_detect(area_id, "_1_") ~ as.integer(1),
      TRUE ~ as.integer(area_level)
    ))
  
  naomi_to_admin1 <- best_matches %>%
    filter(area_level >1) %>%
    st_as_sf() %>%
    st_make_valid() %>%
    select(-area_id) %>%
    st_join(areas %>% filter(area_level == 1) %>% select(area_id) %>% st_make_valid(), largest=TRUE)
  
  assigned_province <- best_matches %>%
    filter(!row_id %in% naomi_to_admin1$row_id) %>%
    bind_rows(naomi_to_admin1) %>%
    select(row_id, area_id)
  
  x <- x %>%
    left_join(assigned_province) %>%
    # select(row_id, kp, year, age_group, area_id, value, denominator, ref) %>%
    filter(!is.na(area_id))
  
  out <- list("assigned_province" = x, "bad_match_error" = bad_match_error)
  
  

})

write_csv(out$prev$bad_match_error, "prev_bad_match_error.csv")
write.csv(out$prev$assigned_province, "prev_assigned_province.csv")

write_csv(out$art$bad_match_error, "art_bad_match_error.csv")
write.csv(out$art$assigned_province, "art_assigned_province.csv")