iso3_c <- iso3

areas <- read_sf("depends/naomi_areas.geojson")%>%
  mutate(iso3 = iso3_c)

# areas <- read_sf("archive/bdi_data_areas/20201024-115335-68b89939/bdi_areas.geojson") %>%
#   mutate(iso3 = iso3_c)
# merge_cities <- read_sf("src/aaa_assign_populations/merge_cities.geojson") %>%
#   filter(iso3 == iso3_c)

merge_cities <- read_sf("merge_cities.geojson") %>%
  filter(iso3 == iso3_c)

sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))

prev_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Analytical datasets/key-populations", "prev.csv")
prev <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = prev_path)
prev <- read_csv(prev)

cities_areas <- merge_cities %>%
  bind_rows(areas)

prev <- prev %>%
  mutate(row_id = row_number())

min_dist <- prev %>%
  select(iso3, area_name, year, kp, row_id) %>%
  filter(!str_detect(area_name, "\\,|\\;|\\&|\\+\\/")) %>%
  mutate(area_name = tolower(area_name)) %>%
  rename(given_area = area_name) %>%
  left_join(cities_areas, by="iso3") %>%
  mutate(dist = stringdist::stringdist(given_area, tolower(area_name))) %>%
  group_by(row_id) %>%
  filter(dist == min(dist))

best_matches <- min_dist %>%
  filter(n() == 1, dist<3) %>%
  ungroup

level_check <- min_dist %>%
  filter(dist==0, n()>1) %>%
  count(row_id, area_level) %>%
  filter(n() == 1)

if(nrow(level_check)) {
  best_matches <- best_matches %>%
    bind_rows(
      min_dist %>%
        filter(dist==0, n()>1) %>%
        filter(row_id %in% level_check$row_id),
      min_dist %>%
        filter(dist==0, n()>1) %>%
        filter(!row_id %in% level_check$row_id) %>%
        filter(area_level == max(area_level))
    ) %>%
    ungroup
  
  warning("\nSeveral identical names matches were found. Check.\n")
  
}

bad_match <- min_dist %>%
  filter(dist>=3)

if (nrow(bad_match)) {
  bad_match_error <- bad_match %>%
    ungroup %>%
    mutate(iso3 = iso3_c) %>%
    select(iso3, given_area, attempted_match = area_name, attempted_area_id = area_id, string_distance = dist)
  
  warning("\nString match is bad:\n", 
          paste0(utils::capture.output(bad_match_error), collapse = "\n"))
  
  
} else {
  bad_match_error <- data.frame(iso3 = iso3_c, x = "No bad matches")
}

write_csv(bad_match_error, "bad_match_error.csv")

assigned_province <- best_matches %>%
  filter(is.na(area_id)) %>%
  st_as_sf %>%
  st_make_valid() %>%
  select(row_id, geometry) %>%
  st_join(areas %>% st_make_valid() %>% mutate(area = as.numeric(st_area(geometry)))) %>%
  filter(area_level == 1) %>%
  filter(area == min(area)) %>%
  select(row_id, area_id) %>%
  st_drop_geometry()

best_matches <- best_matches %>%
  filter(!is.na(area_id)) %>%
  select(row_id, area_id) %>%
  bind_rows(assigned_province)

prev <- prev %>%
  left_join(best_matches) %>%
  select(row_id, kp, year, area_id, prev) %>%
  filter(!is.na(area_id))

write.csv(prev, "prev_assigned_province.csv")