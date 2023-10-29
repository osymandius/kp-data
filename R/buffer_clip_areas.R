library(terra)

areas <- read_sf("2023_admin1_areas.geojson")

areas <- st_make_valid(areas)

areas_buffer <- st_buffer(areas, 0.02)

areas_buffer %>%
  filter(iso3 == "AGO") %>%
  ggplot() +
    geom_sf(fill = NA)

clip <- st_difference(areas_buffer)

clip %>% 
  filter(iso3 == "AGO") %>%
  ggplot() +
    geom_sf(fill = NA)

collections <- clip %>%
  rowwise() %>%
  mutate(has_collection = list(class(geometry)),
         foo = list(unique(str_detect(has_collection, "GEOMETRYCOLLECTION")))) %>%
  filter(length(foo) > 1)

unified_geog <- collections %>%
  rowwise() %>%
  st_collection_extract("POLYGON") %>%
  group_by(id.area) %>%
  summarise(geometry = st_union(geometry))

polygon <- collections %>% st_drop_geometry() %>% left_join(unified_geog)

fixed_areas <- clip %>%
  filter(!id.area %in% polygon$id.area) %>% bind_rows(polygon) %>% arrange(id.area)

# nb <- areas %>%
#   spdep::poly2nb() %>%
#   `names<-`(areas$id.area)
# 
# nb

nb <- fixed_areas %>%
  spdep::poly2nb() %>%
  `names<-`(fixed_areas$id.area)

nb <- lapply(nb, as.integer)
class(nb) <- "nb"
spdep::nb2INLA("admin1_level_adj.adj", nb)

write_sf(fixed_areas %>% select(-c(foo, has_collection)), "2023_admin1_areas.geojson")

