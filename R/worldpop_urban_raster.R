library(raster)
library(naomi)
library(sf)
library(stars)

urban_raster <- raster::raster("~/Downloads/MOZ_buildings_v1_1/MOZ_buildings_v1_1_urban.tif")

urban_poly <- st_as_stars(urban_raster) %>% 
  st_as_sf(merge = TRUE) %>%
  st_set_crs("WGS84")

urban_poly2 <- urban_poly %>%
  rename(is_urban = MOZ_buildings_v1_1_urban) %>%
  filter(is_urban == 1) %>%
  mutate(idx = row_number()) %>%
  st_as_sf()

st_crs(urban_poly2)

foo <- urban_poly2 %>%
  mutate(buffer_geom = st_buffer(geometry, .003))

parts <- st_cast(st_union(foo$buffer_geom),"POLYGON")
plot(parts)

clust <- unlist(st_intersects(foo$buffer_geom, parts))

diss <- cbind(foo, clust) %>%
  group_by(clust) %>%
  summarise(geometry = st_union(geometry))

areas <- read_sf("archive/moz_data_areas/20210923-150706-07125400/moz_areas.geojson")

p1 <- areas %>%
  filter(parent_area_id %in% c("MOZ_1_12")) %>%
  ggplot() +
    geom_sf() +
    geom_sf(data = filter(diss, clust == 1), color=NA, fill="red") +
    geom_sf(data = urban_extent %>%
              filter(str_detect(area_name, "maputo")),
            fill=NA,
            color="blue",
            size=1
    )

p2 <- areas %>%
  filter(parent_area_id %in% c("MOZ_1_12")) %>%
  mutate(is_mat = ifelse(area_name == "Matola", 1, NA)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = factor(is_mat)), show.legend = FALSE)

ggpubr::ggarrange(p1, p2)

urban_extent <- read_sf("src/aaa_assign_populations/merge_cities.geojson") %>% 
  filter(iso3 == "MOZ") %>%
  mutate(idx = row_number())

joined_areas <- diss %>%
  st_join(urban_extent, largest=TRUE)

test <- urban_extent %>%
  filter(!area_name %in% joined_areas$area_name) %>%
  bind_rows(joined_areas %>% 
              dplyr::select(-clust) %>%
              filter(!is.na(area_name))) %>%
  filter(area_name %in% c("maxixe", "tete"))

test %>%
  filter(area_name == "maxixe") %>%
  ggplot() +
    geom_sf(aes(geometry = geometry), fill="red", alpha = 0.3) +
    geom_sf(data = urban_extent %>%
              filter(area_name == "maxixe"), fill="blue", alpha = 0.3)



  st_drop_geometry() %>%
  group_by(idx) %>%
  filter(n() > 1)

  urban_extent %>%
    filter(area_name == "maxixe") %>%
    ggplot() +
    geom_sf(aes(geometry = geometry))
  