library(tidyverse)
library(raster)
library(naomi)
library(sf)
library(stars)
library(rgeos)

urban_raster <- raster::raster("~/Downloads/ZWE_buildings_v1_1/ZWE_buildings_v1_1_urban.tif")

zwe_areas <- read_sf("archive/zwe_data_areas/20201103-094144-94388ee2/zwe_areas.geojson")

urban_poly <- st_as_stars(urban_raster) %>% 
  st_as_sf(merge = TRUE) %>%
  st_set_crs("WGS84")

urban_poly2 <- urban_poly %>%
  st_as_sf() %>%
  st_make_valid() %>%
  rename(is_urban = ZWE_buildings_v1_1_urban) %>%
  filter(is_urban == 1) %>%
  mutate(idx = row_number())

st_is_valid(urban_poly2)

st_crs(urban_poly2)

foo <- urban_poly2 %>%
  mutate(valid_geometry = st_is_valid(geometry)) %>%
  filter(valid_geometry) %>%
  mutate(buffer_geom = st_buffer(geometry, 0.001))

loop_union <- function(df) {
  x <- st_union(df$buffer_geom)
  # x <- st_cast(x, "POLYGON")
}

i <- 0

while(i < 6) {
  
  message(i)
  i <- i+1
  
  test <- foo %>%
    mutate(grouping = round(row_number(), -2)/100) %>%
    group_by(grouping) %>%
    group_split()
  
  union_res <- parallel::mclapply(test, loop_union, mc.cores = 7)
  
  ls <- union_res %>% unlist(recursive = FALSE)
  
  foo <- enframe(ls) %>%
    rename(buffer_geom = value) %>%
    st_as_sf()
  
}

st_crs(foo) <- st_crs(zwe_areas)

foo %>%
  filter(name < 100) %>%
  ggplot() +
    geom_sf(data = zwe_areas %>% filter(area_level == 1)) +
    geom_sf()

message_parallel <- function(...){
  system(sprintf('echo "\n%s\n"', paste0(..., collapse="")))
}

combine_res <- st_combine(test)


maptools_res <- st_as_sf(
  rgeos::gUnaryUnion(as_Spatial(test))
  )

parts <- st_cast(parts, "POLYGON")

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
  
  #############
  
  library(tidyverse)
  library(raster)
  library(naomi)
  library(sf)
  library(stars)
  library(rgeos)
  
  urban_raster <- raster::raster("~/Downloads/UGA_buildings_v2_0/UGA_buildings_v2_0_urban.tif")
  
  uga_areas <- read_sf("archive/uga_data_areas/20220715-160710-1284bc60/uga_areas.geojson")
  
  urban_poly <- st_as_stars(urban_raster) %>% 
    st_as_sf(merge = TRUE) %>%
    st_set_crs("WGS84")
  
  urban_poly <- urban_poly %>%
    mutate(idx = row_number()) %>%
    rename(is_urban = UGA_buildings_v2_0_urban)
  
  foo <- rgeos::gUnaryUnion(as_Spatial(urban_poly %>% filter(is_urban == 1)), id = urban_poly$idx)
  
  foo <- urban_poly %>% 
    filter(is_urban == 1) %>%
    geos::as_geos_geometry() %>% 
    geos::geos_make_collection() %>% 
    geos::geos_unary_union() %>%
    st_as_sf()
  
  path <- "https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/maxar_v1/UGA/uga_ppp_2020_UNadj_constrained.tif"
  tmpf <- tempfile(fileext = ".tif")
  on.exit(unlink(tmpf, force = TRUE))
  download.file(path, tmpf)
  rast <- raster::raster(tmpf)
  city_population_2020 <- exactextractr::exact_extract(rast, foo, "sum")
  city_population_2020 <- data.frame(area_id = cities$area_id, year = 2020, city_population_2020, stringsAsFactors = FALSE)
  
  
  urban_poly2 <- urban_poly %>%
    st_as_sf() %>%
    st_make_valid() %>%
    rename(is_urban = UGA_buildings_v2_0_urban) %>%
    filter(is_urban == 1) %>%
    mutate(idx = row_number())
  
  unique(st_is_valid(urban_poly2))
  
  st_crs(urban_poly2)
  
  foo <- urban_poly2 %>%
    mutate(valid_geometry = st_is_valid(geometry)) %>%
    filter(valid_geometry) %>%
    mutate(buffer_geom = st_buffer(geometry, 0.001))
  
  single_geom <- urban_poly %>%
    rename(is_urban = UGA_buildings_v2_0_urban) %>%
    group_by(is_urban) %>%
    summarise(geometry = st_union(geometry))
  
  loop_union <- function(df) {
    x <- st_union(df$buffer_geom)
    # x <- st_cast(x, "POLYGON")
  }
  
  i <- 0
  
  while(i < 6) {
    
    message(i)
    i <- i+1
    
    test <- foo %>%
      mutate(grouping = round(row_number(), -2)/100) %>%
      group_by(grouping) %>%
      group_split()
    
    union_res <- parallel::mclapply(test, loop_union, mc.cores = 7)
    
    ls <- union_res %>% unlist(recursive = FALSE)
    
    foo <- enframe(ls) %>%
      rename(buffer_geom = value) %>%
      st_as_sf()
    
  }
  
  st_crs(foo) <- st_crs(zwe_areas)
  
  foo %>%
    filter(name < 100) %>%
    ggplot() +
    geom_sf(data = zwe_areas %>% filter(area_level == 1)) +
    geom_sf()
  
  message_parallel <- function(...){
    system(sprintf('echo "\n%s\n"', paste0(..., collapse="")))
  }
  
  combine_res <- st_combine(test)
  
  
  maptools_res <- st_as_sf(
    rgeos::gUnaryUnion(as_Spatial(test))
  )
  
  parts <- st_cast(parts, "POLYGON")
  
  parts <- st_cast(st_union(foo$buffer_geom),"POLYGON")
  plot(parts)
  
  clust <- unlist(st_intersects(foo$buffer_geom, parts))
  
  diss <- cbind(foo, clust) %>%
    group_by(clust) %>%
    summarise(geometry = st_union(geometry))
  