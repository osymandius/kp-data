library(tidyverse)
library(sf)
library(orderly)
library(orderly.sharepoint)
library(naomi)
library(naomi.utils)
library(countrycode)

ssa_names <- c("Angola", "Botswana", "Comoros", "Eritrea", "Eswatini", "Ethiopia", "Kenya", "Lesotho", "Madagascar", "Malawi", "Mauritius", "Mozambique", "Namibia", "Rwanda", "Seychelles", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Mauritania", "Niger", "Nigeria", "Sao Tome and Principe", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- countrycode(ssa_names, "country.name", "iso3c")

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

write_sf(merge_cities, "src/aaa_assign_populations/merge_cities.geojson")
