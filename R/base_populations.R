grump <- read_sf("src/aaa_download_worldpop/merge_cities.geojson") %>%
  mutate(idx = row_number())

id_input <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_areas_pull", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
}) %>% 
  setNames(ssa_iso3)

areas <- lapply(file.path("archive/aaa_areas_pull", id_input, "naomi_areas.geojson"), sf::read_sf) %>%
  lapply(select, -spectrum_region_code) %>%
  bind_rows() %>%
  separate(area_id, sep=3, remove=FALSE, into=c("iso3", NA))

grump_naomi_match <- grump %>%
  filter(iso3 %in% ssa_iso3) %>%
  st_drop_geometry() %>%
  full_join(areas %>% 
              select(iso3, area_name, area_id, area_level) %>% 
              st_drop_geometry() %>%
              mutate(area_name = tolower(area_name)) %>%
              rename(naomi_area_name = area_name),
            by = "iso3") %>%
  mutate(dist = stringdist::stringdist(area_name, tolower(naomi_area_name))) %>%
  group_by(idx) %>%
  filter(dist == min(dist),
         area_level == max(area_level),
         dist < 2) 

id_worldpop <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_download_worldpop", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
}) %>% 
  setNames(ssa_iso3)

city_population <- lapply(file.path("archive/aaa_download_worldpop", id_worldpop, "interpolated_city_population.csv"), read_csv, show_col_types = FALSE) %>%
  bind_rows()

city_population <- city_population %>%
  moz.utils::five_year_to_15to49("population") %>%
  moz.utils::sex_aggregation("population") %>%
  filter(sex == "both",
         year == 2020) %>%
  mutate(area_name = tolower(area_name))

naomi_populations <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

naomi_populations <- lapply(file.path("archive/aaa_scale_pop", naomi_populations, "interpolated_population.csv"), read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  left_join(areas %>% dplyr::select(area_id, area_name) %>% st_drop_geometry()) %>%
  mutate(area_name = tolower(area_name)) %>%
  moz.utils::five_year_to_15to49("population") %>%
  moz.utils::sex_aggregation("population") %>%
  filter(sex == "both",
         year == 2020) 

grump_naomi <- city_population %>%
  filter(area_name %in% grump_naomi_match$area_name) %>%
  rename(city_population = population) %>%
  left_join(grump_naomi_match %>% select(area_name, naomi_area_id = area_id)) %>%
  left_join(naomi_populations %>% rename(naomi_population = population, naomi_area_id = area_id) %>% select(-area_name)) %>%
  mutate(ratio = city_population/naomi_population) %>%
  separate(area_id, sep=3, remove=FALSE, into=c("iso3", NA))

p1 <- grump_naomi %>%  
  ggplot(aes(x=iso3, y=ratio)) +
    geom_boxplot() +
    lims(y=c(0,5))

p2 <- grump_naomi %>%  
  ggplot(aes(x=iso3, y=ratio)) +
  geom_point(aes(size=naomi_population), show.legend = FALSE) +
  lims(y=c(0,5))

p2

grump_naomi %>% 
  filter(iso3 == "ZWE") %>%
  ggplot(aes(x=naomi_population, y=city_population)) +
  geom_point() +
  geom_abline(aes(intercept=0, slope=1), linetype=3) +
  scale_y_continuous(trans = "log", 
                     breaks = c(30000, 60000, 100000, 250000, 500000, 1000000, 3000000),
                     labels = scales::label_number(scale = 1E-3), limits = c(20000, 3300000)) +
  scale_x_continuous(trans = "log", 
                     breaks = c(30000, 60000, 100000, 250000, 500000, 1000000, 3000000),
                     labels = scales::label_number(scale = 1E-3), limits = c(20000, 3300000)) +
  standard_theme() +
  labs(y="GRUMP population (thousands)", x="Naomi population (thousands)")

grump %>% 
  filter(area_name == "harare") %>%
  ggplot() +
    geom_sf() +
    geom_sf(data = areas %>% filter(area_id == "ZWE_2_3"))

grump %>% 
  filter(area_name == "bulawayo") %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = areas %>% filter(area_id == "ZWE_2_1"))

pse_priority <- read_csv("~/Downloads/pse_estimates_grump_priority.csv") %>%
  mutate(source = "Priority GRUMP") %>%
  bind_rows(
    read_csv("~/Downloads/pse_estimates_naomi_priority.csv") %>%
      mutate(source = "Priority Naomi")
  )

pse_priority %>%
  ggplot(aes(x=fct_rev(iso3), y=median, color=source)) +
    geom_point() +
    facet_wrap(~kp) +
    standard_theme() +
    coord_flip() +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(x=element_blank(), y=element_blank())

## Namibia

setdiff(
  grump %>%
    st_drop_geometry() %>%
    filter(iso3 == "NAM") %>%
    pull(area_name),
  areas %>% 
    st_drop_geometry() %>%
    filter(iso3 == "NAM") %>%
    pull(area_name) %>%
    sort() %>%
    tolower()
)
