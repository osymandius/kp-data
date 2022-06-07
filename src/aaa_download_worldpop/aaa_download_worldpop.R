iso3_c <- iso3

areas <- read_sf("depends/naomi_areas.geojson")%>%
  mutate(iso3 = iso3) %>%
  st_make_valid()

population <- read_csv("depends/interpolated_population.csv")

cities <- read_sf("merge_cities.geojson") %>%
  filter(iso3 == iso3_c) %>%
  arrange(area_name) %>%
  mutate(area_id = paste0(iso3, "_city_", row_number())) %>%
  st_make_valid()

city_province_map <- cities %>%
  rename(city_name = area_name,
                 city_id = area_id) %>%
  st_join(areas %>% filter(area_level ==1), 
          largest = TRUE) %>%
  st_drop_geometry() %>%
  dplyr::select(area_name = city_name,
         area_id = city_id,
         province = area_name)

write_csv(city_province_map, "city_province_map.csv")

if(nrow(cities)) {

  path <- file.path("https://data.worldpop.org/GIS/Population/Global_2000_2020_Constrained/2020/maxar_v1", iso3, paste0(tolower(iso3), "_ppp_2020_UNadj_constrained.tif"))
  tmpf <- tempfile(fileext = ".tif")
  on.exit(unlink(tmpf, force = TRUE))
  download.file(path, tmpf)
  rast <- raster::raster(tmpf)
  city_population_2020 <- exactextractr::exact_extract(rast, cities, "sum")
  city_population_2020 <- data.frame(area_id = cities$area_id, year = 2020, city_population_2020, stringsAsFactors = FALSE)
  
  age_distribution_match <- cities %>%
    rename(city_area_id = area_id) %>%
    st_join(areas %>% 
              filter(area_level == max(area_level)) %>%
              dplyr::select(area_id), largest = TRUE)
  
  age_sex_distribution <- population %>%
    filter(area_id %in% age_distribution_match$area_id,
           year == 2020) %>%
    group_by(area_id, year, sex) %>%
    mutate(age_distribution = population/sum(population)) %>%
    group_by(area_id, year, age_group) %>%
    mutate(sex_ratio = population[sex=="female"]/population[sex=="male"]) %>%
    dplyr::select(-population)
  
  city_population_2020 <- crossing(city_population_2020,
           sex = c("male", "female"),
           age_group = population$age_group) %>%
    rename(city_area_id = area_id) %>%
    left_join(age_distribution_match %>%
                st_drop_geometry() %>%
                dplyr::select(city_area_id, area_id)) %>%
    left_join(age_sex_distribution) %>%
    mutate(population = ifelse(sex == "female", city_population_2020 * age_distribution * sex_ratio,
                               city_population_2020 * age_distribution * 1/sex_ratio)
           ) %>%
    dplyr::select(city_area_id, year, sex:area_id, population)
  
  city_population <- crossing(city_area_id = city_population_2020$city_area_id,
           year = 2000:2020,
           sex = c("male", "female"),
           age_group = city_population_2020$age_group) %>%
    left_join(city_population_2020 %>% dplyr::select(-year)) %>%
    left_join(population %>%
                group_by(area_id, sex, age_group) %>%
                mutate(population_ratio = population/population[year == 2020]) %>%
                dplyr::select(-population)) %>%
    mutate(population = population * population_ratio) %>%
    dplyr::select(-c(population_ratio, area_id), area_id = city_area_id) %>%
    left_join(cities %>% dplyr::select(area_id, area_name) %>% mutate(area_name = stringr::str_to_sentence(area_name)) %>% st_drop_geometry()) %>%
    dplyr::select(area_id, area_name, everything())
  
  check_city <- city_population %>%
    group_by(area_name, year) %>%
    summarise(population = sum(population))

} else {
  city_population <- data.frame()
}

# population %>%
#   filter(area_id == iso3_c, year > 1999) %>%
#   group_by(year) %>%
#   summarise(population = sum(population)) %>%
#   mutate(area_name = iso3_c) %>%
#   ggplot(aes(x=year, y=log(population), color=area_name)) + 
#     geom_point(size=1) +
#     geom_line(data = check_city)
  

write_csv(city_population, "interpolated_city_population.csv")