extract_kp_worldpop <- function(areas, iso3, 
                                years = c(2010)) {
  stopifnot(inherits(areas, "sf"))
  stopifnot(grepl("^[A-Z]{3}$", iso3))
  stopifnot(years %in% 2000:2020)
  wp_ages <- c(`15` = "Y015_019", `20` = "Y020_024",
               `25` = "Y025_029", `30` = "Y030_034", `35` = "Y035_039",
               `40` = "Y040_044", `45` = "Y045_049"
               )
  wp_sexes <- c(m = "male", f = "female") 
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
                      sex, source, wt = population, name = "population")
  # pop$asfr <- NA_real_
  pop$age_group <- "Y015_049"
  # validate_naomi_population(pop, areas, unique(areas$area_level))
  pop
}
