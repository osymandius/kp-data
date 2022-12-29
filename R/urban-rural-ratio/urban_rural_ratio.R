library(raster)
library(naomi)
library(sf)
library(stars)

ken_population <- read_csv("archive/aaa_inputs_orderly_pull/20211012-193910-be870bed/naomi_population.csv")
ken_areas <- read_sf("archive/aaa_inputs_orderly_pull/20211012-193910-be870bed/naomi_areas.geojson")

sampled_district_populations <- ken_population %>%
  filter(
    str_detect(area_id, "_2_"),
    area_name %in% c("Kiambu", "Kitui", "Laikipia", "Machakos", "Mombasa")) %>%
  moz.utils::five_year_to_15to49("population")

## 3S-CRC from KEN 2020 PSE NASCOP report

fsw_pse <- data.frame(area_name = c("Kiambu", "Kitui", "Laikipia", "Machakos", "Mombasa"),
                      restype = c("urban", "rural", "rural", "urban", "urban"),
                      kp = "FSW",
           sex = "female",
           pse_lower = c(4194, 1230, 1165, 2844, 3912),
           pse = c(4726, 1322, 1205, 3141, 4219),
           pse_upper = c(5384, 1435, 1257, 3505, 4580)
           )

msm_pse <- data.frame(area_name = c("Kiambu", "Kitui", "Laikipia", "Machakos", "Mombasa"),
                      restype = c("urban", "rural", "rural", "urban", "urban"),
                      kp = "MSM",
                      sex = "male",
                      pse_lower = c(1089, 474, 590, 1789, 1580),
                      pse = c(1204, 474, 646, 1973, 1681),
                      pse_upper = c(1350, 474, 723, 2199, 1805)
)

pse <- fsw_pse %>% bind_rows(msm_pse)

##

mwi_population <- read_csv("archive/mwi_data_population/20210214-230158-41c43727/mwi_population_projections18.csv") %>%
  filter(str_detect(area_id, "_5_"),
         calendar_quarter == "CY2013Q2",
         sex == "female") %>%
  moz.utils::five_year_to_15to49("population")

mwi_pse <- data.frame(area_name = c("Karonga", "Mzuzu City",
                                    "Kasungu", "Lilongwe City",
                                    "Mchinji", "Dedza",
                                    "Mangochi", "Blantyre City",
                                    "Thyolo", "Mulanje",
                                    "Mwanza", "Nkhotakota",
                                    "Nsanje", "Dowa"),
                      restype = c("border", "city", "commercial_farm", "city", "border",
                                  "border", "tourist", "city", "commercial_farm", "commercial_farm",
                                  "border", "commercial_farm", "rural", "rural"),
                      kp = "FSW",
                      sex = "female",
                      pse_upper = c(289, 357, 567, 1830, 400, 112, 213, 1197, 591, 1735, 280, 340, 173, 806),
                      pse = c(249, 317, 536, 1773, 358, 89, 213, 1130, 554, 965, 203, 297, 153, 592),
                      pse_lower = c(210, 277, 506, 1716, 317, 67, 213, 1062, 517, 195, 127, 255, 133, 377)
) %>% left_join(mwi_population %>% select(area_name, population)) %>%
  mutate(pse_proportion = pse/population,
         pse_proportion_lower = pse_lower/population,
         pse_proportion_upper = pse_upper/population)
  
mwi_pse %>%  
  ggplot(aes(x=area_name, y=pse_proportion, fill=restype)) +
  geom_col(position = position_dodge()) +
  geom_linerange(aes(ymin = pse_proportion_lower, ymax = pse_proportion_upper)) +
  scale_y_continuous(labels = scales::label_percent()) +
  # scale_fill_manual(values =wesanderson::wes_palette("Zissou1")[c(1,4)]) +
  moz.utils::standard_theme() +
  labs(x=element_blank(), y="PSE proportion", fill = element_blank()) +
  facet_wrap(~kp)

### Zimbabwe PSE from Fearon et al. 2020

zwe_pse <- read_csv("R/urban-rural-ratio/zwe_urban_rural.csv") %>%
  mutate(site = ifelse(site == "Vic Falls", "Victoria Falls", site))
zwe_denom <- readxl::read_excel("R/urban-rural-ratio/census_denominators_zim_fsw_sites_28sep17.xlsx") %>%
  left_join(zwe_pse %>% select(site, year))
  

zwe_pse %>%
  mutate(site = ifelse(site == "Vic Falls", "Victoria Falls", site)) %>%
  left_join(zwe_denom %>% select(site, pop = `females_15-49`)) %>%
  pivot_longer(-c(site:year, pop)) %>%
  filter(!is.na(value)) %>%
  mutate(pse_prop = value/pop) %>%
  separate(name, into=c("method", "col"), sep="-") %>%
  pivot_wider(names_from = col, values_from = c(value, pse_prop)) %>%
  ggplot(aes(x=site, y=pse_prop_pse, fill = method)) +
    geom_col(position = position_dodge2(.9, preserve = "single")) +
    geom_linerange(aes(ymin = pse_prop_lower, ymax=pse_prop_upper), position = position_dodge2(.9, preserve = "single")) +
    facet_wrap(~type, scales = "free_x") +
    standard_theme() +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(x=element_blank(), title="Fearon et al | FSW") +
    theme(axis.text.x = element_text(angle = 25, hjust=1))

zwe_matched_pops <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final_sourced.csv")
zwe_matched_pops_grump_priority <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final_sourced.csv")
zwe_matched_pops_naomi_priority <- read_csv("~/Downloads/pse_final_sourced_naomi_priority.csv")
pse_grump_priority <- read_csv("~/Downloads/pse_estimates_grump_priority.csv")
pse_naomi_priority <- read_csv("~/Downloads/pse_estimates_naomi_priority.csv")
 
pop_df <- zwe_matched_pops %>%
  filter(ref == "Fearon et al.") %>%
  distinct(area_name, year, population) %>%
  mutate(source = "WorldPop | GRUMP priority") %>%
  bind_rows(
    zwe_matched_pops_naomi_priority %>%
      filter(ref == "Fearon et al.") %>%
      distinct(area_name, year, population) %>%
      mutate(source = "WorldPop | Naomi priority"),
    zwe_denom %>% 
              select(area_name = site, population = `females_15-49`, year) %>%
              mutate(source = "Fearon | Census wards") %>%
              filter(area_name %in% zwe_pse$site)) %>%
  left_join(zwe_pse %>%
              rename(area_name = site) %>%
              select(area_name, type)) %>%
  filter(!area_name %in% c("Chirundu", "Nkayi"))

pop_df %>%
  ggplot(aes(x=area_name, y=population, fill=source)) +
    geom_col(position = position_dodge2(.9, preserve = "single")) +
    scale_y_log10(labels = scales::label_number(), breaks = c(10, 100, 500, 1000, 2000, 5000, 20000, 50000, 200000, 500000, 1000000)) +
    labs(x=element_blank(), y="Population (thousands)") +
    facet_grid(~type, scales = "free_x", space = "free") +
    standard_theme() +
    theme(axis.text.x = element_text(angle = 25, hjust=1))

pse_prop_comparison <- zwe_pse %>%
  rename(area_name = site) %>%
  pivot_longer(-c(area_name:year)) %>%
  filter(!is.na(value)) %>%
  separate(name, into=c("method", "col"), sep="-") %>%
  left_join(pop_df) %>%
  mutate(pse_prop = value/population) %>%
  pivot_wider(names_from = col, values_from = c(value, pse_prop)) %>%
  mutate(method = recode(method,
                        "crc" = "2S-CRC",
                        "service" = "Service multiplier",
                        "object" = "Object/event multiplier")
  ) %>%
  select(area_name, type, method, source, pse_prop_pse)
    
p1 <- pse_prop_comparison %>%
  filter(method == "2S-CRC") %>%
  ggplot(aes(x=area_name, y=pse_prop_pse, fill=source)) +
  geom_col(position = position_dodge2(.9, preserve = "single")) +
    facet_grid(~type, scales = "free_x", space = "free") +
    standard_theme() +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(x=element_blank(), y="PSE proportion", title = "2S-CRC") +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))

p2 <- pse_prop_comparison %>%
  filter(method == "Object/event multiplier") %>%
  ggplot(aes(x=area_name, y=pse_prop_pse, fill=source)) +
  geom_col(position = position_dodge2(.9, preserve = "single")) +
  facet_grid(~type, scales = "free_x", space = "free") +
  standard_theme() +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x=element_blank(), y="PSE proportion", title = "Object/event multiplier") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

p3 <- pse_prop_comparison %>%
  filter(method == "Service multiplier") %>%
  ggplot(aes(x=area_name, y=pse_prop_pse, fill=source)) +
  geom_col(position = position_dodge2(.9, preserve = "single")) +
  facet_grid(~type, scales = "free_x", space = "free") +
  standard_theme() +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x=element_blank(), y=element_blank(), title = "Service multiplier") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "bottom")

p1 / p2 | p3 / plot_spacer()

zwe_matched_pops %>%
  filter(ref == "Fearon et al.") %>%
  select(area_name, method, pse_prop_pse = population_proportion) %>%
  mutate(source = "Stevens") %>%
  left_join(zwe_pse %>% select(area_name = site, type)) %>%
  ggplot(aes(x=area_name, y=pse_prop_pse, fill=method)) +
    geom_col(position = position_dodge2(.9, preserve = "single")) +
    facet_wrap(~type, scales = "free_x") +
    scale_y_continuous(labels = scales::label_percent()) +
    standard_theme() +
    theme(axis.text.x = element_blank()) +
    labs(x="Sites", y=element_blank())

b1 <- zwe_matched_pops %>%
  filter(ref == "Fearon et al.") %>%
  select(area_name, method, pse_prop_pse = population_proportion) %>%
  mutate(source = "Stevens") %>%
  left_join(zwe_pse %>% select(area_name = site, type)) %>%
  ggplot(aes(x=type, y=pse_prop_pse)) +
    geom_boxplot() +
    standard_theme() +
    labs(x=element_blank(), y="PSE proportion") +
    scale_y_continuous(labels = scales::label_percent())

b2 <- zwe_matched_pops %>%
  filter(ref == "Fearon et al.") %>%
  select(area_name, method, pse_prop_pse = population_proportion) %>%
  mutate(source = "Stevens") %>%
  left_join(zwe_pse %>% select(area_name = site, type)) %>%
  ggplot(aes(x=type, y=pse_prop_pse, color=method)) +
  geom_boxplot() +
  standard_theme() +
  labs(x=element_blank(), y="PSE proportion") +
  theme(legend.position = "right") +
  scale_y_continuous(labels = scales::label_percent())
  
b1 + b2

zwe_pse <- zwe_pse %>%
  filter(!is.na(lat)) %>%
  sf::st_as_sf(coords = c("lat", "long"))

zwe_pse <- zwe_pse %>%
  mutate(lat = unlist(map(zwe_pse$geometry,1)),
         long = unlist(map(zwe_pse$geometry,2)))

zwe_ward_areas <- sf::read_sf("~/Downloads/zwe_admbnda_adm3_zimstat_ocha_20180911/zwe_admbnda_adm3_zimstat_ocha_20180911.shp")

st_crs(zwe_pse) <- st_crs(zwe_ward_areas)

zwe_ward_areas %>%
  ggplot() +
    geom_sf() +
    geom_point(data = zwe_pse, aes(x=long, y=lat))

zwe_pse %>%
  select(site, geometry) %>%
  st_join(zwe_ward_areas)

zwe_areas <- sf::read_sf("archive/zwe_data_areas/20201103-094144-94388ee2/zwe_areas.geojson")

orderly::orderly_search(name = "aaa_download_worldpop", query = paste0('latest(parameter:iso3 == "', "ZWE", '")'), draft = FALSE)

zwe_urban_pop <- read_csv("archive/aaa_download_worldpop/20211014-161041-fc18c44a/interpolated_city_population.csv") %>%
  moz.utils::five_year_to_15to49("population") %>%
  filter(sex == "female") %>%
  mutate(area_name = ifelse(area_name == "Victoria falls", "Vic Falls", area_name))

zwe_pop <- read_csv("archive/zwe_data_population/20201130-162514-ea8350d2/zwe_population_nso.csv") %>%
  moz.utils::five_year_to_15to49("population") %>%
  filter(sex == "female") %>%
  separate(calendar_quarter, into = c(NA, "year", NA), sep = c(2,6)) %>%
  type_convert()

zwe_pse <- zwe_pse %>%
  pivot_longer(-c(site:year), names_sep = "\\.", names_to = c("method", "bound")) %>%
  pivot_wider(names_from = bound, values_from = value) %>%
  filter(!is.na(pse))

zwe_pse %>%
  rename(area_name = site) %>%
  left_join(zwe_urban_pop %>% ungroup %>% select(area_name, year, population)) %>%
  mutate(pse_proportion = pse/population,
         pse_proportion_lower = lower/population,
         pse_proportion_upper = upper/population) %>%
  ggplot(aes(x=area_name, y=pse_proportion, fill = method)) +
    geom_col(position = position_dodge()) +
    facet_wrap(~type, scales = "free_x")

pse %>%
  left_join(sampled_district_populations) %>%
  arrange(restype) %>%
  mutate(pse_proportion = pse/population,
         pse_proportion_lower = pse_lower/population,
         pse_proportion_upper = pse_upper/population,
         area_name = fct_inorder(area_name)) %>%
  dplyr::select(area_id, area_name, restype, kp, pse_proportion, pse_proportion_lower, pse_proportion_upper) %>%
  ggplot(aes(x=area_name, y=pse_proportion, fill=restype)) +
    geom_col(position = position_dodge()) +
    geom_linerange(aes(ymin = pse_proportion_lower, ymax = pse_proportion_upper)) +
    scale_y_continuous(labels = scales::label_percent()) +
    scale_fill_manual(values =wesanderson::wes_palette("Zissou1")[c(1,4)]) +
    moz.utils::standard_theme() +
    labs(x=element_blank(), y="PSE proportion", fill = element_blank()) +
    facet_wrap(~kp)
  

  