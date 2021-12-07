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
  

