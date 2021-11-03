library(INLA)
library(tidyverse)
library(countrycode)
library(sf)

ssa_names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- countrycode(ssa_names, "country.name", "iso3c")

grey <- read_sf("~/Downloads/Longitude_Graticules_and_World_Countries_Boundaries-shp/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp") %>%
  filter(CNTRY_NAME %in% c("Western Sahara", "Mauritania", "Morocco", "Algeria", "Libya", "Tunisia", "Egypt", "Equatorial Guinea", "Somalia", "Djibouti", "Eritrea")) %>%
  bind_rows(read_sf("~/Downloads/sdn_adm_cbs_nic_ssa_20200831_shp/sdn_admbnda_adm0_cbs_nic_ssa_20200831.shp")) %>%
  bind_rows(read_sf("~/Downloads/ssd_admbnda_imwg_nbs_shp/ssd_admbnda_adm0_imwg_nbs_20180817.shp")) %>%
  st_crop(xmin=-180, xmax=180, ymin=-35, ymax=90)

geographies <- read_sf("~/Downloads/Longitude_Graticules_and_World_Countries_Boundaries-shp/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp") %>%
  bind_rows(read_sf("~/Downloads/ssd_admbnda_imwg_nbs_shp/ssd_admbnda_adm0_imwg_nbs_20180817.shp") %>% mutate(CNTRY_NAME = "South Sudan")) %>%
  mutate(iso3 = countrycode(CNTRY_NAME, "country.name", "iso3c"),
         area_name = countrycode(iso3, "iso3c", "country.name")) %>%
  filter(iso3 %in% ssa_iso3)

geographies <- geographies %>%
  arrange(iso3) %>%
  mutate(id.iso3 = as.numeric(factor(iso3))) %>%
  select(iso3, area_name, id.iso3) %>%
  st_make_valid()

# nb <- spdep::poly2nb(geographies)
# spdep::nb2INLA("national_level_adj.adj", nb)

invlogit <- function(x) {exp(x)/(1+exp(x))}
logit <- function(x) {log(x/(1-x))}

region <- read.csv("~/Documents/GitHub/fertility_orderly/global/region.csv") %>%
  mutate(iso3 = toupper(iso3))

iso3_vec <- c("BDI", "BWA", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")

id <- lapply(iso3_vec, function(x){
  orderly::orderly_search(name = "aaa_extrapolate_naomi", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

names(id) <- iso3_vec

prev_dat <- lapply(file.path("archive/aaa_extrapolate_naomi", id[!is.na(id)], "anonymised_prev.csv"),
                   read.csv)



############## ART Coverage ############

art_dat <- lapply(file.path("archive/aaa_extrapolate_naomi", id[!is.na(id)], "anonymised_art.csv"),
                  read.csv)

names(art_dat) <- names(id[!is.na(id)])

art_df <- art_dat %>%
  bind_rows(.id = "iso3") %>%
  left_join(region %>% select(region, iso3)) %>%
  filter(!is.na(denominator), denominator != 0) %>%
  mutate(value = ifelse(value == 1, 0.99, value),
         value = ifelse(value ==0, 0.01, value),
         logit_kp_art = logit(value),
         logit_gen_art = logit(provincial_value),
         logit_gen_art2 = logit_gen_art,
         positive = round(value * denominator),
         negative = round(denominator - positive)
  )


art_res <- lapply(c("FSW", "MSM", "PWID"), function(kp_id) {
  
  art_df <- art_df %>%
    filter(kp == kp_id)
  
  ## Prediction data frames
  df_natural <- data.frame(provincial_value = seq(0.25, 0.99,0.01))
  df_logit <- data.frame(logit_gen_art = logit(seq(0.25, 0.99, 0.01)))
  
  ## Quasibinomial model formula using positives and negatives [same result using the ART coverage and the weight argument in glm()]
  formula <- cbind(positive, negative) ~ logit_gen_art
  mod <- glm(formula, family = "quasibinomial", data = art_df)
  
  ## Predict on both the natural and link [logit] scale
  qb_dat <-  df_natural %>%
    mutate(fit = predict.glm(mod, newdata = df_logit, type = "response"),
           type = "natural") %>%
    bind_rows(df_logit %>% cbind(
      fit = predict.glm(mod, newdata = df_logit, type = "link"),
      type = "logit"
    )) %>%
    mutate(source = "quasibinomial",
           kp = kp_id)
  
  
  ## Logit linear model
  formula <- logit_kp_art ~ logit_gen_art
  mod <- glm(formula, family = "gaussian", data = art_df)
  
  logit_dat <- df_natural %>%
    mutate(fit = invlogit(predict.glm(mod, newdata = df_logit, type = "response")),
           type = "natural") %>%
    bind_rows(df_logit %>% cbind(
      fit = predict.glm(mod, newdata = df_logit, type = "link"),
      type = "logit"
    )) %>%
    mutate(source = "logit linear",
           kp = kp_id)
  
  qb_dat %>%
    bind_rows(logit_dat)
  
  
})

names(art_res) <- c("FSW", "MSM", "PWID")
art_res <- art_res %>%
  bind_rows(.id = "kp")


p1 <- art_res %>%
  filter(type == "natural") %>%
  ggplot(aes(x=provincial_value, y=fit)) +
  geom_point(data = filter(art_df, kp %in% c("FSW", "MSM", "PWID")), aes(y=value, size=denominator), alpha = 0.3) +
  geom_line(aes(color=source), size=1) +
  geom_abline(aes(intercept = 0, slope =1), linetype = 3) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(labels = scales::label_percent()) +
  
  labs(y="KP ART coverage", x = "Genpop ART coverage") +
  facet_wrap(~kp)

p2 <- art_res %>%
  filter(type == "logit") %>%
  ggplot(aes(x=logit_gen_art, y=fit)) +
  geom_point(data = filter(art_df, kp %in% c("FSW", "MSM", "PWID")), aes(x=logit(provincial_value), y=logit(value), size=denominator), alpha = 0.3) +
  geom_line(aes(color=source), size=1) +
  geom_abline(aes(intercept = 0, slope =1), linetype = 3) +
  lims(y=c(-5,5)) +
  labs(y="Logit KP ART coverage", x = "Logit genpop ART coverage") +
  facet_wrap(~kp)

ggpubr::ggarrange(p1, p2, nrow=2, common.legend = TRUE, legend = "bottom")