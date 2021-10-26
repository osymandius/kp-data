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

get_mod_results_test <- function(mod, inla_df, var) {
  
  
  df <- inla_df %>%
    filter(across(all_of(var), ~!is.na(.x)))
  
  print("Sampling..")
  samples <- inla.posterior.sample(1000, mod)
  print("Done sampling")
  contents = mod$misc$configs$contents
  effect = "Predictor"
  id.effect = which(contents$tag==effect)
  ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])
  
  ind.effect <- 1:(nrow(inla_df) - nrow(df))
  
  samples.effect = lapply(samples, function(x) x$latent[ind.effect])
  
  ident <- inla_df[ind.effect, ]
  
  qtls <- apply(sapply(samples.effect, cbind), 1, quantile, c(0.025, 0.5, 0.975))
  
  samples_ident <- ident %>%
    mutate(lower = qtls[1,],
           median = qtls[2,],
           upper = qtls[3,]
    )
  
  return(samples_ident)
  
}

region <- read.csv("~/Documents/GitHub/fertility_orderly/global/region.csv") %>%
  mutate(iso3 = toupper(iso3))

iso3_vec <- c("BDI", "BWA", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")

id <- lapply(iso3_vec, function(x){
  orderly::orderly_search(name = "aaa_extrapolate_naomi", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

names(id) <- iso3_vec

prev_dat <- lapply(file.path("archive/aaa_extrapolate_naomi", id[!is.na(id)], "anonymised_prev.csv"),
                   read.csv)

names(prev_dat) <- names(id[!is.na(id)])

prev_df <- prev_dat %>%
  bind_rows(.id = "iso3") %>%
  left_join(region %>% select(region, iso3)) %>%
  filter(value != 0) %>%
  select(iso3, region, year, kp, value, provincial_value) %>%
  group_by(iso3, year, kp) %>%
  mutate(study_idx = cur_group_id()) %>%
  ungroup %>%
  mutate(
         log_kp_prev = log(value),
         log_gen_prev = log(provincial_value),
         log_gen_prev2 = log_gen_prev,
         id.region = group_indices(., region)
  )

formula <- log_kp_prev ~ log_gen_prev + region

prev_inla <- crossing(log_gen_prev = log(seq(0.01, 0.4, 0.01)),
                      region = c("WCA", "ESA")) %>%
  bind_rows(prev_df %>%
              filter(kp == "FSW") %>%
              ungroup) %>%
  mutate(idx = row_number())

fit <- INLA::inla(formula,
                         data = prev_inla,
                         family = "gaussian", 
                         control.compute = list(config = TRUE, waic = TRUE),
                  control.predictor=list(compute=TRUE),
                  verbose = TRUE)

#' Without region fixed effect
#' Watanabe-Akaike information criterion (WAIC) ...: 768.41
#' Effective number of parameters .................: 3.23

#' With region fixed effect
#' Watanabe-Akaike information criterion (WAIC) ...: 749.98
#' Effective number of parameters .................: 4.31

fitted_val <- get_mod_results_test(fit, prev_inla, "iso3")

fsw_prev_out <- fitted_val %>%
  mutate(gen_prev = exp(log_gen_prev))

#####

formula <- log_kp_prev ~ log_gen_prev

prev_inla <- crossing(log_gen_prev = log(seq(0.01, 0.4, 0.01))) %>%
  bind_rows(prev_df %>%
              filter(kp == "MSM") %>%
              ungroup) %>%
  mutate(idx = row_number())

fit <- INLA::inla(formula,
                  data = prev_inla,
                  family = "gaussian", 
                  control.compute = list(config = TRUE, waic = TRUE),
                  control.predictor=list(compute=TRUE),
                  verbose = TRUE)

#' With region fixed effect
#' Watanabe-Akaike information criterion (WAIC) ...: 849.31
#' Effective number of parameters .................: 3.29

fitted_val <- get_mod_results_test(fit, prev_inla, "iso3")

msm_prev_out <- fitted_val %>%
  mutate(gen_prev = exp(log_gen_prev))

######

prev_inla <- crossing(log_gen_prev = log(seq(0.01, 0.4, 0.01))) %>%
  bind_rows(prev_df %>%
              filter(kp == "PWID") %>%
              ungroup) %>%
  mutate(idx = row_number())

fit <- INLA::inla(formula,
                  data = prev_inla,
                  family = "gaussian", 
                  control.compute = list(config = TRUE, waic = TRUE),
                  control.predictor=list(compute=TRUE),
                  verbose = TRUE)

#' With region fixed effect
#' Watanabe-Akaike information criterion (WAIC) ...: 205.16
#' Effective number of parameters .................: 4.08

fitted_val <- get_mod_results_test(fit, prev_inla, "iso3")

pwid_prev_out <- fitted_val %>%
  mutate(gen_prev = exp(log_gen_prev))

prev_out <- bind_rows(
    msm_prev_out %>% mutate(kp = "MSM"),
    pwid_prev_out %>% mutate(kp = "PWID")
  )

p1 <- prev_df %>%
  filter(!is.na(iso3), kp %in% c("FSW")) %>%
  ggplot(aes(x=log_gen_prev, y=log_kp_prev)) +
  geom_point(aes(color=region), alpha = 0.3) +
  geom_line(data = fsw_prev_out %>% mutate(kp = "FSW"), aes(x=log_gen_prev, y=median, color=region), size=1) +
  geom_ribbon(data = fsw_prev_out %>% mutate(kp = "FSW"), aes(x=log_gen_prev, ymin = lower, ymax=upper, fill=region), alpha=0.3) + 
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  moz.utils::standard_theme() +
  scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
  labs(y = "Log KP HIV prevalence", x = element_blank())+
  theme(panel.border = element_rect(fill=NA, color="black"),
        legend.position = "none") +
  facet_wrap(~kp, ncol=1)
# lims(x=c(0,1), y=c(0,1))

p2 <- prev_df %>%
  filter(!is.na(iso3), kp %in% c("FSW")) %>%
  ggplot(aes(x=provincial_value, y=value, group=region)) +
  geom_point(aes(color=region), alpha = 0.3) +
  geom_line(data = fsw_prev_out %>% mutate(kp = "FSW"), aes(x=gen_prev, y=exp(median), color=region), size=1) +
  geom_ribbon(data = fsw_prev_out %>% mutate(kp = "FSW"), aes(x=gen_prev, ymin = exp(lower), ymax=exp(upper), fill=region), alpha=0.3) + 
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  moz.utils::standard_theme() +
  scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
  labs(y = "KP HIV prevalence", x = element_blank())+
  theme(panel.border = element_rect(fill=NA, color="black"),
        legend.position = "none") +
  facet_wrap(~kp, ncol=1)

p3 <- prev_df %>%
  filter(!is.na(iso3), kp %in% c("MSM", "PWID")) %>%
  ggplot(aes(x=log_gen_prev, y=log_kp_prev)) +
    geom_point(alpha = 0.3) +
    geom_line(data = prev_out %>% mutate(region = "SSA"), aes(color=region, x=log_gen_prev, y=median), size=1) +
    geom_ribbon(data = prev_out %>% mutate(region = "SSA"), aes(x=log_gen_prev, ymin = lower, ymax=upper), alpha=0.3) + 
    geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
    moz.utils::standard_theme() +
  scale_color_manual(values = "black") +
  # scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
    labs(y = "Log KP HIV prevalence", x = "Log general population HIV prevalence") +
    theme(panel.border = element_rect(fill=NA, color="black"),
          legend.position = "none") +
    facet_wrap(~kp, ncol=1)
    # lims(x=c(0,1), y=c(0,1))

p4 <- prev_df %>%
  filter(!is.na(iso3), kp %in% c("MSM", "PWID")) %>%
  ggplot(aes(x=provincial_value, y=value)) +
  geom_point(alpha = 0.3) +
  geom_line(data = prev_out %>% mutate(region = "SSA"), aes(color=region, x=gen_prev, y=exp(median)), size=1) +
  geom_ribbon(data = prev_out %>% mutate(region = "SSA"), aes(x=gen_prev, ymin = exp(lower), ymax=exp(upper)), alpha=0.3) + 
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  moz.utils::standard_theme() +
  scale_color_manual(values = "black") +
  # scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
  labs(y = "KP HIV prevalence", x = "General population HIV prevalence") +
  theme(panel.border = element_rect(fill=NA, color="black"),
        legend.position = "none") +
  facet_wrap(~kp, ncol=1)

leg <- ggpubr::get_legend(data.frame(x=1, y=1, region = c("SSA", "WCA", "ESA")) %>%
  ggplot(aes(x=x, y=y,color=region)) +
    geom_line(size=1) +
  scale_color_manual(values = c(wesanderson::wes_palette("Zissou1")[c(1)], "black", wesanderson::wes_palette("Zissou1")[c(4)]))+
  moz.utils::standard_theme() +
    labs(color = "Region")
)

prev_plot_a <- ggpubr::ggarrange(p1, p2, nrow=1)
prev_plot_b <- ggpubr::ggarrange(p3, p4, nrow=1)
ggpubr::ggarrange(prev_plot_a, prev_plot_b, ncol=1, heights = c(1.1,2), legend.grob = leg, legend = "bottom")

mod <- lm(log_kp_prev ~ log_gen_prev, data = prev_inla %>% filter(!is.na(iso3)))
summary(mod)

mod.res = resid(mod) 
# formula <- log_kp_prev ~ log_gen_prev + f(study_idx, model = "iid")
# 
# fit_re <- INLA::inla(formula,
#                   data = prev_inla,
#                   family = "gaussian", 
#                   control.compute = list(config = TRUE),
#                   control.predictor=list(compute=TRUE),
#                   verbose = TRUE)
# 
# fitted_val <- get_mod_results_test(fit_re, prev_inla)
# 
# fitted_val <- fitted_val %>%
#   mutate(gen_prev = exp(log_gen_prev))
# 
# p3 <- prev_inla %>%
#   filter(!is.na(iso3)) %>%
#   ggplot(aes(x=log_gen_prev, y=log_kp_prev)) +
#   geom_line(data = fitted_val, aes(x=log_gen_prev, y=median)) +
#   geom_ribbon(data = fitted_val, aes(x=log_gen_prev, ymin = lower, ymax=upper), alpha=0.3) + 
#   geom_point() +
#   labs(title = "PWID - log + re")
# # lims(x=c(0,1), y=c(0,1))
# 
# p4 <- prev_inla %>%
#   filter(!is.na(iso3)) %>%
#   ggplot(aes(x=provincial_value, y=value)) +
#   geom_line(data = fitted_val, aes(x=gen_prev, y=exp(median))) +
#   geom_ribbon(data = fitted_val, aes(x=gen_prev, ymin = exp(lower), ymax=exp(upper)), alpha=0.3) + 
#   geom_point() +
#   lims(x=c(0,1), y=c(0,1)) +
#   labs(title = "PWID - natural + re")
# 
# ggpubr::ggarrange(p1, p2, p3, p4, nrow=2, ncol=2)

############## PSE
# 
iso3_vec <- c("BDI", "BWA", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")
# 
id <- lapply(iso3_vec, function(x){
  orderly::orderly_search(name = "aaa_assign_populations", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

names(id) <- iso3_vec

pse_dat <- lapply(file.path("archive/aaa_assign_populations/", id[!is.na(id)], "pse_prevalence.csv"),
                   read.csv)
names(pse_dat) <- names(id[!is.na(id)])

# iso3_vec <- c("BDI", "BWA", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")
# 
# id_list <- list.files("draft/aaa_assign_populations/")
# 
# pse_dat <- lapply(file.path("draft/aaa_assign_populations", id_list, "pse_prevalence.csv"),
#                   read.csv)
# 
# names(pse_dat) <- iso3_vec

pse_dat <- pse_dat %>%
  bind_rows(.id = "iso3") %>%
  bind_rows(read.csv("R/ZAF/assign_populations/pse_prevalence.csv") %>% mutate(iso3 = "ZAF")) %>%
  left_join(region %>% select(region, iso3)) %>%
  select(iso3, year, kp, population_proportion) %>%
  filter(population_proportion != 0, !is.na(population_proportion), population_proportion < 1) %>%
  distinct() %>%
  group_by(iso3, year, kp) %>%
  mutate(study_idx = cur_group_id()) %>%
  ungroup %>%
  mutate(
    logit_proportion = log(population_proportion/(1-population_proportion))
  )

# area_iso3 <- unique(pse_dat$iso3)
# 
# area_id <- lapply(ssa_iso3, function(x){
#   orderly::orderly_search(name = "aaa_inputs_orderly_pull", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
# })
# 
# areas <- lapply(file.path("archive/aaa_inputs_orderly_pull", area_id, "naomi_areas.geojson"),
#                   sf::read_sf) %>%
#   bind_rows() %>%
#   filter(area_level == 0) %>%
#   mutate(iso3 = area_iso3)
# 
# 

pse_inla <- crossing(iso3 = ssa_iso3) %>%
  bind_rows(pse_dat %>%
              filter(kp == "FSW") %>%
              ungroup) %>%
  left_join(geographies %>% st_drop_geometry()) %>%
  select(iso3, logit_proportion, id.iso3)

pse_formula <- logit_proportion ~ f(id.iso3, model = "besag", scale.model = TRUE, graph = "national_level_adj.adj")

pse_fit <- INLA::inla(pse_formula,
                  data = pse_inla,
                  family = "gaussian", 
                  control.compute = list(config = TRUE),
                  control.predictor=list(compute=TRUE),
                  verbose = TRUE)

fitted_val <- get_mod_results_test(pse_fit, pse_inla, "logit_proportion")

invlogit <- function(x) {exp(x)/(1+exp(x))}

fsw_val <- fitted_val %>%
  mutate(across(c(lower, median, upper), invlogit))

######################

pse_inla <- crossing(iso3 = ssa_iso3) %>%
  bind_rows(pse_dat %>%
              filter(kp == "MSM") %>%
              ungroup) %>%
  mutate(idx = row_number(),
         id.iso3 = as.numeric(factor(iso3))) %>%
  select(iso3, logit_proportion, id.iso3)
 
pse_fit <- INLA::inla(pse_formula,
                      data = pse_inla,
                      family = "gaussian", 
                      control.compute = list(config = TRUE),
                      control.predictor=list(compute=TRUE),
                      verbose = TRUE)

fitted_val <- get_mod_results_test(pse_fit, pse_inla, "logit_proportion")

invlogit <- function(x) {exp(x)/(1+exp(x))}

msm_val <- fitted_val %>%
  mutate(across(c(lower, median, upper), invlogit))

######################

pse_inla <- crossing(iso3 = ssa_iso3) %>%
  bind_rows(pse_dat %>%
              filter(kp == "PWID") %>%
              ungroup) %>%
  mutate(idx = row_number(),
         id.iso3 = as.numeric(factor(iso3))) %>%
  select(iso3, logit_proportion, id.iso3)

pse_fit <- INLA::inla(pse_formula,
                      data = pse_inla,
                      family = "gaussian", 
                      control.compute = list(config = TRUE),
                      control.predictor=list(compute=TRUE),
                      verbose = TRUE)

fitted_val <- get_mod_results_test(pse_fit, pse_inla, "logit_proportion")

invlogit <- function(x) {exp(x)/(1+exp(x))}

pwid_val <- fitted_val %>%
  mutate(across(c(lower, median, upper), invlogit))


#######

pse_out <- fsw_val %>%
  mutate(kp = "FSW") %>%
  bind_rows(
    msm_val %>% mutate(kp = "MSM"),
    pwid_val %>% mutate(kp = "PWID")
  ) %>%
  mutate(area_name = countrycode(iso3, "iso3c", "country.name")) %>%
  select(iso3, area_name, kp, lower:upper)

pse_out <- read_csv("R/pse_out_surveillance_only.csv")
# write.csv(pse_out, "R/pse_out_surveillance_only.csv")

pse_out %>%
  # filter(source == "Surveillance only") %>%
  left_join(pse_dat %>%
              select(iso3, kp) %>%
              distinct() %>%
              mutate(has_data = 1)) %>%
  mutate(has_data = ifelse(is.na(has_data), "No data", "Data")) %>%
  ggplot(aes(x=iso3, y=median, color=has_data)) +
    geom_pointrange(aes(ymin=lower, ymax=upper)) +
    geom_hline(data = data.frame(yintercept = 0.01, kp = "MSM", iso3 = c("AGO", "ZWE")), linetype = 3, aes(yintercept = yintercept), color="red") +
    facet_wrap(~kp, nrow=3, scales="free") +
    scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(x=element_blank(), y="Population proportion", color = "Informed by:") +
    moz.utils::standard_theme() +
    theme(axis.text.x = element_text(size=10))

pse_out %>%
  # filter(source == "Surveillance only") %>%
  left_join(geographies) %>%
  ggplot() +
    geom_sf(data = grey, aes(geometry = geometry), fill="darkgrey") +
    geom_sf(aes(geometry = geometry, fill=median)) +
    moz.utils::standard_theme() +
    viridis::scale_fill_viridis(labels = scales::label_percent()) +
    labs(fill = "PSE proportion") +
    coord_sf(datum = NA) +
    theme(legend.key.width = unit(1.5, "cm")) +
    facet_wrap(~kp, ncol=3)
  

pse_out %>%
  filter(source == "Surveillance only") %>%
  left_join(region) %>%
  group_by(kp, region) %>%
  count()
  summarise(median = 100*median(median))
  
reg_med <- pse_out %>%
    filter(source == "Surveillance only") %>%
    left_join(region) %>%
    left_join(pse_dat %>%
                select(iso3, kp) %>%
                distinct() %>%
                mutate(has_data = 1)) %>%
    mutate(has_data = ifelse(is.na(has_data), "No data", "Data")) %>%
    group_by(kp, region, has_data) %>%
    count()

reg_med %>% bind_rows(
  reg_med %>%
    ungroup() %>%
    group_by(kp, has_data) %>%
    summarise(n = sum(n)) %>%
    mutate(region = "SSA") 
) %>%
  mutate(region = factor(region, levels = c("SSA", "ESA", "WCA"))) %>%
  arrange(kp, region) %>%
  group_by(kp, region) %>%
  summarise(ratio = n[has_data == "Data"]/(n[has_data == "Data"] + n[has_data == "No data"]))


pse_out %>%
  filter(source == "Surveillance only") %>%
  left_join(region) %>%
  group_by(kp, region) %>%
  summarise(tibble(x = 100*quantile(median, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75))) %>%
  bind_rows(
    pse_out %>%
      filter(source == "Surveillance only") %>%
      left_join(region) %>%
      group_by(kp) %>%
      summarise(tibble(x = 100*quantile(median, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75))) %>%
      mutate(region = "SSA")
  ) %>%
  mutate(region = factor(region, levels = c("SSA", "ESA", "WCA"))) %>%
  arrange(kp, region) %>%
  View()
