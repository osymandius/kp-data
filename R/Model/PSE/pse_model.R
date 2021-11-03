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

invlogit <- function(x) {exp(x)/(1+exp(x))}
logit <- function(x) {log(x/(1-x))}

region <- read.csv("~/Documents/GitHub/fertility_orderly/global/region.csv") %>%
  mutate(iso3 = toupper(iso3))

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
  mutate(population_proportion = pse/population) %>%
  filter(population_proportion != 0, !is.na(population_proportion), population_proportion < 1) %>%
  bind_rows(read.csv("R/ZAF/assign_populations/pse_prevalence.csv") %>% mutate(iso3 = "ZAF")) %>%
  left_join(region %>% select(region, iso3)) %>%
  mutate(
    logit_proportion = logit(population_proportion),
    is_national = ifelse(area_name == country.name, 1, 0)
  ) %>%
  select(iso3, year, kp, is_national, logit_proportion, population_proportion)

# pse_dat <- read.csv("R/Model/pse_surveillance_data.csv")
# write_csv(pse_dat, "R/Model/pse_surveillance_data.csv")

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

log_prec_besag_msm <- pse_fit$internal.marginals.hyperpar$`Log precision for id.iso3`

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

prec.prior <- list(prec= list(prior = "normal", param = c(0.4, 6.25)))

pse_formula <- logit_proportion ~ f(id.iso3, model = "besag",
                                    scale.model = TRUE,
                                    graph = "national_level_adj.adj",
                                    hyper = prec.prior)

pse_fit <- INLA::inla(pse_formula,
                      data = pse_inla,
                      family = "gaussian", 
                      control.compute = list(config = TRUE),
                      control.predictor=list(compute=TRUE),
                      verbose = TRUE)

fitted_val <- get_mod_results_test(pse_fit, pse_inla, "logit_proportion")

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

# pse_out <- read_csv("R/pse_out_surveillance_only.csv")
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
