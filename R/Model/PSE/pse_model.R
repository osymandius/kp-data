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

pse_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final.csv")

lso_pop_id <- orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', "LSO", '")'), draft = FALSE)

lso_pop <- read_csv(paste0("archive/aaa_scale_pop/", lso_pop_id, "/interpolated_population.csv")) %>%
  left_join(read_sf("archive/lso_data_areas/20211015-090317-5bfee435/lso_areas.geojson") %>%
              st_drop_geometry() %>%
              select(area_id, area_name, area_level)) %>%
  filter(area_level == 1) %>%
  moz.utils::five_year_to_15to49("population")

pse_dat <- pse_dat %>%
  mutate(iso3 = countrycode(country.name, "country.name", "iso3c")) %>%
  filter(!(iso3 == "LSO" & year == 2018)) %>%
  bind_rows(
    pse_dat %>%
      filter(iso3 == "LSO", year == 2018) %>%
      select(-population) %>%
      left_join(lso_pop %>% select(area_name, year, sex, population) %>% ungroup()) %>%
      mutate(population_proportion = pse/population) %>%
      select(colnames(pse_dat))
  )

pse_dat <- pse_dat %>%
  mutate(population_proportion = pse/population,
         ) %>%
  filter(population_proportion != 0, !is.na(population_proportion), population_proportion < 1) %>%
  left_join(region %>% select(region, iso3)) %>%
  mutate(
    logit_proportion = logit(population_proportion)
  ) %>%
  ungroup %>%
  select(iso3, year, kp, method, simple_method, logit_proportion, population_proportion, ref) %>%
  filter(iso3 != "LBR",
         !(iso3 == "BFA" & kp == "PWID"))

pse_inla <- crossing(iso3 = ssa_iso3) %>%
  bind_rows(pse_dat %>%
              filter(kp == "FSW") %>%
              mutate(method = factor(method)) %>%
              group_by(ref) %>%
              mutate(id.ref = cur_group_id(),
                     id.ref = ifelse(is.na(ref), NA, id.ref)) %>%
              ungroup) %>%
  left_join(geographies %>% st_drop_geometry()) %>%
  select(iso3, logit_proportion, method, id.iso3, id.ref)

pse_formula <- logit_proportion ~ f(id.iso3, model = "besag", scale.model = TRUE, graph = "national_level_adj.adj") + method + f(id.ref, model = "iid")

fsw_fit <- INLA::inla(pse_formula,
                  data = pse_inla,
                  family = "gaussian", 
                  control.compute = list(config = TRUE),
                  control.predictor=list(compute=TRUE),
                  verbose = TRUE)

log_prec_spatial <- fsw_fit$internal.marginals.hyperpar$`Log precision for id.iso3`
hist(log_prec_spatial)
MASS::fitdistr()

fitted_val <- get_mod_results_test(fsw_fit, pse_inla, "logit_proportion")

# invlogit <- function(x) {exp(x)/(1+exp(x))}

fsw_val <- fitted_val %>%
  mutate(across(c(lower, median, upper), invlogit))

######################

pse_inla <- crossing(iso3 = ssa_iso3) %>%
  bind_rows(pse_dat %>%
              filter(kp == "MSM") %>%
              mutate(method = factor(method)) %>%
              group_by(ref) %>%
              mutate(id.ref = cur_group_id(),
                     id.ref = ifelse(is.na(ref), NA, id.ref)) %>%
              ungroup) %>%
  mutate(idx = row_number(),
         id.iso3 = as.numeric(factor(iso3))) %>%
  select(iso3, logit_proportion, method, id.iso3, id.ref)
 
msm_fit <- INLA::inla(pse_formula,
                      data = pse_inla,
                      family = "gaussian", 
                      control.compute = list(config = TRUE),
                      control.predictor=list(compute=TRUE),
                      verbose = TRUE)

fitted_val <- get_mod_results_test(msm_fit, pse_inla, "logit_proportion")

invlogit <- function(x) {exp(x)/(1+exp(x))}

msm_val <- fitted_val %>%
  mutate(across(c(lower, median, upper), invlogit))

######################

pse_inla <- crossing(iso3 = ssa_iso3) %>%
  bind_rows(pse_dat %>%
              filter(kp == "PWID") %>%
              mutate(method = factor(method)) %>%
              group_by(ref) %>%
              mutate(id.ref = cur_group_id(),
                     id.ref = ifelse(is.na(ref), NA, id.ref)) %>%
              ungroup) %>%
  mutate(idx = row_number(),
         id.iso3 = as.numeric(factor(iso3))) %>%
  select(iso3, logit_proportion, method, id.iso3, id.ref)

prec.prior <- list(prec= list(prior = "normal", param = c(0.4, 6.25)))

# pwid_pse_formula <- logit_proportion ~ f(id.iso3, model = "besag",
#                                     scale.model = TRUE,
#                                     graph = "national_level_adj.adj",
#                                     hyper = prec.prior) + method

pwid_fit <- INLA::inla(pse_formula,
                      data = pse_inla,
                      family = "gaussian", 
                      control.compute = list(config = TRUE),
                      control.predictor=list(compute=TRUE),
                      verbose = TRUE)

fitted_val <- get_mod_results_test(pwid_fit, pse_inla, "logit_proportion")

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
  select(iso3, area_name, kp, lower:upper) %>%
  left_join(region)

# pse_out <- read_csv("R/Model/PSE/pse_estimates.csv")
write.csv(pse_out, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_estimates.csv")

pse_dat %>%
  count(method)

fsw_fit$summary.fixed %>%
  mutate(kp = "FSW") %>%
  rownames_to_column("method") %>%
  bind_rows(
    msm_fit$summary.fixed %>%
      mutate(kp = "MSM") %>%
      rownames_to_column("method"),
    pwid_fit$summary.fixed %>%
      mutate(kp = "PWID") %>%
      rownames_to_column("method") 
  ) %>%
  filter(method != "(Intercept)") %>%
  mutate(method = str_remove(method, "method")) %>%
  left_join(pse_dat %>%
              count(method)) %>%
  mutate(method = paste0(method, "  n = (", n, ")")) %>%
  ggplot(aes(x=kp)) +
    geom_pointrange(position = position_dodge(.9), aes(y=`0.5quant`, ymin = `0.025quant`, ymax = `0.975quant`)) +
    geom_hline(aes(yintercept = 0), linetype = 3, color = "red") +
    facet_wrap(~method) +
    moz.utils::standard_theme() +
    labs(x=element_blank(), y=element_blank()) +
    theme(panel.background = element_rect(fill=NA, color="black"))

pse_out %>%
  mutate(source = "Default") %>%
  bind_rows(foo) %>%
  # filter(source == "Surveillance only") %>%
  # left_join(pse_dat %>%
  #             select(iso3, kp) %>%
  #             distinct() %>%
  #             mutate(has_data = 1)) %>%
  mutate(has_data = ifelse(is.na(has_data), "No data", "Data")) %>%
  ggplot(aes(x=iso3, y=median, color=source)) +
    geom_pointrange(aes(ymin=lower, ymax=upper), position = position_dodge(.9)) +
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
  filter(iso3 != "ZAF") %>%
  group_by(kp, region) %>%
  summarise(tibble(x = 100*quantile(median, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75))) %>%
  bind_rows(
    pse_out %>%
      group_by(kp) %>%
      summarise(tibble(x = 100*quantile(median, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75))) %>%
      mutate(region = "SSA")
  ) %>%
  mutate(region = factor(region, levels = c("SSA", "ESA", "WCA"))) %>%
  arrange(kp, region) %>%
  pivot_wider(names_from = q, values_from = x)

pse_out %>%
  filter(iso3 == "ZAF")
