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

geographies <- read_sf("R/Report/R objects for report/Longitude_Graticules_and_World_Countries_Boundaries-shp/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp") %>%
  bind_rows(read_sf("R/Report/R objects for report/ssd_admbnda_imwg_nbs_shp/ssd_admbnda_adm0_imwg_nbs_20180817.shp") %>% mutate(CNTRY_NAME = "South Sudan")) %>%
  mutate(iso3 = countrycode(CNTRY_NAME, "country.name", "iso3c"),
         area_name = countrycode(iso3, "iso3c", "country.name")) %>%
  filter(iso3 %in% ssa_iso3)

geographies <- geographies %>%
  arrange(iso3) %>%
  mutate(id.iso3 = as.numeric(factor(iso3))) %>%
  dplyr::select(iso3, area_name, id.iso3) %>%
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

pse_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final_sourced.csv")

lso_pop_id <- orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', "LSO", '")'), draft = FALSE)

lso_pop <- read_csv(paste0("archive/aaa_scale_pop/", lso_pop_id, "/interpolated_population.csv")) %>%
  left_join(sf::read_sf("archive/lso_data_areas/20201211-100113-b6410c57/lso_areas.geojson") %>%
              st_drop_geometry() %>%
              dplyr::select(area_id, area_name, area_level)) %>%
  filter(area_level == 1) %>%
  moz.utils::five_year_to_15to49("population")

pse_dat <- pse_dat %>%
  mutate(iso3 = countrycode(country.name, "country.name", "iso3c")) %>%
  filter(!(iso3 == "LSO" & year == 2018)) %>%
  bind_rows(
    pse_dat %>%
      filter(iso3 == "LSO", year == 2018) %>%
      dplyr::select(-population) %>%
      left_join(lso_pop %>% dplyr::select(area_name, year, sex, population) %>% ungroup()) %>%
      mutate(population_proportion = pse/population) %>%
      dplyr::select(colnames(pse_dat))
  ) %>%
  mutate(
    method = ifelse(method == "Service multiplier", "Object/event multiplier", method),
    method = ifelse(method == "Object/event multiplier", "Multiplier", method)
  )

pse_dat <- pse_dat %>%
  mutate(population_proportion = pse/population,
         ) %>%
  filter(population_proportion != 0, !is.na(population_proportion), population_proportion < 1) %>%
  left_join(region %>% dplyr::select(region, iso3)) %>%
  mutate(
    logit_proportion = logit(population_proportion),
    method = factor(method, levels=c("3S-CRC", unique(pse_dat$method)[unique(pse_dat$method) != "3S-CRC" & !is.na(unique(pse_dat$method))]))
  ) %>%
  ungroup %>%
  dplyr::select(iso3, year, kp, method, simple_method, logit_proportion, population_proportion, ref) %>%
  filter(iso3 != "LBR",
         !(iso3 == "BFA" & kp == "PWID"))

ref.iid.prec.prior <- list(prec= list(prior = "normal", param = c(1.6, 4)))
spatial.prec.prior <- list(prec= list(prior = "normal", param = c(-0.75, 6.25)))

res <- lapply(c("FSW", "MSM", "PWID"), function(kp_id) {

  pse_inla <- crossing(iso3 = ssa_iso3) %>%
    bind_rows(pse_dat %>%
                filter(kp == kp_id) %>%
                group_by(ref) %>%
                mutate(id.ref = cur_group_id(),
                       id.ref = ifelse(is.na(ref), NA, id.ref)) %>%
                ungroup) %>%
    left_join(geographies %>% st_drop_geometry()) %>%
    dplyr::select(iso3, logit_proportion, method, id.iso3, id.ref)
  
  pse_formula <- logit_proportion ~ 
    f(id.iso3, model = "besag", scale.model = TRUE, graph = "national_level_adj.adj", hyper=spatial.prec.prior) +
    method +
    f(id.ref, model = "iid", hyper = ref.iid.prec.prior)
  
  fit <- INLA::inla(pse_formula,
                    data = pse_inla,
                    family = "gaussian", 
                    control.compute = list(config = TRUE),
                    control.predictor=list(compute=TRUE),
                    verbose = FALSE)
  
  fitted_val <- get_mod_results_test(fit, pse_inla, "logit_proportion")
  
  
  out <- list()
  
  out$res <- fitted_val %>%
    mutate(across(c(lower, median, upper), invlogit),
           kp = kp_id)
  
  out$fixed <- data.frame(fit$summary.fixed) %>%
    rownames_to_column() %>%
    dplyr::select(rowname, starts_with("X")) %>%
    type.convert(as.is = FALSE) %>%
    mutate(kp = kp_id)
  
  out

})


pse_out <- bind_rows(pse_out,
                     lapply(res, "[[", "res") %>%
  bind_rows() %>%
  mutate(area_name = countrycode(iso3, "iso3c", "country.name")) %>%
  dplyr::select(iso3, area_name, kp, lower:upper) %>%
  left_join(region) %>%
  mutate(source = "Source")
)

# pse_out <- read_csv("R/Model/PSE/pse_estimates.csv")
write.csv(pse_out, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_estimates_full.csv")

pse_method <- bind_rows(pse_method, 
                        lapply(res, "[[", "fixed") %>%
  bind_rows() %>%
  filter(rowname != "(Intercept)") %>%
  mutate(rowname = str_remove(rowname, "method")) %>%
  rename(method = rowname) %>%
  full_join(pse_dat %>%
              count(kp, method)) %>%
  full_join(pse_dat %>%
              distinct(kp, ref, method) %>%
              count(kp, method) %>%
              rename(n_studies = n)) %>%
  filter(kp != "TG", !is.na(method)) %>%
  mutate(across(starts_with("X"), exp),
         est = paste0(round(X0.5quant,2), " (", round(X0.025quant,2), "-", round(X0.975quant,2),")"),
         est = ifelse(method == "3S-CRC", 1, est),
         method = fct_relevel(method, 
                              c("3S-CRC", "2S-CRC", "Multiplier", "PLACE/Mapping", "SS-PSE", "Multiple methods - empirical", "Multiple methods - mixture"))
  ) %>%
  arrange(method) %>%
  dplyr::select(method, kp, est, n_studies, n) %>%
  mutate(source = "Sourced")
)
write_csv(pse_method, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_method.csv")

  ggplot(aes(x=kp)) +
  geom_pointrange(position = position_dodge(.9), aes(y=`X0.5quant`, ymin = `X0.025quant`, ymax = `X0.975quant`)) +
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
  #             dplyr::select(iso3, kp) %>%
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

pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")

pse_maps <- lapply(c("FSW", "MSM", "PWID", "TG"), function(x) {
  
  pse_out %>%
    filter(kp == x) %>%
    left_join(geographies) %>%
    ggplot() +
    geom_sf(data = grey, aes(geometry = geometry), fill="darkgrey") +
    geom_sf(aes(geometry = geometry, fill=median)) +
    moz.utils::standard_theme() +
    # viridis::scale_fill_viridis(labels = scales::label_percent()) +
    scale_fill_gradientn(colours = pal, labels = scales::label_percent()) +
    labs(fill = "PSE proportion", title = x) +
    coord_sf(datum = NA) +
    theme(legend.key.width = unit(1.5, "cm"),
          plot.title = element_text(hjust = 0.5, face = "bold", size=16))
  
})

ggpubr::ggarrange(pse_maps[[1]], pse_maps[[2]], pse_maps[[3]], pse_maps[[4]], nrow=1)

  

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
                dplyr::select(iso3, kp) %>%
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
