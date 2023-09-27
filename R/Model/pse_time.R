library(tidyverse)
library(moz.utils)
library(sf)

logit <- plogis
invlogit <- qlogis

areas <- read_sf("~/Downloads/jeff-kp-runthrough/areas.geojson")

geographies <- read_sf(national_areas()) %>%
  mutate(iso3 = area_id) %>%
  arrange(iso3) %>%
  group_by(iso3) %>%
  mutate(id.iso3 = cur_group_id())

######## PSE Model ##########

pse_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final_sourced.csv")

pse_dat <- pse_dat %>%
  rename(proportion_estimate = prop_estimate) %>%
  mutate(
    fe_method = case_when(
      str_detect(method, "methods") ~ "other methods",
      method == "PLACE/Mapping" ~ method,
      TRUE ~ "empirical"
    )
  )

pse_dat <- pse_dat %>%
  filter(proportion_estimate != 0, !is.na(proportion_estimate), proportion_estimate < 1) %>%
  left_join(region %>% dplyr::select(region, iso3)) %>%
  mutate(
    logit_proportion = logit(proportion_estimate),
    fe_method = factor(fe_method, levels=c("empirical", unique(pse_dat$fe_method)[unique(pse_dat$fe_method) != "empirical" & !is.na(unique(pse_dat$fe_method))])),
  ) %>%
  ungroup %>%
  dplyr::select(iso3, area_id, kp, year, fe_method, method, logit_proportion, proportion_estimate, study_idx)

pse_dat <- pse_dat %>%
  mutate(method = case_when(
    method %in% c("Object Multiplier", "Multiplier") ~ "Object multiplier",
    method %in% c("Service Multiplier") ~ "Service multiplier",
    method == "PLACE/mapping" ~ "PLACE/Mapping",
    TRUE ~ method
  ),
  fe_method = ifelse(method == "PLACE/Mapping", method, as.character(fe_method)),
  fe_method = ifelse(method == "Multiple methods - empirical", "empirical", as.character(fe_method)),
  fe_method = factor(fe_method))

method.iid.prec.prior <- list(prec= list(prior = "normal", param = c(4, 1)))
  
pse_time <- function(kp_id) {
  pse_inla <- 
    # genpop_pred %>%
    # distinct(region, iso3, area_id, kp) %>%
    # filter(kp == kp_id) %>%
    
    
    crossing(
      year = c(2010:2022),
      iso3 = ssa_iso3()) %>%
    
    # crossing(
    #   year = c(2010:2022),
    #   method = unique(pse_dat$method)) %>%
    bind_rows(pse_dat %>%
                filter(kp == kp_id) %>%
                group_by(study_idx) %>%
                mutate(id.ref = cur_group_id()) %>%
                ungroup %>%
                arrange(fe_method) %>%
                mutate(id.method = as.numeric(fct_inorder(method)),
                       id.method = ifelse(method %in% c("Multiple methods - mixture", "PLACE/Mapping"), NA, id.method))
    ) %>%
    left_join(areas %>% st_drop_geometry()) %>%
    left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
    mutate(area_id = ifelse(is.na(area_id), iso3, area_id),
           id.year = multi.utils::to_int(year),
           id.iso3_rep = id.iso3) %>%
    dplyr::select(iso3, kp, id.iso3, id.iso3_rep, area_id, id.area, logit_proportion, fe_method, id.method, method, id.ref, year, id.year) %>%
    mutate(idx = row_number())
  
  nat_level_obs <- pse_inla %>%
    filter(area_id == iso3 & !is.na(logit_proportion)) %>%
    ungroup() %>%
    group_by(id.ref) %>%
    mutate(id.ref.nat = cur_group_id(),
           id.ref = NA)
  
  pse_inla <- pse_inla %>%
    filter(!idx %in% nat_level_obs$idx) %>%
    bind_rows(nat_level_obs)
  # 
  prec.prior <- list(prec= list(prior = "normal", param = c(2,1)))
  
  pse_formula <- logit_proportion ~ 
    f(id.iso3, model = "besag", scale.model = TRUE, graph = national_adj(), hyper = prec.prior) +
    # f(id.iso3, model = "besag", scale.model = TRUE, graph = "geog.adj") +
    fe_method +
    # id.year
    f(id.iso3_rep, id.year, model = "iid", hyper = prec.prior, constr = T) +
    id.year +
    # f(id.year2, model = "ar1", group=id.iso3, control.group=list(model = "besag", scale.model = TRUE, graph = moz.utils::national_adj()))
    # f(id.method, model = "iid") +
    f(id.ref, model = "iid", hyper = prec.prior)
  # f(id.iso3, model = "iid") +
  # f(id.ref.nat, model = "iid")
  
  pse_fit <- INLA::inla(pse_formula,
                        data = pse_inla,
                        family = "gaussian", 
                        control.compute = list(config = TRUE,
                                               dic = T),
                        control.predictor=list(compute=TRUE),
                        verbose = FALSE)
  # fitted_val <- get_mod_results_test(fit, pse_inla, "logit_proportion")
  
  df <- pse_inla %>%
    filter(across(all_of("logit_proportion"), ~!is.na(.x)))
  
  print("Sampling..")
  samples <- inla.posterior.sample(1000, pse_fit)
  print("Done sampling")
  contents = pse_fit$misc$configs$contents
  effect = "Predictor"
  id.effect = which(contents$tag==effect)
  ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])
  
  ind.effect <- 1:(nrow(pse_inla) - nrow(df))
  
  samples.effect = lapply(samples, function(x) x$latent[ind.effect])
  
  pse_samples <- matrix(sapply(samples.effect, cbind), ncol=1000)
  
  ident <- pse_inla[ind.effect, ]
  
  qtls <- apply(pse_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
  pse <- ident %>%
    ungroup() %>%
    mutate(
      lower = qtls[1,],
      median = qtls[2,],
      upper = qtls[3,],
      kp = kp_id,
      indicator = "pse"
    ) 
  
  out <- list()
  out$pse <- pse
  out$pse_samples <- pse_samples
  # out$pse_fixed <- pse_fixed
  out
}

mod <- lapply(c("FSW", "MSM", "PWID"), pse_time)

mod %>%
  lapply("[[", "pse") %>%
  bind_rows()
# 
# pse %>%
#   ggplot(aes(x=year, y=median)) +
#     geom_point(data = pse_inla %>% filter(!is.na(logit_proportion), kp == "FSW"), aes(y=logit_proportion)) +
#     geom_line() +
#     scale_y_continuous(labels = function(x) round(100*invlogit(x), 2)) +
#     scale_x_continuous(labels = scales::label_number(accuracy = 1, big.mark = "")) +
#     facet_wrap(~countrycode(iso3, "iso3c", "country.name")) +
#     standard_theme() +
#     labs(x=element_blank(), y="PSE proportion (%)") +
#     theme(panel.grid = element_blank())

# pse %>%
#   ggplot(aes(x=year, y=median)) +
#   geom_point(data = pse_inla %>% 
#                        mutate(value = invlogit(logit_proportion)) %>%
#                        filter(!is.na(logit_proportion), 
#                               kp == "FSW"), 
#              aes(y=logit_proportion)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
#   scale_x_continuous(labels = scales::label_number(accuracy = 1, big.mark = "")) +
#   facet_wrap(~countrycode(iso3, "iso3c", "country.name")) +
#   standard_theme() +
#   labs(x=element_blank(), y="PSE proportion (%)") +
#   theme(panel.grid = element_blank())

msm_pse %>%
  ggplot(aes(x=year, y=invlogit(median))) +
  geom_point(data = pse_inla %>% 
               mutate(value = invlogit(logit_proportion)) %>%
               filter(!is.na(logit_proportion), 
                      kp == "MSM",
                      value < 0.2
                      ), 
             aes(y=value)) +
  geom_line() +
  geom_ribbon(aes(ymin = invlogit(lower), ymax = invlogit(upper)), alpha = 0.2) +
  scale_percent() +
  scale_x_continuous(labels = scales::label_number(accuracy = 1, big.mark = ""), breaks = c(2012, 2017, 2022)) +
  facet_wrap(~countrycode(iso3, "iso3c", "country.name")) +
  standard_theme() +
  labs(x=element_blank(), y="PSE proportion (%)") +
  theme(panel.grid = element_blank())

pse_dat %>%
  filter(!is.na(proportion_estimate)) %>%
  distinct(iso3, kp) %>%
  count(kp)

pse_dat %>% 
  filter(!is.na(proportion_estimate)) %>%
  distinct(iso3, kp, year, study_idx) %>%
  count(kp, year) %>%
  name_kp(F) %>%
  mutate(flib = "zing") %>%
  ggplot(aes(x=year, y=n, fill = flib)) +
    geom_col(show.legend = F) +
    facet_wrap(~kp) +
    standard_theme() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.07))) +
    scale_manual("fill", 1) +
    labs(y="Number of studies", x=element_blank()) +
    theme(panel.grid = element_blank())

crossing(iso3 = ssa_iso3(),
         kp = c("FSW", "MSM", "PWID", "TGW")) %>%
  left_join(
    pse_dat %>% 
      filter(!is.na(proportion_estimate)) %>%
      distinct(iso3, kp, year, study_idx) %>%
      group_by(iso3, kp) %>%
      summarise(n = n())
    ) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  name_kp(F) %>%
  mutate(flib = "zing") %>%
  ggplot(aes(x=n, fill = flib)) +
    geom_bar(show.legend = F) +
    facet_wrap(~kp) +
    standard_theme() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.07))) +
    scale_manual("fill", 1) +
    labs(x="Number of studies", y = "Number of countries") +
    theme(panel.grid = element_blank())

crossing(iso3 = ssa_iso3(),
         kp = c("FSW", "MSM", "PWID", "TGW")) %>%
  left_join(
    pse_dat %>% 
      filter(!is.na(proportion_estimate)) %>%
      distinct(iso3, kp, year, study_idx) %>%
      group_by(iso3, kp) %>%
      summarise(n = n())
  ) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  group_by(kp) %>%
  summarise(median = median(n))

pse <- bind_rows(fsw_pse, msm_pse, pwid_pse)

pse <- mod %>%
  lapply("[[", "pse")

names(pse) <- c("FSW", "MSM", "PWID")
  
pse %>%
  bind_rows(.id = "kp") %>%
  filter(iso3 %in% c("MWI", "KEN", "MOZ")) %>%
  name_kp(F) %>%
  ggplot(aes(x=factor(year), y=invlogit(median), group = iso3)) +
    geom_line() +
    geom_ribbon(aes(ymin = invlogit(lower), ymax = invlogit(upper)), alpha = 0.2) +
    geom_jitter(data = pse_dat %>%
                 filter(iso3 %in% c("MWI", "KEN", "MOZ"),
                        kp != "TGW") %>%
                 name_kp(F),
               aes(y=proportion_estimate)) +
    scale_y_log10(labels = scales::label_percent(accuracy = .1)) +
    scale_x_discrete(breaks = c(2010, 2015, 2020)) +
    facet_grid(kp~countrycode(iso3, "iso3c", "country.name")) +
    standard_theme() +
    theme(panel.grid = element_blank()) +
    labs(y= "PSE proportion", x=element_blank())

########

pse_formula <- logit_proportion ~ 
  f(method, hyper = prec.prior) +
  f(method2, id.year, model = "iid", hyper = prec.prior, constr = T) +
  id.year +
  # f(id.year2, model = "ar1", group=id.iso3, control.group=list(model = "besag", scale.model = TRUE, graph = moz.utils::national_adj()))
  # f(id.method, model = "iid") +
  f(id.ref, model = "iid", hyper = prec.prior)
# f(id.iso3, model = "iid") +
# f(id.ref.nat, model = "iid")

pse_fit <- INLA::inla(pse_formula,
                      data = pse_inla %>% 
                        mutate(
                          method = ifelse(str_detect(method, "multiplier"), "2S-CRC", method),
                          method2 = method) %>%
                        droplevels(),
                      family = "gaussian", 
                      control.compute = list(config = TRUE,
                                             dic = T),
                      control.predictor=list(compute=TRUE),
                      verbose = FALSE)
# fitted_val <- get_mod_results_test(fit, pse_inla, "logit_proportion")

df <- pse_inla %>%
  filter(across(all_of("logit_proportion"), ~!is.na(.x)))

print("Sampling..")
samples <- inla.posterior.sample(1000, pse_fit)
print("Done sampling")
contents = pse_fit$misc$configs$contents
effect = "Predictor"
id.effect = which(contents$tag==effect)
ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])

ind.effect <- 1:(nrow(pse_inla) - nrow(df))

samples.effect = lapply(samples, function(x) x$latent[ind.effect])

pse_samples <- matrix(sapply(samples.effect, cbind), ncol=1000)

ident <- pse_inla[ind.effect, ]

qtls <- apply(pse_samples, 1, quantile, c(0.025, 0.5, 0.975))

new_pse <- ident %>%
  ungroup() %>%
  mutate(
    lower = qtls[1,],
    median = qtls[2,],
    upper = qtls[3,],
    indicator = "pse"
  )

new_pse %>%
  ggplot(aes(x=year, y=invlogit(median))) +
    geom_line() +
    geom_point(data = pse_dat %>% 
                 filter(kp == "FSW",
                        proportion_estimate < 0.2) %>%
                 mutate(method = ifelse(str_detect(method, "multiplier"), "2S-CRC", method)), aes(y=proportion_estimate)) +
    facet_wrap(~method)
