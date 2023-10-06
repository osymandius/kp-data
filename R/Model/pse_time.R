library(tidyverse)
library(moz.utils)
library(sf)
  
pse_time <- function(kp_id) {
  pse_inla <- crossing(genpop_pred %>% distinct(region, iso3, area_id, kp),
             year = c(2010:2022)
             ) %>%
    filter(kp == kp_id) %>%
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
           id.iso3_rep = id.iso3,
           id.method_rep = id.method) %>%
    dplyr::select(iso3, kp, id.iso3, id.iso3_rep, area_id, id.area, logit_proportion, fe_method, id.method, id.method_rep, method, id.ref, year, id.year) %>%
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
    f(id.area, model = "besag", scale.model = TRUE, graph = admin1_adj(), hyper=prec.prior) +
    f(id.iso3, model = "besag", scale.model = TRUE, graph = national_adj(), hyper = prec.prior) +
    fe_method +
    f(id.iso3_rep, id.year, model = "iid", hyper = prec.prior, constr = T) +
    id.year +
    f(id.method, model = "iid", hyper = prec.prior) +
    f(id.method_rep, id.year, model = "iid", hyper = prec.prior, constr = T) +
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
  qtls <- apply(pse_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
  ident <- pse_inla[ind.effect, ]
  pse_samples <- ident %>% cbind(pse_samples)
  
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
  out$pse_fixed <- pse_fit$summary.fixed %>% rownames_to_column("indicator") %>% mutate(kp = kp_id)
  out$pse_method_slope <- pse_fit$summary.random$id.method_rep %>% mutate(kp = kp_id)
  out
}

mod <- lapply(c("FSW", "MSM", "PWID"), pse_time)
names(mod) <- c("FSW", "MSM", "PWID")

mod %>%
  lapply("[[", "pse_fixed") %>%
  bind_rows() %>%
  filter(indicator == "(Intercept)") %>%
  mutate(across(`0.025quant`:`0.975quant`, ~round(.x, 2)))

mod %>%
  lapply("[[", "pse_method_slope") %>%
  bind_rows() %>%
  mutate(across(`0.025quant`:`0.975quant`, ~round(.x, 2)))
####

urban_proportion <- read_csv("~/Downloads/jeff-kp-runthrough/urban_proportion.csv")


pse_s <- mod %>%
  lapply("[[", "pse_samples") %>%
  bind_rows() %>%
  left_join(kp_to_sex()) %>%
  left_join(region() %>% select(iso3, region)) %>%
  select(region, iso3, area_id, kp, sex, year, as.character(1:1000))

pop <- read_csv("~/Downloads/jeff-kp-runthrough/pop.csv")

urban_prop_s <- matrix(rep(rbeta(1000, 5, 3), nrow(pse_s)), nrow = nrow(pse_s), byrow = TRUE)
rural_pse_s <- pse_s %>%
  select(region, iso3, area_id, kp, sex, year) %>%
  cbind(invlogit(pse_s[as.character(1:1000)]) * urban_prop_s)

urban_count_samples <- pse_s %>%
  left_join(pop %>% select(area_id, sex, population)) %>%
  left_join(urban_proportion %>% select(area_id, urban_proportion)) %>%
  mutate(across(as.character(1:1000), ~invlogit(.x) * population * urban_proportion),
         indicator = "pse_urban_count")

rural_count_samples <- rural_pse_s %>%
  left_join(pop %>% select(area_id, sex, population)) %>%
  left_join(urban_proportion %>% select(area_id, urban_proportion)) %>%
  mutate(across(as.character(1:1000), ~.x * population * (1-urban_proportion)))

pse_count_samples <- bind_rows(urban_count_samples, rural_count_samples) %>%
  group_by(region, iso3, area_id, kp, year) %>%
  summarise(across(as.character(1:1000), sum)) %>%
  mutate(indicator = "pse_count")


denominators <- pop %>%
  separate(area_id, into = c("iso3", NA), sep = 3, remove = F) %>%
  left_join(region) %>%
  left_join(urban_proportion) %>%
  mutate(urban_population = population*urban_proportion) %>%
  filter(!is.na(urban_population)) %>% ## TODO: better way of selecting the area_level that is being modelled. Fine for now
  left_join(kp_to_sex() %>% filter(kp %in% unique(pse_dat$kp))) %>%
  group_by(region, iso3, kp) %>% 
  summarise(urban_population = sum(urban_population),
            population = sum(population)) %>%
  left_join(spec_dat %>%
              group_by(iso3) %>%
              summarise(plhiv = sum(hivpop))
  )
  
pse_count_samples <- pse_count_samples %>%
  group_by(region, iso3, kp, indicator, year) %>%
  summarise(across(as.character(1:1000), sum))

pse_count_samples <- pse_count_samples %>%
  filter(indicator %in% c("pse_count", "pse_urban_count", "kplhiv")) %>%
  left_join(denominators %>% 
              rename(pse_urban_count = urban_population, pse_count = population, kplhiv = plhiv) %>% 
              pivot_longer(-c(region, iso3, kp), names_to = "indicator", values_to = "denominator")
  ) %>%
  select(region:year, denominator, as.character(1:1000)) %>%
  mutate(across(as.character(1:1000), ~.x/denominator),
         indicator = case_when(
           indicator == "kplhiv" ~ "plhiv_prop",
           indicator == "pse_count" ~ "pse_prop",
           indicator == "pse_urban_count" ~ "pse_urban_prop"
         )
  )

country_qtls <- apply(pse_count_samples[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))

country_res <- pse_count_samples %>%
  select(region:year) %>%
  cbind(data.frame(t(country_qtls)))

colnames(country_res) <- c("region", "iso3", "kp", "indicator", "year", "lower", "median", "upper")

country_res <- country_res %>%
  arrange(iso3, kp)
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

country_res %>%
  filter(kp == "FSW") %>%
  ggplot(aes(x=year, y=median)) +
  geom_point(data = pse_inla %>% 
               mutate(value = invlogit(logit_proportion)) %>%
               filter(!is.na(logit_proportion), 
                      kp == "FSW"
                      # value < 0.2
                      ), 
             aes(y=value)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_percent() +
  scale_x_continuous(labels = scales::label_number(accuracy = 1, big.mark = ""), breaks = c(2012, 2017, 2022)) +
  facet_wrap(~countrycode(iso3, "iso3c", "country.name")) +
  standard_theme() +
  labs(x=element_blank(), y="FSW PSE proportion (%)") +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18)) +
  coord_cartesian(ylim = c(0, 0.15))

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
  
country_res %>%
  filter(iso3 %in% c("MWI", "KEN", "MOZ")) %>%
  name_kp() %>%
  ggplot(aes(x=factor(year), y=median, group = iso3)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_jitter(data = pse_dat %>%
                 filter(iso3 %in% c("MWI", "KEN", "MOZ"),
                        kp != "TGW") %>%
                 name_kp(),
               aes(y=proportion_estimate, color = method), size = 2) +
    scale_y_log10(labels = scales::label_percent(accuracy = .1)) +
    scale_x_discrete(breaks = c(2010, 2015, 2020)) +
    facet_grid(kp~countrycode(iso3, "iso3c", "country.name")) +
    standard_theme() +
    theme(panel.grid = element_blank(),
          legend.text = element_text(size = 15),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 16)) +
    labs(y= "PSE proportion", x=element_blank(), color = element_blank())

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
