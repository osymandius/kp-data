library(INLA)
library(tidyverse)
library(countrycode)
library(sf)
library(lme4)

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
  sd <- apply(sapply(samples.effect, cbind), 1, sd)
  
  samples_ident <- ident %>%
    mutate(
      sd = sd,
      lower = qtls[1,],
      median = qtls[2,],
      upper = qtls[3,]
    )
  
  return(samples_ident)
  
}

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

invlogit <- function(x) {exp(x)/(1+exp(x))}
logit <- function(x) {log(x/(1-x))}

region <- read.csv("~/Documents/GitHub/fertility_orderly/global/region.csv") %>%
  mutate(iso3 = toupper(iso3))

convert_logis_labels <- function(x) {
  paste0(round(plogis(x)*100, 1), "%")
}

prev_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_final_sourced.csv")

imp_denomin <- prev_dat %>%
  filter(!is.na(denominator),
         denominator != 0) %>%
  group_by(kp) %>%
  summarise(quant = quantile(denominator, 0.25))

prev_df <- prev_dat %>%
  bind_rows() %>%
  # bind_rows(.id = "iso3") %>%
  left_join(region %>% select(region, iso3)) %>%
  mutate(denominator = case_when(
    (is.na(denominator) | denominator == 0) & kp == "FSW" ~ filter(imp_denomin, kp == "FSW")$quant,
    (is.na(denominator) | denominator == 0) & kp == "MSM" ~ filter(imp_denomin, kp == "MSM")$quant,
    (is.na(denominator) | denominator == 0) & kp == "PWID" ~ filter(imp_denomin, kp == "PWID")$quant,
    (is.na(denominator) | denominator == 0) & kp == "TG" ~ filter(imp_denomin, kp == "TG")$quant,
    TRUE ~ denominator
  )) %>%
  filter(!is.na(provincial_value),
         value<1) %>%
  ungroup %>%
  mutate(value = ifelse(value == 1, 0.99, value),
         value = ifelse(value ==0, 0.001, value),
         log_kp_prev = log(value),
         log_gen_prev = log(provincial_value),
         log_gen_prev2 = log_gen_prev,
         logit_kp_prev = logit(value),
         logit_gen_prev = logit(provincial_value),
         logit_gen_prev2 = logit_gen_prev,
         positive = round(value * denominator),
         negative = round(denominator - positive),
         method = factor(method, levels = c("lab", "selfreport"))
  )

genpop_pred <- read_csv("R/Model/HIV prevalence/national_genpop_prev.csv") %>%
  mutate(logit_gen_prev = logit(mean)) %>%
  select(-mean)

genpop_pred <- genpop_pred %>%
  bind_rows(
    genpop_pred %>% filter(kp == "FSW") %>% mutate(kp = "TG")
  )

df_logit <- data.frame(logit_gen_prev = logit(seq(0.001, 0.1, 0.005)),
           region = "WCA") %>%
  bind_rows(
    data.frame(logit_gen_prev = logit(seq(0.006, 0.35, 0.005)),
               region = "ESA"),
    genpop_pred
  )

####### PSE

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
    method = ifelse(method == "Object/event multiplier", "Multiplier", method),
    method = ifelse(str_detect(method, "CRC"), "CRC", method)
  )

pse_dat <- pse_dat %>%
  mutate(population_proportion = pse/population,
  ) %>%
  filter(population_proportion != 0, !is.na(population_proportion), population_proportion < 1) %>%
  left_join(region %>% dplyr::select(region, iso3)) %>%
  mutate(
    logit_proportion = logit(population_proportion),
    method = factor(method, levels=c("CRC", unique(pse_dat$method)[unique(pse_dat$method) != "CRC" & !is.na(unique(pse_dat$method))]))
  ) %>%
  ungroup %>%
  dplyr::select(iso3, year, kp, method, simple_method, logit_proportion, population_proportion, ref) %>%
  filter(iso3 != "LBR",
         !(iso3 == "BFA" & kp == "PWID"))

ref.iid.prec.prior <- list(prec= list(prior = "normal", param = c(1.6, 4)))
spatial.prec.prior <- list(prec= list(prior = "normal", param = c(-0.75, 6.25)))

#### ART

art_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_final.csv")

imp_art_denomin <- art_dat %>%
  filter(!is.na(denominator),
         denominator != 0) %>%
  group_by(kp) %>%
  summarise(quant = quantile(denominator, 0.25))


art_df <- art_dat %>%
  bind_rows() %>%
  left_join(region %>% select(region, iso3)) %>%
  mutate(denominator = case_when(
    (is.na(denominator) | denominator == 0) & kp == "FSW" ~ filter(imp_art_denomin, kp == "FSW")$quant,
    (is.na(denominator) | denominator == 0) & kp == "MSM" ~ filter(imp_art_denomin, kp == "MSM")$quant,
    (is.na(denominator) | denominator == 0) & kp == "PWID" ~ filter(imp_art_denomin, kp == "PWID")$quant,
    TRUE ~ denominator
  )) %>%
  filter(value < 1,
         !is.na(provincial_value)) %>%
  mutate(value = ifelse(value == 1, 0.99, value),
         value = ifelse(value ==0, 0.01, value),
         logit_kp_art = logit(value),
         logit_gen_art = logit(provincial_value),
         logit_gen_art2 = logit_gen_art,
         positive = round(value * denominator),
         negative = round(denominator - positive),
         method = factor(method, levels = c("lab", "selfreport"))
  ) %>%
  group_by(year, kp, iso3) %>%
  mutate(idx = cur_group_id())

# df_logit <- crossing(logit_gen_art = logit(seq(0.01, 0.99, 0.01)),
#                      region = c("WCA", "ESA"))

df_logit_art <- crossing(logit_gen_art = logit(seq(0.01, 0.99, 0.01))) %>%
  bind_rows(read_csv("R/Model/ART coverage/national_genpop_art.csv") %>%
              filter(iso3 != "SSD") %>%
              mutate(logit_gen_art = logit(value/100)) %>%
              select(-c(value, year))
  )



prev <- lapply(c("FSW", "MSM", "PWID", "TG"), function(kp_id) {
  
  out <- list()
  
  prev_df <- prev_df %>%
    filter(kp == kp_id) %>%
    group_by(ref) %>%
    mutate(id.ref = cur_group_id(),
           id.ref = ifelse(is.na(ref), NA, id.ref)) %>%
    ungroup()
  
  prev_inla <- df_logit %>% 
    filter(is.na(kp) | kp == kp_id) %>%
    mutate(denominator = 1) %>%
    bind_rows(prev_df %>%
                ungroup) %>%
    select(iso3, logit_gen_prev, method, positive, negative, region, denominator, id.ref)
  
  
  prev_formula <- positive ~ logit_gen_prev + region + logit_gen_prev*region + method + f(id.ref, model = "iid")
  
  prev_fit <- INLA::inla(prev_formula,
                         data = prev_inla,
                         family = "betabinomial", 
                         Ntrials = prev_inla$denominator,
                         # offset = log(denominator),
                         control.compute = list(config = TRUE),
                         control.predictor=list(compute=TRUE),
                         verbose = FALSE)
  
  ###
  
  df <- prev_inla %>%
    filter(across(all_of("positive"), ~!is.na(.x)))
  
  print("Sampling..")
  samples <- inla.posterior.sample(1000, prev_fit)
  print("Done sampling")
  contents = prev_fit$misc$configs$contents
  effect = "Predictor"
  id.effect = which(contents$tag==effect)
  ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])
  
  ind.effect <- 1:(nrow(prev_inla) - nrow(df))
  
  samples.effect = lapply(samples, function(x) x$latent[ind.effect])
  
  prev_samples <- matrix(sapply(samples.effect, cbind), ncol=1000)
  
  ident <- prev_inla[ind.effect, ]

  qtls <- apply(prev_samples, 1, quantile, c(0.025, 0.5, 0.975))

  prev <- ident %>%
    mutate(
      lower = qtls[1,],
      median = qtls[2,],
      upper = qtls[3,],
      indicator = "prevalence"
    )
  
  prev_fixed <- data.frame(prev_fit$summary.fixed) %>%
    rownames_to_column() %>%
    mutate(kp = kp_id)
  
  out <- list()
  out$prev_samples <- prev_samples
  out$prev <- prev
  out$prev_fixed <- prev_fixed
  out
  
})
  

  
  ###
  
pse <- lapply(c("FSW", "MSM", "PWID"), function(kp_id) {
    
    pse_inla <- crossing(iso3 = ssa_iso3[ssa_iso3 != "SSD"]) %>%
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
    
    pse_fit <- INLA::inla(pse_formula,
                          data = pse_inla,
                          family = "gaussian", 
                          control.compute = list(config = TRUE),
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
      mutate(
        lower = qtls[1,],
        median = qtls[2,],
        upper = qtls[3,],
        indicator = "pse"
      )
    
    pse_fixed <- data.frame(fit$summary.fixed) %>%
      rownames_to_column() %>%
      dplyr::select(rowname, starts_with("X")) %>%
      type.convert(as.is = FALSE) %>%
      mutate(kp = kp_id)
    
    out <- list()
    out$pse <- pse
    out$pse_samples <- pse_samples
    out$pse_fixed <- pse_fixed
    out
    
})

art <- lapply(c("FSW", "MSM", "PWID"), function(kp_id) {
  
  art_df <- art_df %>%
    group_by(ref) %>%
    filter(kp == kp_id) %>%
    # filter(kp == "PWID") %>%
    mutate(id.ref = cur_group_id(),
           id.ref = ifelse(is.na(ref), NA, id.ref),
           logit_gen_art2 = logit_gen_art) %>%
    ungroup()
  
  
  art_inla <- df_logit_art %>%
    filter(is.na(kp) | kp == kp_id) %>%
    mutate(denominator = 1) %>%
    bind_rows(art_df %>%
                ungroup) %>%
    select(logit_gen_art, logit_gen_art2, positive, negative, region, denominator, method, id.ref)
  
  art_formula <- positive ~ logit_gen_art + method + f(id.ref, model = "iid")
  
  if(kp_id == "PWID") {
    art_formula <- positive ~ f(logit_gen_art, mean.linear = 0.66, prec.linear = 25, model = "linear") + method + f(id.ref, model = "iid")
  }
  
  # 
  # # pwid_pse_formula <- logit_proportion ~ f(id.iso3, model = "besag",
  # #                                     scale.model = TRUE,
  # #                                     graph = "national_level_adj.adj",
  # #                                     hyper = prec.prior) + method
  
  art_fit <- INLA::inla(art_formula,
                        data = art_inla,
                        family = "betabinomial", 
                        Ntrials = art_inla$denominator,
                        # offset = log(denominator),
                        control.compute = list(config = TRUE),
                        control.family = list(link = "logit"),
                        control.predictor=list(compute=TRUE),
                        verbose = FALSE)
  
  
  df <- art_inla %>%
    filter(across(all_of("positive"), ~!is.na(.x)))
  
  print("Sampling..")
  samples <- inla.posterior.sample(1000, art_fit)
  print("Done sampling")
  contents = art_fit$misc$configs$contents
  effect = "Predictor"
  id.effect = which(contents$tag==effect)
  ind.effect = contents$start[id.effect]-1 + (1:contents$length[id.effect])
  
  ind.effect <- 1:(nrow(art_inla) - nrow(df))
  
  samples.effect = lapply(samples, function(x) x$latent[ind.effect])
  
  art_samples <- matrix(sapply(samples.effect, cbind), ncol=1000)
  
  ident <- art_inla[ind.effect, ]
  
  qtls <- apply(art_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
  art <- ident %>%
    mutate(
      lower = qtls[1,],
      median = qtls[2,],
      upper = qtls[3,],
      indicator = "art"
    )
  
  out <- list()
  out$art <- art
  out$art_samples <- art_samples
  out
  
})

kplhiv_art <- Map(function(prev, pse, art) {
  prev <- prev$prev_samples
  pse <- pse$pse_samples
  art <- art$art_samples
  
  kplhiv_samples <- invlogit(prev[90:126,]) * invlogit(pse)
  kpart_samples <- kplhiv_samples * invlogit(art[100:136])
  kplhiv_qtls <- apply(kplhiv_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
  kplhiv <- data.frame(iso3 = ssa_iso3[ssa_iso3 != "SSD"],
             lower = kplhiv_qtls[1,],
             median = kplhiv_qtls[2,],
             upper = kplhiv_qtls[3,],
             indicator = "KPLHIV"
             )
  
  kpart_qtls <- apply(kpart_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
  kpart <- data.frame(iso3 = ssa_iso3[ssa_iso3 != "SSD"],
                       lower = kpart_qtls[1,],
                       median = kpart_qtls[2,],
                       upper = kpart_qtls[3,],
                       indicator = "KPART"
  )
  
  bind_rows(kplhiv, kpart)
  
  }, prev[1:3], pse, art) %>%
  setNames(c("FSW", "MSM", "PWID")) %>%
  bind_rows(.id = "kp")

names(prev) <- c("FSW", "MSM", "PWID", "TG")
names(pse) <- c("FSW", "MSM", "PWID")
names(art) <- c("FSW", "MSM", "PWID")

prev_res <- lapply(prev, "[[", "prev") %>%
  bind_rows(.id = "kp")

pse_res <- lapply(pse, "[[", "pse") %>%
  bind_rows(.id = "kp")

art_res <- lapply(art, "[[", "art") %>%
  bind_rows(.id = "kp")

prev_fixed <- lapply(prev, "[[", "prev_fixed") %>%
  bind_rows(.id = "kp")

pse_fixed <- lapply(pse, "[[", "pse_fixed") %>%
  bind_rows(.id = "kp")

pop <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

pop <- lapply(file.path("archive/aaa_scale_pop", pop, "interpolated_population.csv"), read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  filter(year == 2020,
         str_length(area_id) == 3) %>%
  five_year_to_15to49("population")

pop <- pop %>%
  bind_rows(
    pop %>%
      group_by(area_id, year, age_group) %>%
      summarise(population = sum(population)) %>%
      mutate(sex = "both")
  ) %>%
  mutate(iso3 = area_id)

unaids_num <- bind_rows(
  read.csv("~/Downloads/People living with HIV_People living with HIV - Adults (15-49)_Population All adults (15-49).csv") %>%
    mutate(indicator = "plhiv"),
  read.csv("~/Downloads/Treatment cascade_People living with HIV receiving ART (#)_Population Adults (15+).csv") %>%
    mutate(indicator = "art")
) %>%
  select(!contains(c("lower", "upper", "Footnote"))) %>%
  mutate(iso3 = countrycode::countrycode(Country, "country.name", "iso3c")) %>%
  filter(iso3 %in% ssa_iso3) %>%
  select(-Country) %>%
  pivot_longer(-c(iso3, indicator)) %>%
  mutate(year = str_remove(name, "X"),
         value = str_remove_all(value, " ")) %>%
  group_by(iso3, indicator) %>%
  fill(value, .direction = "down") %>%
  filter(year == "2020", !(iso3 == "ZAF" & indicator == "art")) %>%
  type_convert() %>%
  select(-name)

kplhiv_art <- kplhiv_art %>%
  left_join(pop %>%
              mutate(kp = case_when(
                sex == "female" ~ "FSW",
                sex == "male" ~ "MSM",
                sex == "both" ~ "PWID"
              )) %>%
              select(iso3, kp, population)) %>%
  mutate(across(lower:upper, ~.x*population))

remaining_num <- kplhiv_art %>%
  group_by(iso3, indicator) %>%
  summarise(median = sum(median)) %>%
  left_join(unaids_num %>% mutate(indicator = ifelse(indicator == "plhiv", "KPLHIV", "KPART"))) %>%
  mutate(median = value - median,
         kp = "Remainder") %>%
  select(iso3, kp, indicator, median)

kplhiv_art %>%
  bind_rows(remaining_num) %>%
  filter(iso3 != "ZAF") %>%
  ggplot(aes(x=iso3, y=median, fill=fct_rev(kp))) +
  geom_col(position = "fill") +
  standard_theme() +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = 
                      c(
                        wesanderson::wes_palette("Zissou1")[1],
                        wesanderson::wes_palette("Moonrise2")[2],
                        wesanderson::wes_palette("Zissou1")[4],
                        wesanderson::wes_palette("Rushmore1")[3]
                        
                      )) +
  labs(x=element_blank(), y="Proportion of total PLHIV", fill=element_blank()) +
  facet_wrap(~indicator)

kplhiv_art %>%
  ggplot(aes(x=iso3, y=median, group=kp, fill=kp)) +
  geom_col(position = position_dodge(.9)) +
  geom_linerange(aes(ymin=lower, ymax = upper), position=position_dodge(.9)) +
  scale_y_log10(labels = scales::label_number()) +
  standard_theme() +
  labs(x=element_blank(), y=element_blank()) +
  facet_wrap(~indicator)


write_csv(prev_fixed, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_fixed.csv")

write_csv(prev_res %>%
  filter(is.na(iso3)), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_estimates.csv")

write_csv(prev_res %>%
  filter(!is.na(iso3)) %>%
  select(iso3, kp, fit), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_national_matched_estimates.csv")

######

write_csv(pse_fixed, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_fixed.csv")

write_csv(pse_res, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_estimates.csv")

#####

write_csv(art_res %>%
            filter(is.na(iso3)), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_estimates.csv")

write_csv(art_res %>%
            filter(!is.na(iso3)) %>%
            select(iso3, kp, fit), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_national_matched_estimates.csv")



p1 <- prev_res %>%
  filter(is.na(iso3)) %>%
  ggplot(aes(x=logit_gen_prev, y=logit_fit)) +
  # geom_line(size=1) +
  # geom_ribbon(aes(ymin = logit_lower, ymax = logit_upper), alpha=0.3) +
  geom_line(aes(color=region), size=1) +
  geom_ribbon(aes(ymin = logit_lower, ymax = logit_upper, fill=region), alpha=0.3) +
  geom_point(data = prev_df %>% filter(kp %in% c("MSM", "PWID", "FSW", "TG")), aes(y=logit_kp_prev, color=region), alpha = 0.3) +
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  moz.utils::standard_theme() +
  scale_y_continuous(labels = convert_logis_labels) +
  scale_x_continuous(labels = convert_logis_labels) +
  labs(y = "KP HIV prevalence", x = "Age/sex matched total\npopulation HIV prevalence")+
  theme(panel.border = element_rect(fill=NA, color="black")) +
  facet_wrap(~kp, ncol=1)

p2 <- prev_res %>%
  bind_rows() %>%
  filter(is.na(iso3)) %>%
  ggplot(aes(x=provincial_value, y=fit)) +
  # geom_line(size=1) +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.3) +
  geom_line(aes(color=region), size=1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill=region), alpha=0.3) +
  geom_point(data = prev_df %>% filter(kp %in% c("MSM", "PWID", "FSW", "TG")), aes(y=value, color=region), alpha = 0.3) +
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  moz.utils::standard_theme() +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0,0.5)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  labs(y = "KP HIV prevalence", x = "Age/sex matched total\npopulation HIV prevalence")+
  theme(panel.border = element_rect(fill=NA, color="black")) +
  facet_wrap(~kp, ncol=1)

png(file="~/Dropbox/Work Streams/2021/Key populations/Paper/Data consolidation paper/Figs/Prevalence/prev_results.png", width=700, height=850)
ggpubr::ggarrange(p1, p2, nrow=1, common.legend = TRUE, legend = "bottom")
dev.off()

# prev_tg_msm <- prev_df %>%
#   filter(kp %in% c("MSM", "TG")) %>%
#   group_by(ref) %>%
#   mutate(id.ref = cur_group_id(),
#          id.ref = ifelse(is.na(ref), NA, id.ref)) %>%
#   ungroup()
# 
# prev_inla <- df_logit %>%
#   filter(is.na(kp)) %>%
#   mutate(kp = "MSM") %>%
#   bind_rows(df_logit %>%
#               filter(is.na(kp)) %>%
#               mutate(kp = "TG"),
#             df_logit %>%
#               filter(kp %in% c("MSM", "TG"))
#             ) %>%
#   mutate(denominator = 1) %>%
#   bind_rows(prev_tg_msm %>%
#               ungroup) %>%
#   select(iso3, logit_gen_prev, kp, method, positive, negative, region, denominator, id.ref)
# 
# prev_formula <- positive ~ logit_gen_prev + kp + logit_gen_prev*kp + method + f(id.ref, model = "iid")
# 
# prev_fit <- INLA::inla(prev_formula,
#                        data = prev_inla,
#                        family = "betabinomial", 
#                        Ntrials = prev_inla$denominator,
#                        # offset = log(denominator),
#                        control.compute = list(config = TRUE),
#                        control.predictor=list(compute=TRUE),
#                        verbose = FALSE)
# 
# fitted_val <- get_mod_results_test(prev_fit, prev_inla, "positive")
# 
# out <- list()
# 
# out$res <- 
#   # res %>%
#   bind_rows(
#     fitted_val %>%
#       rename(logit_fit = median,
#              logit_lower = lower,
#              logit_upper = upper) %>%
#       mutate(
#         lower = invlogit(logit_lower),
#         upper = invlogit(logit_upper),
#         fit = invlogit(logit_fit),
#         provincial_value = invlogit(logit_gen_prev),
#         # kp = kp_id,
#         model = "betabinomial")
#   )
# 
# out$fixed <- data.frame(prev_fit$summary.fixed) %>%
#   rownames_to_column()
# 
# exp(0.337)
# 
# summary(prev_fit)
