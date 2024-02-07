library(INLA)
library(tidyverse)
library(countrycode)
library(sf)
library(lme4)
library(spud)
library(naomi)
library(moz.utils)

region <- moz.utils::region()
ssa_iso3 <- ssa_iso3()
grey <- read_sf(grey_areas())
geographies <- read_sf(national_areas()) %>%
  mutate(iso3 = area_id) %>%
  arrange(iso3) %>%
  group_by(iso3) %>%
  mutate(id.iso3 = cur_group_id())

ethic <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1murnx2dH0W_94gFFuJ-oSW0ugdmv8WPinmog0_V-0FI/edit#gid=1735078680")

recruitment <- distinct(ethic, study_idx, kp, recruitment) %>%
  mutate(recruitment = ifelse(recruitment == "RDS", "RDS", "Not RDS"))

areas <- read_sf("2023_admin1_areas.geojson")
long_nd <- read_csv("2023_10_24_deduplicated_long_nd.csv", show_col_types = F) %>%
  filter(sex != "both") %>% 
  select(area_level:mean) %>%
  mutate(indicator = case_when(
    indicator == "HIV prevalence" ~ "prevalence",
    indicator == "ART coverage" ~ "art_coverage",
    indicator == "Population" ~ "population",
    TRUE ~ indicator
  ))

invlogit <- function(x) {exp(x)/(1+exp(x))}
logit <- function(x) {log(x/(1-x))}

convert_logis_labels <- function(x) {
  paste0(round(plogis(x)*100, 1), "%")
}

adjusted_art <- read_csv("adjusted_art.csv", show_col_types = F)

adjusted_art_new <- adjusted_art %>%
  select(iso3, area_id, est_male_cov_prov, est_female_cov_prov) %>%
  pivot_longer(-c(iso3, area_id), names_to = "sex", values_to = "mean") %>%
  mutate(indicator = "art_coverage",
         sex = str_remove_all(sex, "est_|_cov_prov|"),
         age_group = "Y015_049")

adjusted_prevalence <- adjusted_art %>%
  select(iso3, area_id, male_hivpop, female_hivpop) %>%
  pivot_longer(-c(iso3, area_id), names_to = "sex", values_to = "mean") %>%
  mutate(indicator = "hivpop") %>%
  mutate(sex = str_remove_all(sex, "_hivpop")) %>%
  left_join(long_nd %>% filter(indicator == "population", age_group_label == "15-49") %>% select(iso3, area_id, sex, pop = mean)) %>%
  mutate(prevalence = mean/pop) %>%
  select(iso3, area_id, sex, mean = prevalence) %>%
  mutate(indicator = "prevalence",
         age_group = "Y015_049")

#### SSD and ERI missing 15-29
long_nd <- long_nd %>%
  filter(!(iso3 %in% adjusted_art$iso3 & indicator %in% c("prevalence", "art_coverage"))) %>%
  bind_rows(long_nd %>%
              filter((iso3 %in% adjusted_art$iso3 & indicator %in% c("prevalence", "art_coverage"))) %>%
              left_join(
                bind_rows(adjusted_art_new, adjusted_prevalence) %>% rename(new_val = mean)
              ) %>%
              mutate(ratio = new_val/mean) %>%
              fill(ratio, .direction = "down") %>%
              mutate(new_val = ifelse(is.na(new_val), ratio * mean, new_val)) %>%
              select(area_level:age_group, mean = new_val)) %>%
  arrange(iso3, area_id)

# spec_id <- lapply(ssa_iso3, function(x){
#   orderly::orderly_search(name = "aaa_data_pjnz", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022)'), draft = FALSE)
# })
# 
# spec_dat <- lapply(paste0("archive/aaa_data_pjnz/", spec_id, "/naomi_pjnz.zip"), extract_pjnz_naomi)
# 
# spec_dat <- spec_dat %>%
#   bind_rows() %>%
#   filter(year == 2021,
#          age %in% 15:49)


spec_dat <- read_csv("2023_spec_dat.csv", show_col_types = F) %>%
  group_by(iso3, sex) %>%
  summarise(prevalence = sum(hivpop)/sum(totpop),
            art_coverage = sum(artpop)/sum(hivpop),
            artpop = sum(artpop),
            hivpop = sum(hivpop),
            totpop = sum(totpop)) %>%
  pivot_longer(-c(iso3, sex), names_to = "indicator", values_to = "mean")

nat_adjusted <- adjusted_art %>%
  group_by(iso3) %>%
  summarise(female_hivpop = sum(female_hivpop),
            male_hivpop = sum(male_hivpop),
            female_art_cov = unique(est_female_cov_nat),
            male_art_cov = unique(est_male_cov_nat),
            ) %>%
  pivot_longer(-iso3, names_sep = "_", names_to = c("sex", "indicator"))

nat_adjusted <- nat_adjusted %>%
  filter(indicator == "hivpop") %>%
  select(-indicator) %>%
  left_join(spec_dat %>% filter(indicator == "totpop")) %>%
  mutate(mean = value/mean,
         indicator = "prevalence",
         age_group = "Y015_049") %>%
  select(-value) %>%
  bind_rows(
    nat_adjusted %>%
      filter(indicator == "art") %>%
      mutate(indicator = "art_coverage",
             age_group = "Y015_049") %>%
      rename(mean = value)
  )

spec_dat <- spec_dat %>%
  filter(!(iso3 %in% adjusted_art$iso3 & indicator %in% c("art_coverage", "prevalence"))) %>%
  bind_rows(nat_adjusted %>% select(-age_group)) %>%
  arrange(iso3)

national_matched_genpop <- data.frame(kp = c("FSW", "MSM", "PWID", "TGW"),
                                      sex = c("female", "male", "male", "female"),
                                      # age_group = c("Y015_049", "Y015_029", "Y015_049", "Y015_029")
                                      age_group = "Y015_049"
) %>%
  left_join(
    spec_dat %>% filter(indicator %in% c("prevalence", "art_coverage"))
  )

genpop_pred <- data.frame(kp = c("FSW", "MSM", "PWID", "TGW"),
                          sex = c("female", "male", "male", "female"),
                          # age_group = c("Y015_049", "Y015_029", "Y015_049", "Y015_029")
                          age_group = "Y015_049"
) %>%
  left_join(
    long_nd %>%
      filter(indicator %in% c("prevalence", "art_coverage")),
    multiple = "all"
  ) %>%
  mutate(
    mean = ifelse(mean > 1, 0.99, mean),
    logit_gen_var = logit(mean)) %>%
  left_join(region)

genpop_pred <- genpop_pred %>%
  select(region, iso3, kp, area_id, indicator, logit_gen_var)


## This will eventually read in from the data sharing workbook but I've found some bugs in it so for now it pulls from the sharepoint versions

# dat <- readxl::read_excel("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Aggregate data/Data sharing/2023-06-28_Le Bao/dataset.xlsx",
#                           sheet = "Data",
#                           col_types = c("numeric","text","text","text","numeric","text","text","text","text","text","text","text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")
#                           )
# 
# dat <- dat %>%
#   filter(!is.na(matched_province_area_id)) %>%
#   select(study_idx, iso3, kp, indicator, method, area_id = matched_province_area_id, proportion_estimate, denominator = sample_size, provincial_value)




###### HIV prevalence #######

prev_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_final_sourced.csv", show_col_types = F) %>%
  rename(area_id = model_matched_provincial_area_id) %>%
  filter(study_idx != 1001,## No record of this study from GNB
         method == "lab") %>% 
  left_join(recruitment %>% mutate(kp = ifelse(kp == "TG", "TGW", kp))) %>%
  mutate(recruitment = case_when(
    study_idx == 74 ~ "RDS",
    study_idx == 314 ~ "RDS",
    study_idx == 3 ~ "Not RDS",
    TRUE ~ recruitment
  ),
  denominator = ifelse(recruitment == "RDS", denominator/2, denominator))

imp_denomin_prev <- prev_dat %>%
  filter(!is.na(denominator),
         denominator != 0) %>%
  group_by(kp, recruitment) %>%
  summarise(quant = quantile(denominator, 0.25))


prev_dat <- bind_rows(prev_dat %>%
            filter(!area_id %in% adjusted_prevalence$area_id),
          prev_dat %>% 
            filter(area_id %in% adjusted_prevalence$area_id) %>%
            select(-c(provincial_value, ratio)) %>%
            left_join(adjusted_prevalence %>%
                        left_join(kp_to_sex() %>% mutate(sex = ifelse(kp == "PWID", "male", sex))) %>%
                        select(iso3, area_id, kp, provincial_value = mean)) %>%
            mutate(ratio = value/provincial_value)
          )

prev_dat %>% filter(is.na(denominator) | denominator == 0) %>% count(recruitment)

prev_df <- prev_dat %>%
  left_join(region %>% select(region, iso3)) %>%
  rename(proportion_estimate = value) %>%
  mutate(denominator = case_when(
    (is.na(denominator) | denominator == 0) & kp == "FSW" & recruitment == "RDS" ~ filter(imp_denomin_prev, kp == "FSW" & recruitment == "RDS")$quant,
    (is.na(denominator) | denominator == 0) & kp == "MSM" & recruitment == "RDS" ~ filter(imp_denomin_prev, kp == "MSM" & recruitment == "RDS")$quant,
    (is.na(denominator) | denominator == 0) & kp == "PWID" & recruitment == "RDS" ~ filter(imp_denomin_prev, kp == "PWID" & recruitment == "RDS")$quant,
    (is.na(denominator) | denominator == 0) & kp == "TGW" & recruitment == "RDS" ~ filter(imp_denomin_prev, kp == "TGW" & recruitment == "RDS")$quant,
    
    (is.na(denominator) | denominator == 0) & kp == "FSW" & recruitment == "Not RDS" ~ filter(imp_denomin_prev, kp == "FSW" & recruitment == "Not RDS")$quant,
    (is.na(denominator) | denominator == 0) & kp == "MSM" & recruitment == "Not RDS" ~ filter(imp_denomin_prev, kp == "MSM" & recruitment == "Not RDS")$quant,
    (is.na(denominator) | denominator == 0) & kp == "PWID" & recruitment == "Not RDS" ~ filter(imp_denomin_prev, kp == "PWID" & recruitment == "Not RDS")$quant,
    (is.na(denominator) | denominator == 0) & kp == "TGW" & recruitment == "Not RDS" ~ filter(imp_denomin_prev, kp == "TGW" & recruitment == "Not RDS")$quant,
    TRUE ~ denominator
  )) %>%
  filter(!is.na(provincial_value),
         proportion_estimate<1) %>%
  ungroup %>%
  mutate(proportion_estimate = ifelse(proportion_estimate == 1, 0.99, proportion_estimate),
         proportion_estimate = ifelse(proportion_estimate ==0, 0.001, proportion_estimate),
         logit_kp_prev = logit(proportion_estimate),
         logit_gen_prev = logit(provincial_value),
         logit_gen_prev2 = logit_gen_prev,
         positive = round(proportion_estimate * denominator),
         negative = round(denominator - positive),
         method = factor(method, levels = c("lab", "selfreport"))
  )

df_logit_prev <- data.frame(logit_gen_prev = logit(seq(0.001, 0.07, 0.005)),
                            region = "WCA") %>%
  bind_rows(
    data.frame(logit_gen_prev = logit(seq(0.006, 0.4, 0.005)),
               region = "ESA"),
    genpop_pred %>%
      filter(indicator == "prevalence") %>%
      rename(logit_gen_prev = logit_gen_var) %>%
      left_join(areas %>% select(area_id, id.area) %>% st_drop_geometry()) %>%
      arrange(id.area)
  ) %>%
  left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry())

ref.iid.prec.prior <- list(prec= list(prior = "normal", param = c(1.6, 4)))
iso.iid.prec.prior <- list(prec= list(prior = "normal", param = c(3, 1)))
spatial.prec.prior <- list(prec= list(prior = "normal", param = c(-0.75, 6.25)))

prec.prior <- list(prec= list(prior = "normal", param = c(2,1)))


fit_prevalence_model <- function(kp_id) {
  
  out <- list()
  
  int <- prev_df %>%
    ungroup() %>%
    mutate(method = ifelse(is.na(method), "selfreport", as.character(method))) %>%
    filter(kp == kp_id) %>%
    group_by(study_idx) %>%
    mutate(id.ref = cur_group_id()) %>%
    ungroup() %>%
    filter(!is.na(study_idx)) %>%
    mutate(obs_iid = row_number()) %>%
    left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
    left_join(areas %>% select(area_id, id.area) %>% st_drop_geometry())
  
  prev_inla <- df_logit_prev %>% 
    filter(is.na(kp) | kp == kp_id) %>%
    mutate(denominator = 1) %>%
    # id.iso3 = ifelse(is.na(iso3), nrow(geographies) + 1, id.iso3),
    # id.area = ifelse(is.na(area_id), nrow(areas) + 1, id.area)) %>%
    bind_rows(int) %>%
    mutate(region = factor(region)) %>%
    ungroup() %>%
    select(kp, iso3, area_id, logit_gen_prev, logit_kp_prev, obs_iid, method, positive, negative, region, denominator, id.ref, id.iso3, id.area) %>%
    mutate(idx = row_number())
  
  nat_level_obs <- prev_inla %>%
    filter(area_id == iso3 & !is.na(positive)) %>%
    ungroup() %>%
    group_by(id.ref) %>%
    mutate(id.ref.nat = cur_group_id(),
           id.ref = NA)
  
  prev_inla <- prev_inla %>%
    filter(!idx %in% nat_level_obs$idx) %>%
    bind_rows(nat_level_obs)
  
  # prev_formula <- positive ~ logit_gen_prev + method + f(id.ref, model = "iid") + f(id.iso3, model = "iid", hyper = iso.iid.prec.prior)
  prev_formula <- positive ~ 
    logit_gen_prev + 
    region*logit_gen_prev +
    # method + 
    f(id.iso3,
      model = "besag",
      scale.model = TRUE,
      graph = national_adj(),
      hyper = prec.prior
    ) +
    f(id.area, model = "besag",
      scale.model = TRUE,
      graph = "admin1_level_adj_gnq_link.adj",
      hyper = prec.prior
    ) +
    f(id.ref, model = "iid") +
    f(id.ref.nat,
      model = "iid",
      hyper = prec.prior
    )
  
  prev_fit <- INLA::inla(prev_formula,
                         data = prev_inla,
                         family = "betabinomial", 
                         Ntrials = prev_inla$denominator,
                         # offset = log(denominator),
                         control.compute = list(config = TRUE,
                                                dic = TRUE),
                         control.predictor=list(compute=TRUE),
                         verbose = FALSE)
  
  message(paste0(kp_id, " | ", prev_fit$dic$dic))
  
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
  qtls <- apply(prev_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
  ident <- prev_inla[ind.effect, ]
  prev_samples <- ident %>% cbind(prev_samples)
  
  prev <- ident %>%
    ungroup() %>%
    mutate(
      lower = qtls[1,],
      median = qtls[2,],
      upper = qtls[3,],
      indicator = "prevalence"
    )
  
  ind.observation <- (nrow(prev_inla) - nrow(df) + 1):nrow(prev_inla)
  samples.observation = lapply(samples, function(x) x$latent[ind.observation])
  observation_samples <- matrix(sapply(samples.observation, cbind), ncol=1000)
  obs_ident <- prev_inla[ind.observation, ]
  obs_pred <- apply(observation_samples, 1, median)
  r2_df <- obs_ident %>%
    select(kp, region, logit_gen_prev, logit_kp_prev) %>%
    mutate(median = obs_pred)
  
  
  prev_fixed <- data.frame(prev_fit$summary.fixed) %>%
    rownames_to_column() %>%
    mutate(kp = kp_id)
  
  out <- list()
  out$prev_samples <- prev_samples
  out$prev <- prev
  out$prev_fixed <- prev_fixed
  out$prev_random <- prev_fit$summary.hyperpar
  out$r2_df <- r2_df
  out
  
}
fit_msm_tg_prevalence_model <- function() {
  
  out <- list()
  
  int <- prev_df %>%
    ungroup() %>%
    mutate(method = ifelse(is.na(method), "selfreport", as.character(method))) %>%
    filter(kp %in% c("MSM", "TGW")) %>%
    group_by(study_idx) %>%
    mutate(id.ref = cur_group_id()) %>%
    ungroup() %>%
    filter(!is.na(study_idx)) %>%
    mutate(obs_iid = row_number()) %>%
    left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
    left_join(areas %>% select(area_id, id.area) %>% st_drop_geometry())
  
  prev_inla <- 
    bind_rows(
      df_logit_prev %>% 
        filter(is.na(kp)) %>% 
        mutate(kp = "MSM"),
      df_logit_prev %>% 
        filter(is.na(kp)) %>% 
        mutate(kp = "TGW"),
      df_logit_prev %>% filter(kp %in% c("TGW", "MSM")) %>% arrange(kp, iso3)
    ) %>%
    mutate(denominator = 1) %>%
    bind_rows(int) %>%
    mutate(region = factor(region)) %>%
    # mutate(id.iso3 = ifelse(is.na(iso3), 39, id.iso3),
    #        id.area = ifelse(is.na(area_id), 578, id.area)) %>%
    ungroup() %>%
    select(iso3, area_id, kp, logit_gen_prev, logit_kp_prev, method, positive, negative, region, denominator, id.ref, id.iso3, id.area) %>%
    mutate(idx = row_number())
  
  nat_level_obs <- prev_inla %>%
    filter(area_id == iso3 & !is.na(positive)) %>%
    ungroup() %>%
    group_by(id.ref) %>%
    mutate(id.ref.nat = cur_group_id(),
           id.ref = NA)
  
  prev_inla <- prev_inla %>%
    filter(!idx %in% nat_level_obs$idx) %>%
    bind_rows(nat_level_obs)
  
  # prev_formula <- positive ~ logit_gen_prev + method + f(id.ref, model = "iid") + f(id.iso3, model = "iid", hyper = iso.iid.prec.prior)
  prev_formula <- positive ~ 
    logit_gen_prev + 
    # region + 
    region*logit_gen_prev +
    kp*region +
    kp*logit_gen_prev +
    # method + 
    f(id.iso3,
      model = "besag",
      scale.model = TRUE,
      graph = national_adj(),
      hyper = prec.prior
    ) +
    f(id.area, model = "besag",
      scale.model = TRUE,
      graph = "admin1_level_adj_gnq_link.adj",
      hyper = prec.prior
    ) +
    f(id.ref, model = "iid") +
    f(id.ref.nat,
      model = "iid",
      hyper = prec.prior
    )
  
  prev_fit <- INLA::inla(prev_formula,
                         data = prev_inla,
                         family = "betabinomial", 
                         Ntrials = prev_inla$denominator,
                         # offset = log(denominator),
                         control.compute = list(config = TRUE,
                                                dic = TRUE),
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
  qtls <- apply(prev_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
  ident <- prev_inla[ind.effect, ] %>%
    ungroup() %>%
    mutate(idx = row_number())
  
  prev_samples <- ident %>% cbind(prev_samples)
  
  prev <- ident %>%
    ungroup() %>%
    mutate(
      lower = qtls[1,],
      median = qtls[2,],
      upper = qtls[3,],
      indicator = "prevalence"
    )
  
  ind.observation <- (nrow(prev_inla) - nrow(df) + 1):nrow(prev_inla)
  samples.observation = lapply(samples, function(x) x$latent[ind.observation])
  observation_samples <- matrix(sapply(samples.observation, cbind), ncol=1000)
  obs_ident <- prev_inla[ind.observation, ]
  obs_pred <- apply(observation_samples, 1, median)
  r2_df <- obs_ident %>%
    select(kp, region, logit_gen_prev, logit_kp_prev) %>%
    mutate(median = obs_pred)
  
  prev_fixed <- data.frame(prev_fit$summary.fixed) %>%
    rownames_to_column() %>%
    mutate(kp = "MSM")
  
  out <- list()
  out$prev_samples <- prev_samples
  out$prev <- prev
  out$prev_fixed <- prev_fixed
  out$prev_random <- prev_fit$summary.hyperpar
  out$r2_df <- r2_df
  out
  
}

# debugonce(fit_prevalence_model)
# fit_prevalence_model("FSW")

prev_mod <- lapply(c("FSW", "PWID"), fit_prevalence_model)
names(prev_mod) <- c("FSW", "PWID")

r2_df <- lapply(prev_mod, "[[", "r2_df") %>%
  bind_rows(.id = "kp")


prev_mod_msm_tg <- list(fit_msm_tg_prevalence_model())
names(prev_mod_msm_tg) <- "MSM-TGW"

msm_idx <- which(prev_mod_msm_tg$`MSM-TG`$prev$kp == "MSM")
tg_idx <- which(prev_mod_msm_tg$`MSM-TG`$prev$kp == "TGW")

prev_mod$MSM$prev_samples <- prev_mod_msm_tg$`MSM-TG`$prev_samples[msm_idx,]
prev_mod$MSM$prev <- prev_mod_msm_tg$`MSM-TG`$prev %>% filter(kp == "MSM")
prev_mod$MSM$prev_fixed <- prev_mod_msm_tg$`MSM-TG`$prev_fixed
prev_mod$MSM$prev_random <- prev_mod_msm_tg$`MSM-TG`$prev_random
prev_mod$MSM$r2_df <- prev_mod_msm_tg$`MSM-TG`$r2_df

prev_mod$TGW$prev_samples <- prev_mod_msm_tg$`MSM-TG`$prev_samples[tg_idx,]
prev_mod$TGW$prev <- prev_mod_msm_tg$`MSM-TG`$prev %>% filter(kp == "TGW")

prev_res <- lapply(prev_mod, "[[", "prev") %>%
  bind_rows(.id = "kp")

prev_fixed <- lapply(prev_mod, "[[", "prev_fixed") %>%
  bind_rows(.id = "kp")

prev_fixed %>%
  filter(rowname == "methodselfreport") %>%
  select(kp, rowname, mean, `X0.025quant`, `X0.975quant`)

r2_df %>%
  bind_rows(prev_mod$MSM$r2_df) %>%
  group_by(kp, region) %>%
  summarise(r2 = sqrt(1 - sum((logit_kp_prev-median)^2)/sum((logit_kp_prev-mean(logit_kp_prev))^2))) %>%
  arrange(region)

##### ART Coverage ########

art_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_final.csv", show_col_types = FALSE) %>%
  rename(area_id = model_matched_provincial_area_id) %>%
  left_join(recruitment %>% mutate(kp = ifelse(kp == "TG", "TGW", kp))) %>%
  mutate(recruitment = case_when(
    study_idx == 74 ~ "RDS",
    study_idx == 314 ~ "RDS",
    study_idx == 3 ~ "Not RDS",
    TRUE ~ recruitment
  ),
  denominator = ifelse(recruitment == "RDS", denominator/2, denominator))

imp_denomin_art <- art_dat %>%
  filter(!is.na(denominator),
         denominator != 0) %>%
  group_by(kp, recruitment) %>%
  summarise(quant = quantile(denominator, 0.25))

art_dat <- bind_rows(art_dat %>%
                        filter(!area_id %in% adjusted_art_new$area_id,
                               !area_id %in% nat_adjusted$iso3),
                      art_dat %>% 
                        filter(area_id %in% adjusted_art_new$area_id |
                                 area_id %in% nat_adjusted$iso3) %>%
                        select(-c(provincial_value, ratio)) %>%
                        left_join(bind_rows(adjusted_art_new, nat_adjusted %>% filter(indicator == "art_coverage") %>% mutate(area_id = iso3)) %>%
                                    left_join(kp_to_sex() %>% mutate(sex = ifelse(kp == "PWID", "male", sex))) %>%
                                    select(iso3, area_id, kp, provincial_value = mean)) %>%
                        mutate(ratio = value/provincial_value)
)

vls <- readxl::read_excel("vls.xlsx") %>%
  distinct(vls_threshold, study_idx)

adjust_vls <- function(vls, vls_threshold) {
  exponent <- (log10(1000)/log10(vls_threshold))^0.85
  adj_vls <- 1-(1-vls)^exponent
}

art_dat %>% 
  left_join(vls) %>% 
  mutate(method = case_when(method == "Self-report" ~ "self-report",
                            method == "vls" ~ "VLS",
                            TRUE ~ method),
         vls_threshold = ifelse(is.na(vls_threshold), 1000, vls_threshold), ## Studies 58 and 164 don't state a threshold. Assume 1000
         adj_value = ifelse(method == "VLS", adjust_vls(value, vls_threshold), NA),
  ) %>%
  filter(vls_threshold != 1000,
         method == "VLS") %>%
  select(iso3, kp, area_name, year, method, vls_threshold, value, adj_value, study_idx) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/vls_threshold.csv")

ethic %>%
  filter(study_idx %in% idx)

art_dat <- art_dat %>% 
  left_join(vls) %>% 
  mutate(method = case_when(method == "Self-report" ~ "self-report",
                            method == "vls" ~ "VLS",
                            TRUE ~ method),
         vls_threshold = ifelse(is.na(vls_threshold), 1000, vls_threshold), ## Studies 58 and 164 don't state a threshold. Assume 1000
         value = ifelse(method == "VLS", adjust_vls(value, vls_threshold), value),
         ) %>%
  filter(method != "self-report")

art_df <- art_dat %>%
  bind_rows() %>%
  rename(proportion_estimate = value) %>%
  left_join(region %>% select(region, iso3)) %>%
  mutate(denominator = case_when(
    (is.na(denominator) | denominator == 0) & kp == "FSW" & recruitment == "RDS" ~ filter(imp_denomin_art, kp == "FSW" & recruitment == "RDS")$quant,
    (is.na(denominator) | denominator == 0) & kp == "MSM" & recruitment == "RDS" ~ filter(imp_denomin_art, kp == "MSM" & recruitment == "RDS")$quant,
    (is.na(denominator) | denominator == 0) & kp == "PWID" & recruitment == "RDS" ~ filter(imp_denomin_art, kp == "PWID" & recruitment == "RDS")$quant,
    (is.na(denominator) | denominator == 0) & kp == "TGW" & recruitment == "RDS" ~ filter(imp_denomin_art, kp == "TGW" & recruitment == "RDS")$quant,
    
    (is.na(denominator) | denominator == 0) & kp == "FSW" & recruitment == "Not RDS" ~ filter(imp_denomin_art, kp == "FSW" & recruitment == "Not RDS")$quant,
    (is.na(denominator) | denominator == 0) & kp == "MSM" & recruitment == "Not RDS" ~ filter(imp_denomin_art, kp == "MSM" & recruitment == "Not RDS")$quant,
    (is.na(denominator) | denominator == 0) & kp == "PWID" & recruitment == "Not RDS" ~ filter(imp_denomin_art, kp == "PWID" & recruitment == "Not RDS")$quant,
    (is.na(denominator) | denominator == 0) & kp == "TGW" & recruitment == "Not RDS" ~ filter(imp_denomin_art, kp == "TGW" & recruitment == "Not RDS")$quant,
    TRUE ~ denominator
  )) %>%
  filter(proportion_estimate < 1,
         provincial_value < 1, ## FIX THIS
         !is.na(provincial_value),
  ) %>%
  mutate(proportion_estimate = ifelse(proportion_estimate == 0, 0.01, proportion_estimate),
         proportion_estimate = ifelse(proportion_estimate == 1, 0.99, proportion_estimate),
         logit_kp_art = logit(proportion_estimate),
         logit_gen_art = logit(provincial_value),
         logit_kp_art = ifelse(method == "VLS", logit_kp_art/0.8, logit_kp_art),
         proportion_estimate = invlogit(logit_kp_art),
         positive = round(proportion_estimate * denominator),
         negative = round(denominator - positive),
         method = ifelse(is.na(method), "self-report", as.character(method))
  ) %>%
  mutate(method = ifelse(method == "VLS", "lab", method))

df_logit_art <- data.frame(logit_gen_art = logit(seq(0.01, 0.99, 0.01)),
                           region = "WCA") %>%
  bind_rows(
    data.frame(logit_gen_art = logit(seq(0.006, 0.35, 0.005)),
               region = "ESA"),
    genpop_pred %>%
      filter(indicator == "art_coverage") %>%
      rename(logit_gen_art = logit_gen_var) %>%
      left_join(areas %>% select(area_id, id.area) %>% st_drop_geometry()) %>%
      arrange(id.area)
  ) %>%
  left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry())

art_df %>%
  mutate(period = ifelse(year < 2017, "2010-2016", "2017-2023")) %>%
  ggplot(aes(x=logit_gen_art, y=logit_kp_art, color = period)) +
    geom_point(aes(size = denominator)) +
    geom_smooth(method = "lm", aes(weight = denominator), se = F) +
    facet_wrap(~kp)

fit_art_model_all <- function() {
  
  int <- art_df %>%
    # mutate(method = ifelse(method == "VLS", "lab", method)) %>%
    ungroup() %>%
    group_by(study_idx) %>%
    mutate(id.ref = cur_group_id()) %>%
    ungroup() %>%
    filter(!is.na(study_idx)) %>%
    mutate(obs_iid = row_number()) %>%
    left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
    left_join(areas %>% select(area_id, id.area) %>% st_drop_geometry()) %>%
    droplevels()
  # mutate(method = factor(method, levels = c("VLS", "lab", "self-report")))
  
  art_inla <- crossing(
    df_logit_art %>% filter(is.na(kp)) %>% select(logit_gen_art, region),
    kp = c("FSW", "MSM", "PWID", "TGW"),
    year = 2023
  ) %>%
    bind_rows(
      df_logit_art %>% filter(!is.na(kp)) %>% arrange(kp) %>% mutate(year = 2023)
    ) %>%
    # filter(is.na(kp)) %>%
    mutate(denominator = 1) %>%
    # id.iso3 = ifelse(is.na(iso3), nrow(geographies) + 1, id.iso3),
    # id.area = ifelse(is.na(area_id), nrow(areas) + 1, id.area)) %>%
    bind_rows(int) %>%
    mutate(region = factor(region)) %>%
    ungroup() %>%
    select(iso3, area_id, kp, year, logit_gen_art, logit_kp_art, obs_iid, method, positive, negative, region, denominator, id.ref, id.iso3, id.area) %>%
    mutate(idx = row_number())
  
  
  nat_level_obs <- art_inla %>%
    filter(area_id == iso3 & !is.na(positive)) %>%
    ungroup() %>%
    group_by(id.ref) %>%
    mutate(id.ref.nat = cur_group_id(),
           id.ref = NA)
  
  art_inla <- art_inla %>%
    filter(!idx %in% nat_level_obs$idx) %>%
    bind_rows(nat_level_obs)
  
  if(length(unique(art_df$method)) > 1) {
    
    message("More than 1 ART method")
    art_formula <- positive ~ 
      logit_gen_art + 
      f(kp, hyper = prec.prior) +
      f(kp2, logit_gen_art, hyper = prec.prior) +
      method +
      f(id.iso3, model = "besag", scale.model = TRUE, graph = national_adj(), hyper = prec.prior) +
      f(id.area, model = "besag", scale.model = TRUE, graph = "admin1_level_adj_gnq_link.adj", hyper = prec.prior) +
      f(id.ref, model = "iid", hyper = prec.prior) +
      f(id.ref.nat, model = "iid", hyper = prec.prior)
    
  } else {
    message("One ART method")
    art_formula <- positive ~ 
      logit_gen_art + 
      # year +
      f(kp, hyper = prec.prior) +
      f(kp2, logit_gen_art, hyper = prec.prior) +
      # f(kp3, year, hyper = prec.prior) +
      f(id.iso3, model = "besag", scale.model = TRUE, graph = national_adj(), hyper = prec.prior) +
      f(id.area, model = "besag", scale.model = TRUE, graph = "admin1_level_adj_gnq_link.adj", hyper = prec.prior) +
      f(id.ref, model = "iid", hyper = prec.prior) +
      f(id.ref.nat, model = "iid", hyper = prec.prior)
  }
  
  art_fit <- INLA::inla(art_formula,
                        data = art_inla %>% mutate(kp2 = kp, kp3 = kp),
                        family = "betabinomial", 
                        Ntrials = art_inla$denominator,
                        # offset = log(denominator),
                        control.compute = list(config = TRUE,
                                               dic = TRUE),
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
  qtls <- apply(art_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
  ident <- art_inla[ind.effect, ]
  art_samples <- ident %>% cbind(art_samples)
  
  art <- ident %>%
    mutate(
      lower = qtls[1,],
      median = qtls[2,],
      upper = qtls[3,],
      indicator = "art"
    )
  
  ind.observation <- (nrow(art_inla) - nrow(df) + 1):nrow(art_inla)
  samples.observation = lapply(samples, function(x) x$latent[ind.observation])
  observation_samples <- matrix(sapply(samples.observation, cbind), ncol=1000)
  obs_ident <- art_inla[ind.observation, ]
  obs_pred <- apply(observation_samples, 1, median)
  r2_df <- obs_ident %>%
    select(kp, logit_gen_art, logit_kp_art) %>%
    mutate(median = obs_pred)
  
  out <- list()
  out$art <- art
  out$art_samples <- art_samples
  out$art_fixed <- art_fit$summary.fixed
  out$art_hyperpar <- art_fit$summary.hyperpar
  out$art_random <- art_fit$summary.random
  out$r2_df <- r2_df
  out
  
}


fit_art_model_one <- function(kp_id) {

  int <- art_df %>%
    # mutate(method = ifelse(method == "VLS", "lab", method)) %>%
    ungroup() %>%
    group_by(study_idx) %>%
    mutate(id.ref = cur_group_id()) %>%
    ungroup() %>%
    filter(!is.na(study_idx)) %>%
    mutate(obs_iid = row_number()) %>%
    left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
    left_join(areas %>% select(area_id, id.area) %>% st_drop_geometry()) %>%
    droplevels()
  # mutate(method = factor(method, levels = c("VLS", "lab", "self-report")))

  art_inla <- crossing(
    df_logit_art %>%
      filter(is.na(kp)) %>%
      select(kp, logit_gen_art, region),
    year = c(2015, 2020, 2023)
  ) %>%
    # bind_rows(
    #   df_logit_art %>% filter(is.na(kp) | kp == kp_id) %>% arrange(kp) %>% mutate(year = 2023)
    # ) %>%
    # filter(is.na(kp)) %>%
    mutate(denominator = 1) %>%
    # id.iso3 = ifelse(is.na(iso3), nrow(geographies) + 1, id.iso3),
    # id.area = ifelse(is.na(area_id), nrow(areas) + 1, id.area)) %>%
    bind_rows(int %>% filter(kp == kp_id)) %>%
    mutate(region = factor(region)) %>%
    ungroup() %>%
    select(iso3, area_id, kp, year, logit_gen_art, logit_kp_art, obs_iid, method, positive, negative, region, denominator, id.ref, id.iso3, id.area) %>%
    mutate(idx = row_number())


  nat_level_obs <- art_inla %>%
    filter(area_id == iso3 & !is.na(positive)) %>%
    ungroup() %>%
    group_by(id.ref) %>%
    mutate(id.ref.nat = cur_group_id(),
           id.ref = NA)

  art_inla <- art_inla %>%
    filter(!idx %in% nat_level_obs$idx) %>%
    bind_rows(nat_level_obs)

  if(length(unique(art_df$method)) > 1) {

    message("More than 1 ART method")
    art_formula <- positive ~
      logit_gen_art +
      # f(kp, hyper = prec.prior) +
      # f(kp2, logit_gen_art, hyper = prec.prior) +
      method +
      f(id.iso3, model = "besag", scale.model = TRUE, graph = national_adj(), hyper = prec.prior) +
      f(id.area, model = "besag", scale.model = TRUE, graph = "admin1_level_adj_gnq_link.adj", hyper = prec.prior) +
      f(id.ref, model = "iid", hyper = prec.prior) +
      f(id.ref.nat, model = "iid", hyper = prec.prior)

  } else {
    message("One ART method")
    art_formula <- positive ~ logit_gen_art + bs(year, df = 4)
      # f(year, model = 'linear', mean.linear = 0, prec.linear = 1) + f(year2, model = "rw2", scale.model = T, hyper = prec.prior)
      # logit_gen_art +
      # year
      # f(kp, hyper = prec.prior) +
      # f(kp2, logit_gen_art, hyper = prec.prior) +
      # f(kp3, year, hyper = prec.prior) +
      # f(id.iso3, model = "besag", scale.model = TRUE, graph = national_adj(), hyper = prec.prior) +
      # f(id.area, model = "besag", scale.model = TRUE, graph = "admin1_level_adj_gnq_link.adj", hyper = prec.prior) +
      # f(id.ref, model = "iid", hyper = prec.prior)
      # f(id.ref.nat, model = "iid", hyper = prec.prior)
  }

  # formula <- logit_kp_art ~ logit_gen_art
  #
  # art_fit <- INLA::inla(formula,
  #                       data = art_inla,
  #                       family = "gaussian",
  #                       # Ntrials = art_inla$denominator,
  #                       # offset = log(denominator),
  #                       weights = denominator,
  #                       control.compute = list(config = TRUE,
  #                                              dic = TRUE),
  #                       control.predictor=list(compute=TRUE),
  #                       verbose = FALSE)


  art_fit <- INLA::inla(art_formula,
                        data = art_inla %>% mutate(year2 = year),
                        family = "betabinomial",
                        Ntrials = art_inla$denominator,
                        # offset = log(denominator),
                        offset = logit_gen_art,
                        control.compute = list(config = TRUE,
                                               dic = TRUE),
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
  qtls <- apply(art_samples, 1, quantile, c(0.025, 0.5, 0.975))

  ident <- art_inla[ind.effect, ]
  art_samples <- ident %>% cbind(art_samples)

  art <- ident %>%
    mutate(
      lower = qtls[1,],
      median = qtls[2,],
      upper = qtls[3,],
      indicator = "art"
    )

  ind.observation <- (nrow(art_inla) - nrow(df) + 1):nrow(art_inla)
  samples.observation = lapply(samples, function(x) x$latent[ind.observation])
  observation_samples <- matrix(sapply(samples.observation, cbind), ncol=1000)
  obs_ident <- art_inla[ind.observation, ]
  obs_pred <- apply(observation_samples, 1, median)
  r2_df <- obs_ident %>%
    select(kp, logit_gen_art, logit_kp_art) %>%
    mutate(median = obs_pred)

  out <- list()
  out$art <- art
  out$art_samples <- art_samples
  out$art_fixed <- art_fit$summary.fixed
  out$art_hyperpar <- art_fit$summary.hyperpar
  out$art_random <- art_fit$summary.random
  out$r2_df <- r2_df
  out

}

art_mod <- fit_art_model_all()

art_mod_default$art %>%
  mutate(source = "new") %>%
  bind_rows(existing$art_continuous_res %>% lapply(data.frame) %>% bind_rows() %>% mutate(source = "old")) %>%
  filter(is.na(area_id),
         logit_gen_art > -2) %>%
  ggplot(aes(x=logit_gen_art, y=median)) +
    geom_line(aes(color = source)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.3) +
    geom_point(data = art_df %>% filter(logit_gen_art > -2), aes(y=logit_kp_art, size = denominator)) +
    facet_wrap(~kp)
  
art_mod_year$art_random

art_mod_default <- art_mod

art %>%
  filter(is.na(area_id),
         logit_gen_art > -3,
         logit_gen_art < 5) %>%
  ggplot(aes(x=logit_gen_art)) +
    geom_line(aes(y=median, group = factor(year))) +
    geom_point(data = art_df %>% filter(kp == "FSW"), aes(y=logit_kp_art, size = denominator, color=year))

art_fsw <- fit_art_model_one("FSW")

int %>% 
  count(kp, year) %>%
  ggplot(aes(x=year, y=n)) +
    stat_ecdf() +
    facet_wrap(~kp)

int %>%
  mutate(period = ifelse(year < 2019, "2010-2018", "2019-2023")) %>%
  ggplot(aes(x=logit_gen_art, y=logit_kp_art, color = period)) +
    geom_point() +
    geom_smooth(method = "lm")

######## PSE Model ##########

pse_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final_sourced.csv", show_col_types = F)

pse_dat <- mutate(pse_dat,
                  study_idx = ifelse(study_idx == 191, 190, study_idx))

pse_dat <- pse_dat %>%
  rename(proportion_estimate = prop_estimate) %>%
  mutate(
    fe_method = case_when(
      str_detect(method, "methods") ~ "other methods",
      method %in% c("PLACE", "mapping", "PLACE/Mapping") ~ "PLACE/Mapping",
      # method == "PLACE" ~ "PLACE",
      # str_detect(method, "apping") ~ "Mapping",
      TRUE ~ "empirical"
    )
  )

bounds <- read_csv("pse_unc2.csv", show_col_types = F)
D <- 2

bounds <- bounds %>%
  filter(!is.na(c1)) %>%
  mutate(c3 = ifelse(is.na(c3), round(c3_prop * c2), c3),
         c3 = ifelse(is.na(c3), round((c1*c2)/count_estimate), c3),
         c3_prop = ifelse(is.na(c3_prop), c3/c2, c3_prop)) %>%
  rename(phat = c3_prop) %>%
  filter(!is.na(c1),
         !is.na(c2),
         !is.na(phat),
         phat < 1
  ) %>%
  rowwise() %>%
  mutate(ci = list(prop.test(phat * c2/D, c2/D, correct = F)$conf.int),
         est = c1/phat,
         lower = c1/ci[2],
         upper = c1/ci[1]) %>%
  select(-ci)

pse_dat <- bind_rows(
  pse_dat %>% filter(!observation_idx %in% bounds$observation_idx),
  pse_dat %>%
    filter(observation_idx %in% bounds$observation_idx) %>%
    select(-c(count_lower:count_upper)) %>%
    left_join(bounds %>%
                select(observation_idx, count_lower = lower, count_estimate = est, count_upper = upper)) %>%
    mutate(prop_lower = count_lower/population,
           proportion_estimate = count_estimate/population,
           prop_upper = count_upper/population)
    )

pse_dat <- pse_dat %>%
  filter(proportion_estimate != 0, !is.na(proportion_estimate), proportion_estimate < 1) %>%
  left_join(region %>% dplyr::select(region, iso3)) %>%
  mutate(
    logit_proportion = logit(proportion_estimate),
    fe_method = factor(fe_method, levels=c("empirical", unique(pse_dat$fe_method)[unique(pse_dat$fe_method) != "empirical" & !is.na(unique(pse_dat$fe_method))])),
  ) %>%
  ungroup %>%
  select(-area_id) %>%
  mutate(has_unc = ifelse(is.na(count_lower), "No", "Yes"),
         prop_lower = ifelse(is.na(prop_lower), count_lower/population, prop_lower),
         prop_upper = ifelse(is.na(prop_upper), count_upper/population, prop_upper),
         l_est = log(count_estimate),
         l_upper = log(count_upper),
         se_u = (l_upper - l_est)/1.96
         ) %>%
  rename(area_id = province_area_id)

pse_dat <- pse_dat %>%
  mutate(method = case_when(
    # method %in% c("Object Multiplier", "Multiplier") ~ "Object multiplier",
    # method %in% c("Service Multiplier") ~ "Service multiplier",
    str_detect(method, "ultiplier") ~ "2S-CRC",
    method == "PLACE/mapping" ~ "PLACE/Mapping",
    TRUE ~ method
  ),
  # fe_method = ifelse(method == "Multiple methods - empirical", "empirical", as.character(fe_method)),
  fe_method = ifelse(str_detect(method, "Multiple methods"), "empirical", as.character(fe_method)),
  fe_method = factor(fe_method))

pse_dat %>%
  dplyr::select(iso3, area_id, kp, year, fe_method, method, starts_with("count"), starts_with("prop"), study_idx, observation_idx) %>%
  write_csv("~/Downloads/pse_uncertainty.csv", na = "")

# 
# imput_se <- pse_dat %>%
#   filter(fe_method != "PLACE/mapping",
#          se_u > 0,
#          !is.na(se_u)) %>%
#   pull(se_u)

# quantile(foo, 0.25) %>% as.numeric

# pse_dat <- pse_dat %>%
#   mutate(se_u = case_when(
#     fe_method == "PLACE/mapping" | is.na(se_u) | se_u == 0 ~ as.numeric(quantile(imput_se, 0.75)),
#     TRUE ~ se_u
#   ))

# count(pse_dat, method, has_unc) %>%
#   pivot_wider(names_from = has_unc, values_from = n)

method.iid.prec.prior <- list(prec= list(prior = "normal", param = c(4, 1)))

fit_pse_model <- function(kp_id) {
  
  pse_inla <- 
    genpop_pred %>%
    distinct(region, iso3, area_id, kp) %>%
    filter(kp == kp_id) %>%
    
    
    # crossing(
    #   year = c(2010:2022),
    #   iso3 = ssa_iso3()) %>%
    
    # crossing(
    #   year = c(2010:2022),
    #   method = unique(pse_dat$method)) %>%
    bind_rows(pse_dat %>%
                filter(kp == kp_id,
                       # fe_method != "PLACE/Mapping"
                       area_id != "CMR_1_9", ## This fixes GNQ
                       !area_id %in% c("COG_1_08gq", "COG_1_10zb", "COG_1_05dt", "COG_1_08gq", "COG_1_05dt")
                       ) %>%
                group_by(study_idx) %>%
                mutate(id.ref = cur_group_id()) %>%
                ungroup %>%
                arrange(fe_method) %>%
                mutate(id.method = as.numeric(fct_inorder(method)),
                       id.method = ifelse(method %in% c(
                         # "Multiple methods - mixture",
                                                        "PLACE/Mapping",
                                                        "PLACE",
                                                        "mapping"), NA, id.method))
              ) %>%
    left_join(areas %>% st_drop_geometry()) %>%
    left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
    mutate(area_id = ifelse(is.na(area_id), iso3, area_id),
           id.year = multi.utils::to_int(year),
           id.iso3_rep = id.iso3) %>%
    dplyr::select(study_idx, iso3, kp, id.iso3, id.iso3_rep, area_id, id.area, logit_proportion, se_u, fe_method, id.method, method, id.ref, year, id.year) %>%
    ungroup() %>%
    mutate(idx = row_number())
  
  nat_level_obs <- pse_inla %>%
    filter(area_id == iso3 & !is.na(logit_proportion)) %>%
    ungroup() %>%
    group_by(id.ref) %>%
    mutate(id.ref.nat = cur_group_id(),
           id.ref = NA)
  
  # place_obs <- pse_inla %>%
  #   filter(fe_method == "PLACE/Mapping" & !is.na(logit_proportion)) %>%
  #   ungroup() %>%
  #   group_by(id.ref) %>%
  #   mutate(id.ref.place = cur_group_id())
  
  pse_inla <- pse_inla %>%
    filter(
      !idx %in% nat_level_obs$idx,
           # !idx %in% place_obs$idx
      ) %>%
    # mutate(id.ref.place = max(place_obs$id.ref.place) +1) %>%
    bind_rows(
      nat_level_obs,
      # place_obs
      )
  # 
  pse_formula <- logit_proportion ~ 
    f(id.area, model = "besag", scale.model = TRUE, graph = "admin1_level_adj_gnq_link.adj", hyper=prec.prior) +
    # f(id.area, model = "besag", scale.model = TRUE, graph = "2023_admin1_level_adj.adj", hyper=prec.prior) +
    f(id.iso3, model = "besag", scale.model = TRUE, graph = national_adj(), hyper=prec.prior) +
    fe_method +
    # f(fe_method, model = "iid", group = id.ref.place, control.group = list(model = "iid"), hyper = prec.prior) +
    f(id.method, model = "iid", hyper = prec.prior, constr = T) +
    f(id.ref, model = "iid", hyper = prec.prior) +
    f(id.ref.nat, model = "iid", hyper = prec.prior)
  
  if(nrow(nat_level_obs) == 0)
    pse_formula <- logit_proportion ~ 
    f(id.area, model = "besag", scale.model = TRUE, graph = "admin1_level_adj_gnq_link.adj", hyper=prec.prior) +
    f(id.iso3, model = "besag", scale.model = TRUE, graph = national_adj(), hyper=prec.prior) +
    fe_method +
    f(id.method, model = "iid", hyper = prec.prior, constr = T) +
    f(id.ref, model = "iid", hyper = prec.prior)
  
  
  pse_fit <- INLA::inla(pse_formula,
                        data = pse_inla,
                        family = "gaussian", 
                        # weights = 1/se_u^2,
                        control.compute = list(config = TRUE,
                                               dic = T),
                        control.predictor=list(compute=TRUE),
                        verbose = FALSE)
  
  message(paste0(kp_id, " | ", pse_fit$dic$dic))
  
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
  qtls <- apply(pse_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
  ident <- pse_inla[ind.effect, ]
  pse_samples <- ident %>% cbind(pse_samples)
  
  pse <- ident %>%
    mutate(
      lower = qtls[1,],
      median = qtls[2,],
      upper = qtls[3,],
      indicator = "pse"
    )
  
  extract_df <- pse_inla %>%
    distinct(id.method, method, fe_method) %>%
    filter(!is.na(method))
  
  pse_random <- distinct(pse_inla, method, id.method) %>%
    filter(!is.na(id.method)) %>%
    left_join(data.frame(pse_fit$summary.random$id.method), by=c("id.method" = "ID")) %>%
    left_join(extract_df)

  pse_fixed <- data.frame(pse_fit$summary.fixed) %>%
    rownames_to_column() %>%
    dplyr::select(rowname, starts_with("X")) %>%
    type.convert(as.is = FALSE)
  
  out <- list()
  out$pse <- pse
  out$pse_samples <- pse_samples
  out$pse_random <- pse_random
  out$pse_fixed <- pse_fixed
  out$pse_summary <- summary(pse_fit)
  out
  
}

# debugonce(fit_pse_model)
# fit_pse_model("FSW")

pse_mod <- lapply(c("FSW", "MSM", "PWID", "TGW"), fit_pse_model)

names(pse_mod) <- c("FSW", "MSM", "PWID", "TGW")

pse_res <- lapply(pse_mod, "[[", "pse") %>%
  bind_rows(.id = "kp")

pse_fixed <- lapply(pse_mod, "[[", "pse_fixed") %>%
  bind_rows(.id = "kp")

# pse_total_method <- lapply(pse_mod, "[[", "pse_total_method") %>%
#   bind_rows(.id = "kp")

pse_random <- lapply(pse_mod, "[[", "pse_random") %>%
  bind_rows(.id = "kp") %>%
  select(kp, method, id.method, fe_method, lower = X0.025quant, median = X0.5quant, upper = X0.975quant) %>%
  mutate(across(lower:upper, ~round(.x, 2)),
         random = paste0(median, " (", lower, ", ", upper, ")")) %>%
  select(-c(lower:upper))

pse_fixed <- pse_fixed %>%
  filter(rowname != "(Intercept)") %>%
  select(kp, fe_method = rowname, lower = X0.025quant, median = X0.5quant, upper = X0.975quant) %>%
  mutate(across(lower:upper, ~round(.x, 2)),
         fixed = paste0(median, " (", lower, ", ", upper, ")"),
         fe_method = case_when(
           str_detect(fe_method, "other") ~ "other methods",
           str_detect(fe_method, "PLACE/Mapping|PLACE|Mapping") ~ "PLACE/Mapping",
           # str_detect(fe_method, "PLACE") ~ "PLACE",
           # str_detect(fe_method, "Mapping") ~ "Mapping",
         )) %>%
  select(-c(lower:upper))

write_csv(pse_random, "R/pse_random.csv")
write_csv(pse_fixed, "R/pse_fixed.csv")

#### Aggregating admin1 level results and estimating counts

options(dplyr.summarise.inform = FALSE)

aggregate_results <- function() {
  
  urban_proportion <- read_csv("2023_urban_proportion.csv", show_col_types = F)
  
  prev_s <- prev_mod %>%
    lapply("[[", "prev_samples") %>%
    bind_rows() %>%
    filter(!is.na(area_id)) %>%
    left_join(region %>% select(iso3, region), by = c("iso3", "region")) %>%
    select(region, iso3, area_id, kp, as.character(1:1000)) 
  
  pse_s <- pse_mod %>%
    lapply("[[", "pse_samples") %>%
    bind_rows() %>%
    left_join(kp_to_sex() %>% mutate(sex = ifelse(kp == "PWID", "male", sex)), by = "kp") %>%
    left_join(region %>% select(iso3, region), by = "iso3") %>%
    select(region, iso3, area_id, kp, sex, as.character(1:1000))
  
  art_s <- art_mod$art_samples %>%
    filter(!is.na(area_id)) %>%
    left_join(region %>% select(iso3, region), by = c("iso3", "region")) %>%
    select(region, iso3, area_id, kp, as.character(1:1000)) 
  
  # scale_pop_id <- lapply(ssa_iso3, function(x){
  #   orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022 && parameter:source == "WPP2022")'), draft = FALSE)
  # })
  # 
  # pop <- lapply(paste0("archive/aaa_scale_pop/", scale_pop_id, "/interpolated_population.csv"), read_csv, show_col_types = F) %>%
  #   bind_rows() %>%
  #   filter(year == 2021) %>%
  #   five_year_to_15to49("population") %>%
  #   sex_aggregation("population")
  
  pop <- read_csv("2023_pop.csv", show_col_types = F)
  
  pop <- pop %>%
    mutate(area_id = case_when(
      area_id == "CMR_1_3" ~ "CMR_1_5", # Douala merged into Littoral
      area_id == "CMR_1_12" ~ "CMR_1_2", # Yaounde merged into Centre
      area_id == "CMR_1_4" ~ "CMR_1_3", 
      area_id == "CMR_1_5" ~ "CMR_1_4", 
      area_id == "CMR_1_6" ~ "CMR_1_5", 
      area_id == "CMR_1_7" ~ "CMR_1_6", 
      area_id == "CMR_1_8" ~ "CMR_1_7", 
      area_id == "CMR_1_9" ~ "CMR_1_8", 
      area_id == "CMR_1_11" ~ "CMR_1_9",
      
      area_id == "ZMB_1_10" ~ "ZMB_1_10gh",
      area_id == "ZMB_1_17" ~ "ZMB_1_17xt",
      area_id == "ZMB_1_12" ~ "ZMB_1_12sg",
      area_id == "ZMB_1_14" ~ "ZMB_1_14fz",
      area_id == "ZMB_1_15" ~ "ZMB_1_15pi",
      
      TRUE ~ area_id
    )) %>%
    summarise(population = sum(population), .by = c(area_id, year, sex, age_group)) %>%
    ungroup()
  
  urban_prop_s <- matrix(rep(rbeta(1000, 5, 3), nrow(pse_s)), nrow = nrow(pse_s), byrow = TRUE)
  rural_pse_s <- pse_s %>%
    select(region, iso3, area_id, kp, sex) %>%
    cbind(invlogit(pse_s[as.character(1:1000)]) * urban_prop_s)
  
  urban_count_samples <- pse_s %>%
    left_join(pop %>% select(area_id, sex, population), by = c("area_id", "sex")) %>%
    left_join(urban_proportion %>% select(area_id, urban_proportion), by = "area_id") %>%
    mutate(across(as.character(1:1000), ~invlogit(.x) * population * urban_proportion),
           indicator = "pse_urban_count")
  
  rural_count_samples <- rural_pse_s %>%
    left_join(pop %>% select(area_id, sex, population), by = c("area_id", "sex")) %>%
    left_join(urban_proportion %>% select(area_id, urban_proportion), by = "area_id") %>%
    mutate(across(as.character(1:1000), ~.x * population * (1-urban_proportion)))
  
  pse_count_samples <- bind_rows(urban_count_samples, rural_count_samples) %>%
    summarise(across(as.character(1:1000), sum), .by = c(region, iso3, area_id, kp)) %>%
    mutate(indicator = "pse_count")
  
  kplhiv_samples <- bind_rows(pse_count_samples,
                              prev_s %>% mutate(indicator = "prev")) %>%
    group_by(region, iso3, area_id, kp) %>%
    summarise(across(as.character(1:1000), ~.x[indicator == "pse_count"] * invlogit(.x[indicator == "prev"]))) %>% ## This is a bit longer than it needs to be - just giving summarise a "multiply" function would do the job, but e.g. ~`*` doesn't work. I could define a function but not sure what the gain is there.
    mutate(indicator = "kplhiv")
  
  kpart_samples <- bind_rows(kplhiv_samples,
                             art_s %>% mutate(indicator = "art")) %>%
    group_by(region, iso3, area_id, kp) %>%
    summarise(across(as.character(1:1000), ~invlogit(.x[indicator == "art"]) * .x[indicator == "kplhiv"])) %>%
    mutate(indicator = "kpart")
  
  count_indicators <- bind_rows(pse_count_samples, urban_count_samples, kplhiv_samples, kpart_samples)
  
  country_aggregates <- count_indicators %>%
    group_by(region, iso3, kp, indicator) %>%
    summarise(across(as.character(1:1000), sum))
  
  regional_aggregates <- count_indicators %>%
    bind_rows(mutate(., region = "SSA")) %>%
    group_by(region, kp, indicator) %>%
    summarise(across(as.character(1:1000), sum))
  
  denominators <- pop %>%
    separate(area_id, into = c("iso3", NA), sep = 3, remove = F) %>%
    left_join(region) %>%
    left_join(urban_proportion) %>%
    mutate(urban_population = population*urban_proportion) %>%
    filter(!is.na(urban_population)) %>% ## TODO: better way of selecting the area_level that is being modelled. Fine for now
    filter(sex != "both") %>% ## Removed when matching PWID to men
    left_join(kp_to_sex() %>% mutate(sex = ifelse(kp == "PWID", "male", sex)) %>% filter(kp %in% unique(pse_dat$kp)))
  
  area_pse_prop <- pse_count_samples %>%
    left_join(denominators %>% select(area_id, kp, population)) %>%
    select(region:indicator, population, as.character(1:1000)) %>%
    mutate(across(as.character(1:1000), ~.x/population),
           indicator = "pse_prop")
  
  denominators <- denominators %>%
    group_by(region, iso3, kp) %>% 
    summarise(urban_population = sum(urban_population),
              population = sum(population)) %>%
    left_join(spec_dat %>% filter(indicator == "hivpop", sex == "both") %>% rename(plhiv = mean) %>% select(iso3, plhiv))
  
  country_proportions <- country_aggregates %>%
    filter(indicator %in% c("pse_count", "pse_urban_count", "kplhiv")) %>%
    left_join(denominators %>% 
                rename(pse_urban_count = urban_population, pse_count = population, kplhiv = plhiv) %>% 
                pivot_longer(-c(region, iso3, kp), names_to = "indicator", values_to = "denominator")
    ) %>%
    select(region:indicator, denominator, as.character(1:1000)) %>%
    mutate(across(as.character(1:1000), ~.x/denominator),
           indicator = case_when(
             indicator == "kplhiv" ~ "plhiv_prop",
             indicator == "pse_count" ~ "pse_prop",
             indicator == "pse_urban_count" ~ "pse_urban_prop"
           )
    )
  
  country_proportions <- country_proportions %>%
    bind_rows(
      count_indicators %>%
        filter(indicator %in% c("kplhiv", "pse_count")) %>%
        group_by(region, iso3, kp) %>%
        summarise(across(as.character(1:1000), ~sum(.x[indicator == "kplhiv"])/sum(.x[indicator == "pse_count"]))) %>%
        mutate(indicator = "prevalence"),
      count_indicators %>%
        filter(indicator %in% c("kpart", "kplhiv")) %>%
        group_by(region, iso3, kp) %>%
        summarise(across(as.character(1:1000), ~sum(.x[indicator == "kpart"])/sum(.x[indicator == "kplhiv"]))) %>%
        mutate(indicator = "art_coverage")
    )
  
  regional_proportions <- country_aggregates %>%
    filter(indicator %in% c("pse_count", "pse_urban_count", "kplhiv")) %>%
    left_join(denominators %>% 
                rename(pse_urban_count = urban_population, pse_count = population, kplhiv = plhiv) %>% 
                pivot_longer(-c(region, iso3, kp), names_to = "indicator", values_to = "denominator")
    ) %>%
    ungroup() %>%
    bind_rows(mutate(., region = "SSA")) %>%
    select(region:indicator, denominator, as.character(1:1000)) %>%
    group_by(region, kp, indicator) %>%
    summarise(across(as.character(1:1000), ~sum(.x)/sum(denominator))) %>%
    mutate(
      indicator = case_when(
        indicator == "kplhiv" ~ "plhiv_prop",
        indicator == "pse_count" ~ "pse_prop",
        indicator == "pse_urban_count" ~ "pse_urban_prop"
      )
    )
  
  regional_proportions <- regional_proportions %>%
    bind_rows(
      count_indicators %>%
        filter(indicator %in% c("kplhiv", "pse_count")) %>%
        bind_rows(mutate(., region = "SSA")) %>%
        group_by(region, kp) %>%
        summarise(across(as.character(1:1000), ~sum(.x[indicator == "kplhiv"])/sum(.x[indicator == "pse_count"]))) %>%
        mutate(indicator = "prevalence"),
      count_indicators %>%
        filter(indicator %in% c("kpart", "kplhiv")) %>%
        bind_rows(mutate(., region = "SSA")) %>%
        group_by(region, kp) %>%
        summarise(across(as.character(1:1000), ~sum(.x[indicator == "kpart"])/sum(.x[indicator == "kplhiv"]))) %>%
        mutate(indicator = "art_coverage")
    )
  
  #' Wanted to do this within across() but:
  #' df %>% 
  #'  group_by(iso3, kp, indicator) %>%
  #'  reframe(across(as.character(1:1000), ~quantile(.x, c(0.025, 0.5, 0.975))))
  #'  
  #'  Is *incredibly* slow. Any suggestions?
  #'  
  #'  
  
  ## PSE area results
  
  area_qtls <- apply(area_pse_prop[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))
  
  area_res <- area_pse_prop %>%
    select(region:kp) %>%
    cbind(data.frame(t(area_qtls)))
  
  colnames(area_res) <- c("region", "iso3", "area_id", "kp", "lower", "median", "upper")
  
  area_res <- area_res %>%
    arrange(iso3, kp) %>%
    mutate(indicator = "pse_prop") %>%
    select(region:kp, indicator, median, lower, upper)
  
  ## Country results
  
  country_qtls <- apply(bind_rows(country_aggregates, country_proportions)[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))
  
  country_res <- bind_rows(country_aggregates, country_proportions) %>%
    select(region:indicator) %>%
    cbind(data.frame(t(country_qtls)))
  
  colnames(country_res) <- c("region", "iso3", "kp", "indicator", "lower", "median", "upper")
  
  country_res <- country_res %>%
    arrange(iso3, kp) %>%
    relocate(median, .before = lower)
  
  # Region results
  region_qtls <- apply(bind_rows(regional_aggregates, regional_proportions)[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))
  
  region_res <- bind_rows(regional_aggregates, regional_proportions) %>%
    select(region:indicator) %>%
    cbind(data.frame(t(region_qtls)))
  
  colnames(region_res) <- c("region", "kp", "indicator", "lower", "median", "upper")
  
  region_res <- region_res %>%
    arrange(region, kp) %>%
    relocate(median, .before = lower)
  
  ## Aggregating all KPs together
  
  region_all_kp <- country_aggregates %>%
    filter(indicator %in% c("kplhiv", "pse_count")) %>%
    bind_rows(mutate(., region = "SSA")) 
  
  region_all_kp_aggregates <- region_all_kp %>%
    group_by(region, indicator) %>%
    summarise(across(as.character(1:1000), sum)) %>%
    mutate(kp = "All")
  
  region_all_kp_proportions <- region_all_kp_aggregates %>%
    left_join(
      spec_dat %>% 
        filter(sex == "both", indicator %in% c("totpop", "hivpop")) %>% 
        left_join(region) %>% 
        bind_rows(mutate(., region = "SSA")) %>%
        group_by(region, indicator) %>%
        summarise(denominator = sum(mean)) %>%
        mutate(indicator = case_when(
          indicator == "hivpop" ~ "kplhiv",
          indicator == "totpop" ~ "pse_count"
        ))
    ) %>%
    mutate(across(as.character(1:1000), ~.x/denominator),
           indicator = case_when(
             indicator == "kplhiv" ~ "plhiv_prop",
             indicator == "pse_count" ~ "pse_prop"
           ),
           kp = "All"
    )
  
  region_all_qtls <- apply(bind_rows(region_all_kp_aggregates, region_all_kp_proportions)[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))
  
  region_all_res <- bind_rows(region_all_kp_aggregates, region_all_kp_proportions) %>%
    select(region, kp, indicator)
  
  region_all_res <- region_all_res %>%
    cbind(data.frame(t(region_all_qtls)))
  
  colnames(region_all_res) <- c("region", "kp", "indicator", "lower", "median", "upper")
  
  region_res <- region_res %>%
    bind_rows(region_all_res) %>%
    arrange(region, kp) %>%
    relocate(median, .before = lower)
  
  ####
  
  kplhiv_art <- list()
  
  kplhiv_art$region <- region_res %>%
    type.convert(as.is = T) %>%
    group_by(kp) %>%
    group_split()
  
  kplhiv_art$country <- country_res %>%
    type.convert(as.is = T) %>%
    group_by(kp) %>%
    group_split()
  
  kplhiv_art$prev_continuous_res <- lapply(prev_mod, "[[", "prev") %>%
    lapply(relocate, median, .before = lower)
  
  kplhiv_art$art_continuous_res <- art_mod$art %>% 
    relocate(median, .before = lower) %>%
    group_by(kp) %>%
    group_split()
  
  kplhiv_art$pse_area_res <- area_res %>%
    group_by(kp) %>%
    group_split()
  
  names(kplhiv_art$art_continuous_res) <- names(kplhiv_art$country) <- names(kplhiv_art$pse_area_res) <- c("FSW", "MSM", "PWID", "TGW")
  names(kplhiv_art$region) <- c("All", "FSW", "MSM", "PWID", "TGW")
  
  kplhiv_art
}

kplhiv_art <- aggregate_results()

pse_dat %>%
  ggplot(aes(x=invlogit(logit_proportion), y=se_u)) +
    geom_point() +
    scale_x_continuous(trans = "logit", breaks = c(0.0001, 0.001, 0.01, 0.1, 0.25, 0.075), labels = scales::label_percent())

kplhiv_art$country %>% 
  lapply(data.frame) %>% 
  bind_rows() %>% 
  filter(indicator == "pse_urban_prop") %>% 
  mutate(source = "With weighting") %>%
  bind_rows(
    without_results$country %>% 
      lapply(data.frame) %>% 
      bind_rows() %>% 
      filter(indicator == "pse_urban_prop") %>% 
      mutate(source = "Without weighting")
  ) %>%
  ggplot(aes(x=iso3, y=median, color = source)) +
    # geom_pointrange(aes(ymin = lower, ymax = upper)) +
    geom_point() +
    facet_wrap(~kp) +
    scale_percent() +
    standard_theme()

# saveRDS(kplhiv_art, "~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/kplhiv_art.rds")

# saveRDS(kplhiv_art_1529, "~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/kplhiv_art_1529.rds")

#### Katie exports
# 
# katie_dat <- bind_rows(pse_count_samples, 
#           pse_s %>% mutate(indicator = "pse_urban_prop",
#                            across(as.character(1:1000), ~invlogit(.x))))
# 
# katie_q <- apply(katie_dat[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))
# 
# katie_exp <- katie_dat %>%
#   select(iso3, kp, area_id, indicator) %>%
#   cbind(data.frame(t(katie_q)))
# 
# colnames(katie_exp) <- c("region", "iso3", "kp", "area_id",  "indicator", "lower", "median", "upper")
# 
# 
# country_res %>% 
#   filter(indicator %in% c("pse_urban_prop")) %>%
#   group_by(kp) %>%
#   reframe(calculate_quantile(median))
# 
# region_res %>%
#   filter(indicator %in% c("plhiv_prop", "pse_prop"), 
#          region == "SSA", 
#          kp == "All")
# 
# region_res %>%
#   filter(indicator %in% c("plhiv_prop"), 
#          region == "SSA", 
#          kp != "All")
# 
# region_res %>%
#   filter(indicator %in% c("pse_count"), 
#          region == "SSA", 
#          kp != "All")
# 
# region_res %>%
#   filter(indicator == "pse_prop", kp == "FSW")

###

# int <- readRDS("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/kplhiv_art.rds")
# 
# names(int)

####

plot_order <- c("SEN", "GMB", "GNB", "GIN", "SLE", "LBR", "MLI", "BFA", "CIV", "GHA", "TGO", "BEN", "NER", "NGA", "CMR", "TCD", "CAF", "SSD", "ERI", "ETH", "GAB", "COG", "GNQ", "COD", "UGA", "KEN", "RWA", "BDI", "TZA", "AGO", "ZMB", "MWI", "MOZ", "BWA", "ZWE", "NAM", "SWZ", "LSO", "ZAF")

# kplhiv_art <- readRDS("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/kplhiv_art.rds")

kplhiv_proportion_plot <- kplhiv_art$country %>%
  lapply(data.frame) %>%
  bind_rows() %>%
  filter(indicator == "plhiv_prop") %>%
  name_kp(F) %>%
  # filter(iso3 != "ZAF",
  #        indicator == "kplhiv") %>%
  ggplot(aes(x=fct_rev(fct_relevel(iso3, plot_order)), y=median, fill=fct_rev(kp))) +
  geom_col(position = "stack", show.legend = F) +
  standard_theme() +
  scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, .05))) +
  scale_x_discrete(labels = ~countrycode::countrycode(sourcevar = .x, origin = "iso3c", destination = "country.name", custom_match = moz.utils::cc_plot()), expand = expansion(mult = c(0.02, .02))) +
  scale_manual("fill", 4) +
  labs(x=element_blank(), y="Proportion of total people living with HIV", fill=element_blank(), tag = "A") +
  coord_flip() +
  theme(legend.position = "right",
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        plot.tag = element_text(size = rel(2.0), face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank()
  )

kplhiv_plot <- kplhiv_art$region %>%
  lapply(data.frame) %>%
  bind_rows()%>%
  filter(kp != "All") %>%
  name_kp(F) %>%
  filter(indicator == "kplhiv", region != "SSA")%>%
  name_region(F) %>%
  ggplot(aes(x=region, y=median, group=kp, fill=kp)) +
  geom_col(position = position_dodge(.9)) +
  geom_linerange(aes(ymin=lower, ymax = upper), position=position_dodge(.9)) +
  scale_y_continuous(labels = scales::label_number(scale = 1E-3), expand = expansion(mult = c(0, .05))) +
  # scale_y_log10(labels = scales::label_number()) +
  scale_fill_manual(values = rev(c(wesanderson::wes_palette("Darjeeling2")[2], wesanderson::wes_palette("Darjeeling1")[c(2,4,5)]))) +
  standard_theme() +
  labs(x=element_blank(), y="KPLHIV (thousands)", fill=element_blank(), tag = "B") +
  theme(plot.tag = element_text(size = rel(2.0), face = "bold"),
        axis.text.x = element_text(size = rel(1.2)),
        axis.text.y = element_text(size = rel(1.2)),
        panel.grid = element_blank())

png("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Figs/Fig 7 kplhiv_plot.png", width = 600, height=1000)
ggpubr::ggarrange(kplhiv_proportion_plot, kplhiv_plot, nrow =2, heights = c(1.6,1), common.legend = T, legend = "bottom")
dev.off()

#####

existing <- readRDS("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/kplhiv_art.rds")

pse_old <- readxl::read_excel("~/Downloads/pse_old.xlsx") %>%
  mutate(iso3 = countrycode(country, "country.name", "iso3c")) %>%
  separate(value, into = c("median", "lower", "upper"), sep = " ") %>%
  mutate(across(c(lower, upper), ~str_remove_all(.x, "\\(|\\,|\\)"))) %>%
  type.convert(as.is = T) %>%
  mutate(across(median:upper, ~.x/100),
         source = "old")
         

prev_old <- readxl::read_excel("~/Downloads/pse_old.xlsx", sheet = 2) %>%
  mutate(iso3 = countrycode(Country, "country.name", "iso3c")) %>%
  select(-Country) %>%
  pivot_longer(-iso3) %>%
  separate(value, into = c("median", "lower", "upper"), sep = " ") %>%
  mutate(across(c(lower, upper), ~str_remove_all(.x, "\\(|\\,|\\)"))) %>%
  type.convert(as.is = T) %>%
  select(iso3, kp = name, median, lower, upper) %>%
  mutate(indicator = "prevalence",
         across(median:upper, ~.x/100),
         source = "old")

art_old <- readxl::read_excel("~/Downloads/pse_old.xlsx", sheet = 3) %>%
  mutate(iso3 = countrycode(Country, "country.name", "iso3c")) %>%
  select(-Country) %>%
  pivot_longer(-iso3) %>%
  separate(value, into = c("median", "lower", "upper"), sep = " ") %>%
  mutate(across(c(lower, upper), ~str_remove_all(.x, "\\(|\\,|\\)"))) %>%
  type.convert(as.is = T) %>%
  select(iso3, kp = name, median, lower, upper) %>%
  mutate(indicator = "art_coverage",
         across(median:upper, ~.x/100),
         source = "old")

old <- bind_rows(pse_old, prev_old, art_old)

new_study <- unique(bind_rows(prev_dat, art_dat, pse_dat) %>% pull(study_idx))
old_study <- unique(readxl::read_excel("~/Imperial College London/HIV Inference Group - WP - Documents/Data/KP/Aggregate data/Data sharing/2023-11-22_Pre-print/2023-11-22_key-population-collated-data.xlsx", sheet = 2)$study_idx)
new_study[!new_study %in% old_study]

ethic %>%
  filter(study_idx %in% new_study[!new_study %in% old_study]) %>% View

pse_dat %>%
  filter(study_idx %in% new_study[!new_study %in% old_study]) %>% View

kplhiv_art$country %>%
  lapply(data.frame) %>%
  bind_rows(.id = "kp") %>%
  filter(indicator == "pse_urban_prop") %>%
mutate(source = "New") %>%
  bind_rows(
    # existing$country %>%
    #   lapply(data.frame) %>%
    #   bind_rows(.id = "kp") %>%
    #   filter(indicator == "pse_urban_prop") %>%
    #   mutate(source = "Old")
    old %>%
      filter(indicator == "pse_urban_prop")
  ) %>%
  ggplot(aes(x=iso3, y=median, color= source)) +
    geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(.9)) +
    facet_wrap(~kp, nrow = 2, scales = "free") +
    standard_theme() +
    scale_percent() +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    no_labels()



kplhiv_art$prev_continuous_res %>%
  lapply(data.frame) %>%
  bind_rows(.id = "kp") %>%
  mutate(source = "new") %>%
  filter(is.na(iso3)) %>%
  bind_rows(
    existing$prev_continuous_res %>%
      lapply(data.frame) %>%
      bind_rows(.id = "kp") %>%
      mutate(source = "old") %>%
      filter(is.na(iso3))
  ) %>%
  ggplot(aes(x=logit_gen_prev, y=median)) +
  geom_line(aes(color =source)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.3) +
  # geom_point(dat = prev_dat %>% mutate(median = logit(value), logit_gen_prev = logit(provincial_value)) %>% left_join(region())) +
  facet_grid(region~kp) +
  standard_theme()

kplhiv_art$art_continuous_res %>%
  lapply(data.frame) %>%
  bind_rows(.id = "kp") %>%
  mutate(source = "new") %>%
  filter(is.na(iso3)) %>%
  bind_rows(
    existing$art_continuous_res %>%
      lapply(data.frame) %>%
      bind_rows(.id = "kp") %>%
      mutate(source = "old") %>%
      filter(is.na(iso3))
  ) %>%
  ggplot(aes(x=logit_gen_art, y=median)) +
    geom_line(aes(color =source)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.3) +
    # geom_point(dat = prev_dat %>% mutate(median = logit(value), logit_gen_prev = logit(provincial_value)) %>% left_join(region())) +
    facet_wrap(~kp) +
    standard_theme()

kplhiv_art$region %>% 
  lapply(data.frame) %>%
  bind_rows() %>% 
  filter(region == "SSA") %>%
  mutate(source = "new") %>%
  bind_rows(
    existing$region %>% 
      lapply(data.frame) %>%
      bind_rows() %>% 
      filter(region == "SSA") %>%
      mutate(source = "old")
  ) %>%
  filter(indicator %in% c("pse_urban_prop", "pse_prop")) %>%
  arrange(indicator, kp) %>%
  write_csv("~/Downloads/comp_pse.csv")


###

pop_2023 <- pop %>% filter(str_length(area_id) == 3) %>% group_by(area_id) %>% summarise(population = sum(population))
pop2022 <- read_csv("~/Downloads/jeff-kp-runthrough/pop.csv", show_col_types = F)
pop_2022 <- pop2022 %>% filter(str_length(area_id) == 3) %>% group_by(area_id) %>% summarise(population = sum(population))
bind_rows(pop_2023 %>% 
            mutate(source = 2023), pop_2022 %>% mutate(source = 2022)) %>% 
  ggplot(aes(x=area_id, y=population, color = factor(source))) + 
    geom_point()

spec_dat_2023 <- spec_dat %>% filter(indicator == "hivpop", sex == "both") %>% mutate(source = "2023")
spec_dat_2022 <- read_csv("~/Downloads/jeff-kp-runthrough/spec_dat.csv", show_col_types = F) %>% 
  mutate(source = "2022") %>% 
  filter(age %in% 15:49, spectrum_region_name != "Nigeria") %>%
  group_by(iso3, source) %>%
  summarise(mean = sum(hivpop))

bind_rows(spec_dat_2023, spec_dat_2022) %>% ggplot(aes(x=iso3, y=mean, color = source)) + geom_point()

version_comparison_plots <- lapply(unique(kplhiv_art$country$FSW$indicator), function(x) {
  
  existing$country %>%
    lapply(data.frame) %>%
    bind_rows() %>%
    filter(indicator == x) %>%
    mutate(source = "old") %>%
    bind_rows(
      kplhiv_art$country %>%
        lapply(data.frame) %>%
        bind_rows() %>%
        filter(indicator == x) %>%
        mutate(source = "new")
    ) %>%
    group_by(kp, iso3) %>%
    summarise(diff = median[source== "new"]/median[source=="old"]) %>%
    group_by(kp) %>%
    mutate(kp_med = median(diff)) %>%
    ggplot(aes(x=iso3, y=diff)) + 
      geom_point() + 
      geom_hline(aes(yintercept = 1), linetype = 2) +
      geom_hline(aes(yintercept = kp_med), color = "red", linetype = 2) +
      facet_wrap(~kp) +
      standard_theme() +
      no_labels() +
      labs(title = x) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
})

