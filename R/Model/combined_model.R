library(INLA)
library(tidyverse)
library(countrycode)
library(sf)
library(lme4)
library(spud)
library(naomi)
library(moz.utils)

setwd(rprojroot::find_rstudio_root_file())

region <- moz.utils::region()
ssa_iso3 <- ssa_iso3()
grey <- read_sf(grey_areas())
geographies <- read_sf(national_areas()) %>%
  mutate(iso3 = area_id) %>%
  arrange(iso3) %>%
  group_by(iso3) %>%
  mutate(id.iso3 = cur_group_id())

invlogit <- function(x) {exp(x)/(1+exp(x))}
logit <- function(x) {log(x/(1-x))}

convert_logis_labels <- function(x) {
  paste0(round(plogis(x)*100, 1), "%")
}

spec_id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_data_pjnz", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022)'), draft = FALSE)
})

spec_dat <- lapply(paste0("archive/aaa_data_pjnz/", spec_id, "/naomi_pjnz.zip"), extract_pjnz_naomi)

ssa_15to49 <- spec_dat %>%
  bind_rows() %>%
  filter(spectrum_region_name != "Nigeria") %>% ## Remove national Nigeria file because all the subnational files are there too
  left_join(region) %>%
  filter(age %in% 15:49,
         year == 2021) %>%
  group_by(region) %>%
  summarise(hivpop = sum(hivpop),
            totpop = sum(totpop)) %>%
  ungroup() %>%
  bind_rows(summarise(., hivpop = sum(hivpop), totpop = sum(totpop), region = "SSA"))

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
    (is.na(denominator) | denominator == 0) & kp == "TGW" ~ filter(imp_denomin, kp == "TGW")$quant,
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

# naomi_dat <- lapply(ssa_iso3[ssa_iso3 != "SSD"], function(iso3) {
#   message(iso3)
# 
#   sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))
#   folder <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = "Shared Documents/Data/Spectrum files/2022 naomi preliminary")
# 
#   if(iso3 == "MOZ") {
#     folder <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = "Shared Documents/Data/Spectrum files/2021 naomi")
#   }
# 
#   path <- filter(folder$list(),
#                  str_detect(name, fixed(iso3, ignore_case=TRUE)),
#                  str_detect(name, "zip"))$name
# 
# 
#   if(length(path) > 1)
#     stop("More than one Naomi fit found")
# 
#   if(length(path) == 0)
#     stop("No Naomi fit found")
# 
#   if(iso3 != "MOZ") {
#     path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/Spectrum files/2022 naomi preliminary", path)
#   } else {
#     path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/Spectrum files/2021 naomi", path)
#   }
# 
#   indicators <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = path)
# 
#   tmpd <- tempfile()
#   on.exit(unlink(tmpd))
#   utils::unzip(indicators, exdir = tmpd)
#   out <- list()
#   out$indicators <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "indicators.csv"))
#   out$meta_age_group <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "meta_age_group.csv"))
#   out$meta_period <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "meta_period.csv"))
#   out$meta_area <- read_sf(list.files(tmpd, full.names = TRUE, pattern = "boundaries.geojson"))
#   out$meta_indicator <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "meta_indicator.csv"))
#   class(out) <- "naomi_output"
# 
#   indicators <- add_output_labels(out)
# 
#   time_point <- unique(indicators$calendar_quarter)[2]
# 
#   indicators <- filter(indicators, calendar_quarter == time_point)
#   areas <- out$meta_area
# 
#   out <- list()
#   out$indicators <- indicators
#   out$areas <- areas
#   out
# 
# })
# names(naomi_dat) <- ssa_iso3[ssa_iso3 != "SSD"]
# saveRDS(naomi_dat, "~/Downloads/naomi_dat.rds")

naomi_dat <- readRDS("~/Downloads/naomi_dat.rds")

long_nd <- lapply(naomi_dat, "[[", "indicators") %>%
  bind_rows(.id = "iso3")

ssd_dat <- read_csv("R/Report/R objects for report/ssd.csv") %>%
  mutate(age_group = "Y015_049") %>%
  select(-c(plhiv, art)) %>%
  pivot_longer(c(population, prevalence, art_coverage), names_to = "indicator", values_to = "mean")
  # mutate(age_group = ifelse(sex == "male", "Y015_029", age_group))

eri_dat <- extract_pjnz_naomi("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2022 final shared/EPP-Gen/Eritrea _Mar_18_2022 Shiny90.PJNZ")

eri_dat <- eri_dat %>%
  filter(year == 2021) %>%
  single_year_to_five_year() %>%
  select(iso3, sex, totpop, hivpop, artpop) %>%
  sex_aggregation(c("totpop", "hivpop", "artpop")) %>%
  group_by(iso3, sex) %>%
  summarise(prevalence = sum(hivpop)/sum(totpop),
            art_coverage = sum(artpop)/sum(hivpop))

eri_areas <- read_sf("archive/eri_data_areas/20221208-090657-ce08516e/eri_areas.geojson") %>%
  select(-display)

eri_pop_id <- orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "ERI" && parameter:version == 2022 && parameter:source == "WPP2022")'), draft = FALSE)

eri_dat <- eri_areas %>%
  mutate(iso3 = "ERI",
         calendar_quarter = "CY2021Q4") %>%
  select(iso3, area_id) %>%
  st_drop_geometry() %>%
  left_join(eri_dat) %>%
  left_join(read_csv(paste0("archive/aaa_scale_pop/", eri_pop_id, "/interpolated_population.csv")) %>%
              sex_aggregation("population") %>%
              five_year_to_15to49("population") %>%
              filter(year == 2021)) %>%
  pivot_longer(c(population, prevalence, art_coverage), names_to = "indicator", values_to = "mean")

areas <- lapply(naomi_dat, "[[", "areas") %>%
  lapply(sf::st_make_valid) %>%
  lapply(select, -name)

ssd_areas <- read_sf("archive/ssd_data_areas/20221213-090608-a0ded228/ssd_areas.geojson") %>%
  select(colnames(areas[[1]])) %>%
  sf::st_make_valid()

areas <- c(areas,
           "SSD" = list(ssd_areas),
           "ERI" = list(eri_areas))

lvl_map <- read.csv("global/iso_mapping_fit.csv")
admin1_lvl <- lvl_map %>% select(iso3, admin1_level)

areas <- areas %>%
  do.call(rbind, .) %>%
  separate(area_id, 3, remove = FALSE, into = c("iso3", NA))

long_nd <- long_nd %>%
  bind_rows(ssd_dat %>%
              mutate(iso3 = "SSD",
                     calendar_quarter = "CY2021Q4"),
            eri_dat %>% mutate(calendar_quarter = "CY2021Q4") %>%
              left_join(eri_areas %>% st_drop_geometry() %>% select(area_id, area_level))
            )

ind_15_29 <- long_nd %>%
  filter(age_group_label %in% c("15-24", "25-29"),
         indicator %in% c("plhiv", "population", "art_current_residents")) %>%
  select(iso3, area_id, area_level, area_name, sex, indicator, age_group, mean) %>%
  pivot_wider(names_from = indicator, values_from = mean) %>%
  group_by(iso3, area_id, area_name, area_level, sex) %>%
  summarise(prevalence = sum(plhiv)/sum(population),
            art_coverage = sum(art_current_residents)/sum(plhiv),
            population = sum(population)) %>%
  mutate(age_group_label = "15-29",
         age_group = "Y015_029")

#' No age-specific estimates of prevalence or ART coverage for SSD or ERI
#' Calculate 15-29:15-49 HIV prevalence and ART coverage ratios from neighbouring countries:
#' "CAF", "COD", "UGA", "KEN", "ETH"
#' 
#' Male:
#' 15-29:15-49 ART coverage ratio = 0.94
#' 15-29:15-49 HIV prevalence ratio = 0.46
#' 
#' #' fenale:
#' 15-29:15-49 ART coverage ratio = 0.92
#' 15-29:15-49 HIV prevalence ratio = 0.56
#' 
#' 
# long_nd %>%
#   filter(iso3 %in% c("CAF", "COD", "UGA", "KEN", "ETH"),
#          area_level == 0) %>%
#   filter(age_group_label %in% c("15-24", "25-29"),
#          indicator %in% c("plhiv", "population", "art_current_residents")) %>%
#   select(iso3, area_id, area_level, area_name, sex, indicator, age_group, mean) %>%
#   pivot_wider(names_from = indicator, values_from = mean) %>%
#   group_by(iso3, area_id, area_name, area_level, sex) %>%
#   summarise(prev_15 = sum(plhiv)/sum(population),
#             art_15 = sum(art_current_residents)/sum(plhiv))  %>%
#   left_join(
#     long_nd %>%
#       filter(iso3 %in% c("CAF", "COD", "UGA", "KEN", "ETH"),
#              area_level == 0) %>%
#       filter(age_group_label == "15-49",
#              indicator %in% c("prevalence", "art_coverage")) %>%
#       select(iso3, area_id, area_level, area_name, sex, indicator, age_group, mean) %>%
#       pivot_wider(names_from = indicator, values_from = mean) 
#   ) %>%
#   mutate(prev_ratio = prev_15/prevalence,
#          art_ratio = art_15/art_coverage) %>%
#   group_by(sex) %>%
#   reframe(calculate_quantile(art_ratio, percentage = F))

eri_ssd_1529 <- long_nd %>%
  filter(iso3 %in% c("SSD", "ERI"),
         indicator != "population",
         sex != "both") %>%
  mutate(age_group = "Y015_029",
         mean = case_when(
           indicator == "prevalence" & sex == "female" ~ mean * 0.56,
           indicator == "art_coverage" & sex == "female" ~ mean * 0.92,
           indicator == "prevalence" & sex == "male" ~ mean * 0.46,
           indicator == "art_coverage" & sex == "male" ~ mean * 0.94,
         ))

eri_ssd_scale_id <- lapply(c("ERI", "SSD"), function(x){
  orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022 && parameter:source == "WPP2022")'), draft = FALSE)
})

eri_ssd_1529_pop <- lapply(paste0("archive/aaa_scale_pop/", eri_ssd_scale_id, "/interpolated_population.csv"), read_csv) %>%
  bind_rows() %>%
  filter(year == 2021,
         age_group %in% c("Y015_019", "Y020_024", "Y025_029")) %>%
  group_by(area_id, sex) %>%
  summarise(population = sum(population)) %>%
  mutate(age_group = "Y015_029",
         calendar_quarter = "CY2021Q4",
         indicator = "population",
         area_level = ifelse(str_detect(area_id, "_1_"), 1, 0),
         iso3 = ifelse(str_detect(area_id, "SSD"), "SSD", "ERI")) %>%
  rename(mean = population)

eri_ssd_1529 <- eri_ssd_1529 %>%
  bind_rows(eri_ssd_1529_pop) %>%
  arrange(area_id)


long_nd <- long_nd %>%
  bind_rows(
    ind_15_29 %>%
      pivot_longer(cols = c("prevalence", "art_coverage", "population"), names_to = "indicator", values_to = "mean"),
    eri_ssd_1529
  ) %>%
  arrange(area_id)


# pop <- data.frame(kp = c("FSW", "MSM", "PWID", "TGW"),
#                   sex = c("female", "male", "both", "female"),
#                   # age_group = c("Y015_049", "Y015_029", "Y015_049", "Y015_029")
#                   age_group = "Y015_049"
#                   ) %>%
#   left_join(
#     long_nd %>%
#       filter(indicator == "population",
#       ) %>%
#       select(iso3, area_id, area_level, sex, age_group, mean) %>%
#       mutate(year = 2021)
#   )


national_matched_genpop <- data.frame(kp = c("FSW", "MSM", "PWID", "TGW"),
                                      sex = c("female", "male", "both", "female"),
                                      # age_group = c("Y015_049", "Y015_029", "Y015_049", "Y015_029")
                                      age_group = "Y015_049"
                                      ) %>%
  left_join(
    long_nd %>%
      filter(
        area_level == 0,
        indicator %in% c("prevalence", "art_coverage"))
      )

long_nd <- long_nd %>%
  left_join(admin1_lvl) %>%
  filter(area_level == admin1_level)

areas <- areas %>%
  arrange(iso3) %>%
  left_join(admin1_lvl) %>%
  filter(area_level == admin1_level) %>%
  select(iso3, area_id, geometry) %>%
  mutate(id.area = row_number())

# nb <- areas %>%
#   spdep::poly2nb() %>%
#   `names<-`(areas$id.area)
# 
# nb <- lapply(nb, as.integer)
# class(nb) <- "nb"
# spdep::nb2INLA("admin1_level_adj.adj", nb)

genpop_pred <- data.frame(kp = c("FSW", "MSM", "PWID", "TGW"),
                          sex = c("female", "male", "both", "female"),
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

# genpop_pred <- national_matched_genpop %>%
#   left_join(region) %>%
#   mutate(logit_gen_var = logit(mean)) %>%
#   select(region, iso3, kp, area_id, indicator, logit_gen_var)

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
    method + 
    f(id.iso3,
      model = "besag",
      scale.model = TRUE,
      graph = national_adj(),
      hyper = prec.prior
      ) +
    f(id.area, model = "besag",
      scale.model = TRUE,
      graph = admin1_adj(),
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
  
  ident <- prev_inla[ind.effect, ]
  
  qtls <- apply(prev_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
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
  r2_med <- apply(observation_samples, 1, median)
  r2_df <- obs_ident %>%
    select(kp, region, logit_gen_prev, logit_kp_prev) %>%
    mutate(median = r2_med)
  
  
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

# debugonce(fit_prevalence_model)
# fit_prevalence_model("FSW")

prev_mod <- lapply(c("FSW", "PWID"), fit_prevalence_model)

names(prev_mod) <- c("FSW", "PWID")

r2_df <- lapply(prev_mod, "[[", "r2_df") %>%
  bind_rows(.id = "kp")

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
    method + 
    f(id.iso3,
      model = "besag",
      scale.model = TRUE,
      graph = national_adj(),
      hyper = prec.prior
    ) +
    f(id.area, model = "besag",
      scale.model = TRUE,
      graph = admin1_adj(),
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
  
  ident <- prev_inla[ind.effect, ] %>%
    ungroup() %>%
    mutate(idx = row_number())
  
  qtls <- apply(prev_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
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
  r2_med <- apply(observation_samples, 1, median)
  r2_df <- obs_ident %>%
    select(kp, region, logit_gen_prev, logit_kp_prev) %>%
    mutate(median = r2_med)
  
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

# debugonce(fit_msm_tg_prevalence_model)
# fit_msm_tg_prevalence_model()

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
  select(rowname, kp, X0.5quant, X0.025quant, X0.975quant) %>%
  mutate(across(-c(rowname, kp), ~round(.x, 2)),
         text = paste0(X0.5quant, " (", X0.025quant, ", ", X0.975quant, ")"))

prev_fixed %>%
  select(rowname, kp, X0.5quant) %>%
  filter(str_detect(rowname, "WCA") == TRUE) %>%
  mutate(region = "WCA") %>%
  bind_rows(mutate(., region = "ESA", X0.5quant = 0)) %>%
  pivot_wider(names_from = rowname, values_from = X0.5quant) %>%
  bind_rows(filter(., kp == "MSM") %>%
              mutate(kp = "TGW"))

r2_df %>%
  bind_rows(prev_mod$MSM$r2_df) %>%
  group_by(kp, region) %>%
  summarise(r2 = 1 - sum((logit_kp_prev-median)^2)/sum((logit_kp_prev-mean(logit_kp_prev))^2)) %>%
  arrange(region)

prev_mod %>%
  lapply("[[", "prev_random") %>%
  bind_rows(.id = "kp") %>%
  rename(lower = `0.025quant`, median = `0.5quant`, upper = `0.975quant`) %>%
  mutate(across(lower:upper, ~round(.x, 2)),
         prec = paste0(median, " (", lower, ", ", upper, ")")
  ) %>%
  select(kp, prec)
  

#### ART

art_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_final.csv", show_col_types = FALSE)

art_dat <- art_dat %>%
  filter(!study_idx %in% c(19, 28, 28, 34, 57, 62, 103, 118, 119, 119, 121, 138, 209, 212, 219, 253, 254, 256, 267))

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
    (is.na(denominator) | denominator == 0) & kp == "TGW" ~ filter(imp_art_denomin, kp == "TGW")$quant,
    TRUE ~ denominator
  )) %>%
  filter(value < 1,
         provincial_value < 1, ## FIX THIS
         !is.na(provincial_value),
         # method != "self-self"
         ) %>%
  mutate(method = case_when(method == "Self-report" ~ "self-report",
                            method == "vls" ~ "VLS",
                            TRUE ~ method),
          value = ifelse(method == "VLS", value/0.9, value),
          value = ifelse(value > 1, 0.99, value),
         value = ifelse(value ==0, 0.01, value),
         provincial_value = ifelse(provincial_value == 1, 0.99, provincial_value),
         provincial_value = ifelse(provincial_value ==0, 0.01, provincial_value),
         logit_kp_art = logit(value),
         logit_gen_art = logit(provincial_value),
         logit_gen_art2 = logit_gen_art,
         positive = round(value * denominator),
         negative = round(denominator - positive),
         method = ifelse(is.na(method), "self-report", as.character(method))
  ) %>%
  filter(method != "self-report") %>%
  mutate(method = ifelse(method == "VLS", "lab", method)) %>%
  group_by(year, kp, iso3) %>%
  mutate(idx = cur_group_id())

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
    kp = c("FSW", "MSM", "PWID", "TGW")
  ) %>%
    bind_rows(
      df_logit_art %>% filter(!is.na(kp)) %>% arrange(kp)
    ) %>%
    # filter(is.na(kp)) %>%
    mutate(denominator = 1) %>%
    # id.iso3 = ifelse(is.na(iso3), nrow(geographies) + 1, id.iso3),
    # id.area = ifelse(is.na(area_id), nrow(areas) + 1, id.area)) %>%
    bind_rows(int) %>%
    mutate(region = factor(region)) %>%
    ungroup() %>%
    select(iso3, area_id, kp, logit_gen_art, logit_kp_art, obs_iid, method, positive, negative, region, denominator, id.ref, id.iso3, id.area) %>%
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
      f(id.area, model = "besag", scale.model = TRUE, graph = admin1_adj(), hyper = prec.prior) +
      f(id.ref, model = "iid", hyper = prec.prior) +
      f(id.ref.nat, model = "iid", hyper = prec.prior)
    
  } else {
    message("One ART method")
    art_formula <- positive ~ 
      logit_gen_art + 
      f(kp, hyper = prec.prior) +
      f(kp2, logit_gen_art, hyper = prec.prior) +
      f(id.iso3, model = "besag", scale.model = TRUE, graph = national_adj(), hyper = prec.prior) +
      f(id.area, model = "besag", scale.model = TRUE, graph = admin1_adj(), hyper = prec.prior) +
      f(id.ref, model = "iid", hyper = prec.prior) +
      f(id.ref.nat, model = "iid", hyper = prec.prior)
  }
  
  art_fit <- INLA::inla(art_formula,
                        data = art_inla %>% mutate(kp2 = kp),
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
  
  ident <- art_inla[ind.effect, ]
  
  qtls <- apply(art_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
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
  r2_med <- apply(observation_samples, 1, median)
  r2_df <- obs_ident %>%
    select(kp, logit_gen_art, logit_kp_art) %>%
    mutate(median = r2_med)
  
  out <- list()
  out$art <- art
  out$art_samples <- art_samples
  out$art_fixed <- art_fit$summary.fixed
  out$art_random <- art_fit$summary.hyperpar
  out$r2_df <- r2_df
  out
  
}

art_mod <- fit_art_model_all()

art_res <- art_mod$art

# art_res_all <- art_mod$art
# art_res_all_coeff <- art_mod$art_fixed
# 
# art_res_lab_only <- art_mod$art
# art_res_lab_only_coeff <- art_mod$art_fixed

art_list <- list()
art_list$FSW <- list()
art_list$MSM <- list()
art_list$PWID <- list()
art_list$TGW <- list()

art_fsw_idx <- which(art_res$kp == "FSW")
art_msm_idx <- which(art_res$kp == "MSM")
art_pwid_idx <- which(art_res$kp == "PWID")
art_tgw_idx <- which(art_res$kp == "TGW")

art_list$FSW$art_samples <- art_mod$art_samples[art_fsw_idx,]
art_list$MSM$art_samples <- art_mod$art_samples[art_msm_idx,]
art_list$PWID$art_samples <- art_mod$art_samples[art_pwid_idx,]
art_list$TGW$art_samples <- art_mod$art_samples[art_tgw_idx,]

art_list$FSW$art <- art_mod$art %>% filter(kp == "FSW")
art_list$MSM$art <- art_mod$art %>% filter(kp == "MSM")
art_list$PWID$art <- art_mod$art %>% filter(kp == "PWID")
art_list$TGW$art <- art_mod$art %>% filter(kp == "TGW")

art_mod$art_random %>%
  rename(lower = `0.025quant`, median = `0.5quant`, upper = `0.975quant`) %>%
  mutate(across(lower:upper, ~round(.x, 2)),
         prec = paste0(median, " (", lower, ", ", upper, ")")
  ) %>%
  select(prec)

art_mod$r2_df %>%
  group_by(kp) %>%
  summarise(r2 = 1 - sum((logit_kp_art-median)^2)/sum((logit_kp_art-mean(logit_kp_art))^2))
  

### Self report sensitivity analysis
# 
# {
#   
#   art_sens <- art_res_all %>%
#     filter(is.na(iso3)) %>%
#     mutate(method = "Diagnostically-confirmed + self-report ART status data") %>%
#     bind_rows(
#       art_res_lab_only %>%
#         filter(is.na(iso3)) %>%
#         mutate(method = "Diagnostically-confirmed ART status only")
#     )
# 
#   art_sens_fig <- art_sens %>%
#     bind_rows() %>%
#     mutate(kp = ifelse(kp == "TG", "TGW", kp)) %>% name_kp(F) %>%
#     # filter(model == "betabinomial") %>%
#     ggplot(aes(x = logit_gen_art, y=median)) +
#     geom_point(data = art_df %>%
#                  filter(kp %in% c("MSM", "PWID", "FSW", "TG")) %>%
#                  mutate(kp = ifelse(kp == "TG", "TGW", kp)) %>% name_kp(F) %>%
#                  left_join(region) %>%
#                  name_region(F) %>%
#                  mutate(method = ifelse(method == "self-report", "Self-report ART status", "Diagnostically-confirmed ART status")),
#                aes(x = qlogis(provincial_value), y=qlogis(value), shape = method), alpha = 0.6, size=2.5) +
#     geom_line(size=1, aes(color = method)) +
#     geom_ribbon(aes(ymin = lower, ymax = upper, fill = method), alpha=0.2, show.legend = F) +
#     geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
#     moz.utils::standard_theme() +
#     scale_y_continuous(labels = convert_logis_labels, limits = c(logit(0.05), logit(0.95))) +
#     scale_x_continuous(labels = convert_logis_labels, limits = c(logit(0.125), logit(0.95))) +
#     scale_manual("color", 2) +
#     scale_manual("fill", 2) +
#     scale_shape_manual(values = c(16, 2)) +
#     labs(y = "KP ART coverage (logit scale)", x = "Total population ART coverage (logit scale)", color = "Modelled estimate", shape = "Data")+
#     theme(panel.border = element_rect(fill=NA, color="black"),
#           panel.spacing = unit(2, "lines"),
#           axis.text = element_text(size = 14),
#           legend.title = element_text(face = "bold", size = 16),
#           legend.text = element_text(size = 16),
#           legend.direction = "vertical",
#           legend.title.align = 0.5,
#           strip.text = element_text(face="bold")) +
#     facet_wrap(~kp, nrow=2)
# 
#   png("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/S3 ART sensitivity.png", width = 800, height = 900)
#   art_sens_fig
#   dev.off()
# }

# data.frame(art_fit$summary.random$kp2) %>%
#   select(X0.5quant, X0.025quant, X0.975quant) %>%
#   mutate(across(everything(), ~round(.x, 2)),
#          text = paste0(X0.5quant, " (", X0.025quant, ", ", X0.975quant, ")"))
# 
# data.frame(art_fit$summary.fixed) %>%
#   select(X0.5quant, X0.025quant, X0.975quant) %>%
#   mutate(across(everything(), ~round(.x, 2)),
#          text = paste0(X0.5quant, " (", X0.025quant, ", ", X0.975quant, ")"))
# 
# summary(art_fit)

# debugonce(fit_art_model)
# fit_art_model("FSW")

####### PSE

pse_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final_sourced.csv")

pse_dat <- pse_dat %>%
  mutate(iso3 = countrycode(country.name, "country.name", "iso3c")) %>%
  mutate(
    fe_method = case_when(
      str_detect(method, "methods") ~ "other methods",
      method == "PLACE/Mapping" ~ method,
      TRUE ~ "empirical"
    ),
    kp = ifelse(kp == "TG", "TGW", kp)
  )

pse_dat <- pse_dat %>%
  filter(prop_estimate != 0, !is.na(prop_estimate), prop_estimate < 1) %>%
  left_join(region %>% dplyr::select(region, iso3)) %>%
  mutate(
    logit_proportion = logit(prop_estimate),
    fe_method = factor(fe_method, levels=c("empirical", unique(pse_dat$fe_method)[unique(pse_dat$fe_method) != "empirical" & !is.na(unique(pse_dat$fe_method))])),
  ) %>%
  ungroup %>%
  dplyr::select(iso3, area_id = province_area_id, year, kp, fe_method, method, logit_proportion, prop_estimate, study_idx) %>%
  filter(iso3 != "LBR",
         !(iso3 == "BFA" & kp == "PWID"))

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

fit_pse_model <- function(kp_id) {
    
    pse_inla <- df_logit_prev %>%
      filter(!is.na(area_id)) %>%
      distinct(iso3, area_id) %>%
      bind_rows(pse_dat %>%
                  filter(kp == kp_id) %>%
                  group_by(study_idx) %>%
                  mutate(id.ref = cur_group_id(),
                         id.ref = ifelse(is.na(study_idx), NA, id.ref)) %>%
                  ungroup %>%
                  arrange(fe_method) %>%
                  mutate(id.method = as.numeric(fct_inorder(method)))) %>%
      left_join(areas %>% st_drop_geometry()) %>%
      left_join(geographies %>% select(iso3, id.iso3) %>% st_drop_geometry()) %>%
      mutate(area_id = ifelse(is.na(area_id), iso3, area_id)) %>%
      dplyr::select(iso3, id.iso3, area_id, id.area, logit_proportion, fe_method, id.method, method, id.ref) %>%
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
    pse_formula <- logit_proportion ~ 
      f(id.area, model = "besag", scale.model = TRUE, graph = admin1_adj(), hyper=prec.prior) +
      f(id.iso3, model = "besag", scale.model = TRUE, graph = national_adj(), hyper=prec.prior) +
      fe_method +
      f(id.method, model = "iid", hyper = prec.prior) +
      f(id.ref, model = "iid", hyper = prec.prior) +
      # f(id.iso3, model = "iid", hyper = prec.prior) +
      f(id.ref.nat, model = "iid", hyper = prec.prior)
    
    if(kp_id == "TGW")
      pse_formula <- logit_proportion ~ 
        f(id.area, model = "besag", scale.model = TRUE, graph = admin1_adj(), hyper=prec.prior) +
        f(id.iso3, model = "besag", scale.model = TRUE, graph = national_adj(), hyper=list(prec= list(prior = "normal", param = c(1.6, 0.8)))) +
        fe_method +
        f(id.method, model = "iid", hyper = prec.prior) +
        f(id.ref, model = "iid", hyper = prec.prior)
    
    
    pse_fit <- INLA::inla(pse_formula,
                          data = pse_inla,
                          family = "gaussian", 
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
    
    ident <- pse_inla[ind.effect, ]
    
    qtls <- apply(pse_samples, 1, quantile, c(0.025, 0.5, 0.975))
    
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
    
    sm <- function(x,y) {
      random_marginal <- eval(parse(text = paste0("pse_fit$marginals.random$id.method$index.", x)))
      random_samples <- inla.rmarginal(1000, random_marginal)
      
      if(y != "empirical") {
        fixed_marginal <- eval(parse(text = paste0("pse_fit$marginals.fixed$`fe_method", y, "`")))
        fixed_samples <- inla.rmarginal(1000, fixed_marginal)
      } else {
        fixed_samples <- rep(0, 1000)
      }
      
      out <- random_samples + fixed_samples
    }
    
    samples <- map2(extract_df$id.method, as.character(extract_df$fe_method), ~sm(.x, .y))
    samples <- matrix(unlist(samples), nrow = nrow(extract_df), byrow = TRUE)
    
    qtls <- t(apply(samples, 1, quantile, c(0.025, 0.5, 0.975)))
    
    pse_total_method <- extract_df %>%
      mutate(lower = qtls[,1],
             median = qtls[,2],
             upper = qtls[,3])
    
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
    out$pse_total_method <- pse_total_method
    out$pse_random <- pse_random
    out$pse_fixed <- pse_fixed
    out
    
}

# debugonce(fit_pse_model)
# fit_pse_model("TGW")

pse_mod <- lapply(c("FSW", "MSM", "PWID", "TGW"), fit_pse_model)

names(pse_mod) <- c("FSW", "MSM", "PWID", "TGW")

pse_res <- lapply(pse_mod, "[[", "pse") %>%
  bind_rows(.id = "kp")

pse_fixed <- lapply(pse_mod, "[[", "pse_fixed") %>%
  bind_rows(.id = "kp")

pse_total_method <- lapply(pse_mod, "[[", "pse_total_method") %>%
  bind_rows(.id = "kp")

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
           str_detect(fe_method, "PLACE/Mapping") ~ "PLACE/Mapping",
         )) %>%
  select(-c(lower:upper))

pse_random %>%
  left_join(pse_fixed) %>%
  left_join(pse_total_method %>%
              mutate(across(lower:upper, ~round(.x, 2)),
                     total = paste0(median, " (", lower, ", ", upper, ")")
              ) %>%
              select(-c(lower:upper))
              ) %>%
  arrange(fe_method, method) %>%
  select(kp, method, fixed, random, total) %>%
  left_join(pse_dat %>% 
              distinct(kp, method, study_idx) %>% 
              count(kp, method) %>%
              rename(study = n)) %>%
  left_join(pse_dat %>%
              count(kp, method) %>%
              rename(datapoint = n)) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/ST5 PSE method.csv")

#######

data.frame(kp = c("FSW", "MSM", "PWID", "TGW"),
                             sex = c("female", "male", "both", "female"),
                             # age_group = c("Y015_049", "Y015_029", "Y015_049", "Y015_029")
                             age_group = "Y015_049"
                             )

scale_pop_id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022 && parameter:source == "WPP2022")'), draft = FALSE)
})

pop <- lapply(paste0("archive/aaa_scale_pop/", scale_pop_id, "/interpolated_population.csv"), read_csv, show_col_types = F) %>%
  bind_rows() %>%
  filter(year == 2021) %>%
  separate(area_id, into = c("iso3", NA), sep = 3, remove = F) %>%
  separate(area_id, into = c(NA, "area_level", NA), sep = "_", remove = F) %>%
  mutate(area_level = ifelse(is.na(area_level), 0, area_level)) %>%
  left_join(admin1_lvl) %>%
  filter(area_level == admin1_level) %>%
  five_year_to_15to49("population") %>%
  sex_aggregation("population")

pop_l <- pop %>%
  left_join(areas %>% select(area_id, id.area) %>% st_drop_geometry()) %>%
  arrange(id.area) %>%
  left_join(region) %>%
  left_join(kp_to_sex() %>% filter(kp %in% c("FSW", "MSM", "PWID", "TGW"))) %>%
  group_by(kp) %>%
  group_split() %>%
  setNames(c("FSW", "MSM", "PWID", "TGW"))

id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_urban_proportion", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022)'), draft = FALSE)
})

paths <- paste0("archive/aaa_urban_proportion/", id, "/urban_proportion.csv")

urban_proportion <- lapply(paths, read_csv, show_col_types = FALSE) %>%
  bind_rows()

urban_proportion <- areas %>%
  st_drop_geometry() %>%
  left_join(urban_proportion) %>%
  mutate(urban_proportion = ifelse(is.na(urban_proportion), 0.5, urban_proportion)) ## FIX THESE AREAS

kplhiv_art <- Map(function(prev, pse, art, pop) {
  # 
  # prev_s <- prev_mod$TGW$prev_samples[which(!is.na(prev_mod$TGW$prev$area_id)),]
  # pse_s <- pse_mod$TGW$pse_samples
  # art_s <- art_list$TGW$art_samples[which(!is.na(art_list$TGW$art$area_id)),]
  # ids <- art_list$TGW$art %>% filter(!is.na(area_id)) %>% pull(area_id)
  # pop <- pop_l$TGW
  # pop <- pop[match(ids, pop$area_id), ]
  # stopifnot(all.equal(prev_mod$TGW$prev[which(!is.na(prev_mod$TGW$prev$area_id)),] %>% pull(area_id), art_list$TGW$art[which(!is.na(art_list$TGW$art$area_id)),] %>% pull(area_id)))
  # stopifnot(all.equal(pse_mod$TGW$pse$area_id, prev_mod$TGW$prev[which(!is.na(prev_mod$TGW$prev$area_id)),] %>% pull(area_id)))
  # stopifnot(all.equal(pop$area_id, prev_mod$TGW$prev[which(!is.na(prev_mod$TGW$prev$area_id)),] %>% pull(area_id)))
  # stop("Don't be an idiot")

  prev_s <- prev$prev_samples[which(!is.na(prev$prev$area_id)),]
  pse_s <- pse$pse_samples
  art_s <- art$art_samples[which(!is.na(art$art$area_id)),]
  ids <- art$art %>% filter(!is.na(area_id)) %>% pull(area_id)
  pop <- pop[match(ids, pop$area_id), ]
  prev_continuous_results <- prev$prev %>% filter(is.na(iso3))
  art_continuous_results <- art$art %>% filter(is.na(iso3))

  stopifnot(all.equal(prev$prev[which(!is.na(prev$prev$area_id)),] %>% pull(area_id), art$art[which(!is.na(art$art$area_id)),] %>% pull(area_id)))
  stopifnot(all.equal(pse$pse$area_id, prev$prev[which(!is.na(prev$prev$area_id)),] %>% pull(area_id)))
  stopifnot(all.equal(pop$area_id, prev$prev[which(!is.na(prev$prev$area_id)),] %>% pull(area_id)))
  

  urban_prop_s <- matrix(rep(rbeta(1000, 5, 3), nrow(pse_s)), nrow = nrow(pse_s), byrow = TRUE)
  rural_pse_s <- invlogit(pse_s) * urban_prop_s

  pse_count_samples <- (invlogit(pse_s) * pop$population * urban_proportion$urban_proportion) + (pop$population * rural_pse_s * (1-urban_proportion$urban_proportion))
  kplhiv_samples <- invlogit(prev_s) * pse_count_samples
  kpart_samples <- kplhiv_samples * invlogit(art_s)
  
  urban_pop <- pop %>%
    left_join(urban_proportion) %>%
    mutate(urban_pop = population * urban_proportion)
  
  # kplhiv_qtls <- apply(kplhiv_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
  df <- df_logit_prev %>%
    filter(!is.na(area_id)) %>%
    select(iso3, area_id, region) %>%
    distinct()
  
  pse_total_count <- df %>%
    mutate(indicator = "pse_total_count") %>%
    cbind(pse_count_samples)
  
  pse <- df %>%
    mutate(indicator = "pse_urban") %>%
    cbind(invlogit(pse_s))
  
  pse_urban_count <- df %>%
    mutate(indicator = "pse_urban_count") %>%
    cbind(invlogit(pse_s) * urban_pop$urban_pop)
  
  plhiv <- df %>%
    mutate(indicator = "kplhiv") %>%
    cbind(kplhiv_samples)
  
  prev <- df %>%
    mutate(indicator = "prev") %>%
    cbind(invlogit(prev_s))
  
  art <- df %>%
    mutate(indicator = "kpart") %>%
    cbind(kpart_samples)
  
  art_cov <- df %>%
    mutate(indicator = "art_coverage") %>%
    cbind(invlogit(art_s))
  
  region_res <- bind_rows(pse_total_count, pse_urban_count, plhiv, art) %>%
    group_by(indicator, region) %>%
    summarise(across(as.character(1:1000), sum)) %>%
    bind_rows(
      bind_rows(pse_total_count, pse_urban_count, plhiv, art) %>%
        group_by(indicator) %>%
        summarise(across(as.character(1:1000), sum)) %>%
        mutate(region = "SSA")
    ) %>%
    ungroup()
  
  kplhiv_samples_region <- region_res %>%
    filter(indicator == "kplhiv") %>%
    select(all_of(as.character(1:1000))) %>%
    as.matrix()
  
  kpart_samples_region <- region_res %>%
    filter(indicator == "kpart") %>%
    select(all_of(as.character(1:1000))) %>%
    as.matrix()
  
  pse_count_samples_region <- region_res %>%
    filter(indicator == "pse_total_count") %>%
    select(all_of(as.character(1:1000))) %>%
    as.matrix()
  
  pse_urban_count_samples_region <- region_res %>%
    filter(indicator == "pse_urban_count") %>%
    select(all_of(as.character(1:1000))) %>%
    as.matrix()
  
  prev_samples_region <- kplhiv_samples_region/pse_count_samples_region
  art_cov_samples_region <- kpart_samples_region/kplhiv_samples_region
  
  urban_pop_region <- urban_pop %>% 
    left_join(region) %>% 
    group_by(region) %>% 
    summarise(urban_pop = sum(urban_pop)) %>% 
    bind_rows(mutate(., region = "SSA") %>% 
                group_by(region) %>% 
                summarise(urban_pop = sum(urban_pop))
              ) %>%
    pull(urban_pop)
  
  pse_urban_region_samples <- pse_urban_count_samples_region/urban_pop_region
  
  region_pop <- pop %>% 
    group_by(region) %>% 
    summarise(population = sum(population)) %>%
    ungroup() %>%
    bind_rows(mutate(., region = "SSA") %>%
                group_by(region) %>%
                summarise(population = sum(population))) %>%
    pull(population)
  
  pse_samples_region <- pse_count_samples_region/region_pop
  
  region_res <- region_res %>%
    bind_rows(
      data.frame(indicator = "prev",
             region = c("ESA", "WCA", "SSA")) %>%
        cbind(prev_samples_region),
      data.frame(indicator = "pse_urban",
                 region = c("ESA", "WCA", "SSA")) %>%
        cbind(pse_urban_region_samples),
      data.frame(indicator = "art_cov",
                 region = c("ESA", "WCA", "SSA")) %>%
        cbind(art_cov_samples_region),
      data.frame(indicator = "pse",
                 region = c("ESA", "WCA", "SSA")) %>%
        cbind(pse_samples_region)
    )

  region_qtls <- apply(region_res[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))
  
  region_res <- region_res %>%
    select(indicator, region) %>%
    # mutate(mean = rowMeans(region_res[as.character(1:1000)])) %>%
    cbind(data.frame(t(region_qtls)))
  
  colnames(region_res) <- c("indicator", "region", "lower", "median", "upper")
  
  #### Country res
  
  country_res <- bind_rows(pse_total_count, pse_urban_count, plhiv, art) %>%
    group_by(indicator, iso3) %>%
    summarise(across(as.character(1:1000), sum)) %>%
    ungroup()
  
  pse_count_samples_nat <- filter(country_res, indicator == "pse_total_count") %>%
    select(all_of(as.character(1:1000))) %>%
    as.matrix()
  
  pse_urban_count_samples_nat <- filter(country_res, indicator == "pse_urban_count") %>%
    select(all_of(as.character(1:1000))) %>%
    as.matrix()
  
  kplhiv_samples_nat <- filter(country_res, indicator == "kplhiv") %>%
    select(all_of(as.character(1:1000))) %>%
    as.matrix()
  
  kpart_samples_nat <- filter(country_res, indicator == "kpart") %>%
    select(all_of(as.character(1:1000))) %>%
    as.matrix()
  
  nat_pop <- urban_pop %>%
    group_by(iso3) %>%
    summarise(nat_pop = sum(population),
              nat_urban_pop = sum(urban_pop))
  
  pse_samples_nat <- pse_count_samples_nat / nat_pop$nat_pop
  pse_urban_samples_nat <- pse_urban_count_samples_nat / nat_pop$nat_urban_pop
  prev_samples_nat <- kplhiv_samples_nat / pse_count_samples_nat
  art_cov_samples_nat <- kpart_samples_nat / kplhiv_samples_nat
  
  nat_val <- crossing(select(country_res, iso3),
                      indicator = c("pse_nat", "pse_urban", "prev", "art_cov")) %>%
    arrange(indicator) %>%
    cbind(rbind(art_cov_samples_nat, prev_samples_nat, pse_samples_nat, pse_urban_samples_nat))
  
  country_res <- country_res %>% bind_rows(nat_val)
  
  country_qtls <- apply(country_res[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))
  
  country_res <- country_res %>%
    select(indicator, iso3) %>%
    # mutate(mean = rowMeans(country_res[as.character(1:1000)])) %>%
    cbind(data.frame(t(country_qtls)))
  
  colnames(country_res) <- c("indicator", "iso3", "lower", "median", "upper")
  
  ### Area res
  
  area_res <- bind_rows(pse_total_count, plhiv, art, pse, prev, art_cov)
  
  area_qtls <- apply(area_res[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))
  
  area_res <- area_res %>%
    select(iso3, area_id, indicator) %>%
    # mutate(mean = rowMeans(area_res[as.character(1:1000)])) %>%
    cbind(t(area_qtls))
  
  colnames(area_res) <- c("iso3", "area_id", "indicator", "lower", "median", "upper")
  
  # country_res <- bind_rows(pse_count, plhiv, art, pse, prev, art_cov)
  # 
  # country_qtls <- apply(country_res[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))
  # 
  # country_res <- country_res %>%
  #   select(iso3, indicator) %>%
  #   cbind(t(country_qtls))
  # 
  # colnames(country_res) <- c("iso3", "indicator", "lower", "median", "upper")
  
  out <- list()
  out$region <- region_res
  out$country <- country_res
  out$area <- area_res
  out$prev_continuous_results <- prev_continuous_results
  out$art_continuous_results <- art_continuous_results
  out
  
  }, prev_mod[c("FSW", "MSM", "PWID", "TGW")], pse_mod, art_list, pop_l) %>%
  setNames(c("FSW", "MSM", "PWID", "TGW"))

saveRDS(kplhiv_art, "~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/kplhiv_art.rds")

########################

prev_s <- rbind(prev_mod$FSW$prev_samples[which(!is.na(prev_mod$FSW$prev$area_id)),],
                prev_mod$MSM$prev_samples[which(!is.na(prev_mod$MSM$prev$area_id)),],
                prev_mod$PWID$prev_samples[which(!is.na(prev_mod$PWID$prev$area_id)),],
                prev_mod$TGW$prev_samples[which(!is.na(prev_mod$TGW$prev$area_id)),])

pse_s <- rbind(pse_mod$FSW$pse_samples, pse_mod$MSM$pse_samples, pse_mod$PWID$pse_samples, pse_mod$TGW$pse_samples)

art_s <- rbind(art_list$FSW$art_samples[which(!is.na(art_list$FSW$art$area_id)),],
               art_list$MSM$art_samples[which(!is.na(art_list$MSM$art$area_id)),],
               art_list$PWID$art_samples[which(!is.na(art_list$PWID$art$area_id)),],
               art_list$TGW$art_samples[which(!is.na(art_list$TGW$art$area_id)),])




pop_curr <- bind_rows(pop_l[[1]], pop_l[[2]], pop_l[[3]], pop_l[[4]]) %>%
  filter(area_id != "MOZ_1_10")

urban_prop_s <- matrix(rep(rbeta(1000, 5, 3), nrow(pse_s)), nrow = nrow(pse_s), byrow = TRUE)
# urban_prop_s <- matrix(rbeta(length(pse_s), 5, 3), nrow = nrow(pse_s))
rural_pse_s <- invlogit(pse_s) * urban_prop_s

pse_count_samples <- (invlogit(pse_s) * pop_curr$population * urban_proportion$urban_proportion) + (pop_curr$population * rural_pse_s * (1-urban_proportion$urban_proportion))
kplhiv_samples <- invlogit(prev_s) * pse_count_samples
kpart_samples <- kplhiv_samples * invlogit(art_s)

# kplhiv_qtls <- apply(kplhiv_samples, 1, quantile, c(0.025, 0.5, 0.975))

df <- df_logit_prev %>%
  filter(!is.na(area_id)) %>%
  select(kp, iso3, area_id, region) %>%
  arrange(kp, area_id)

pse_count <- df %>%
  mutate(indicator = "pse_count") %>%
  cbind(pse_count_samples)

plhiv <- df %>%
  mutate(indicator = "kplhiv") %>%
  cbind(kplhiv_samples)

prev <- df %>%
  mutate(indicator = "prev") %>%
  cbind(prev_s)

art <- df %>%
  mutate(indicator = "kpart") %>%
  cbind(kpart_samples)

# unaids_sex_num <- bind_rows(
#   read.csv("R/Model/People living with HIV_People living with HIV - Adults (15-49)_Population All adults (15-49).csv") %>% mutate(sex = "both"),
#   read.csv("R/Model/People living with HIV_People living with HIV - Adults (15-49)_Population Female adults (15-49).csv") %>% mutate(sex = "female"),
#   read.csv("R/Model/People living with HIV_People living with HIV - Adults (15-49)_Population Male adults (15-49).csv") %>% mutate(sex = "male")
# ) %>%
#   mutate(indicator = "plhiv")
# 
# unaids_sex_num <- unaids_sex_num %>%
#   select(!contains(c("lower", "upper", "Footnote"))) %>%
#   mutate(iso3 = countrycode::countrycode(Country, "country.name", "iso3c")) %>%
#   filter(iso3 %in% ssa_iso3) %>%
#   select(-Country) %>%
#   pivot_longer(-c(iso3, indicator, sex)) %>%
#   mutate(year = str_remove(name, "X"),
#          value = str_remove_all(value, " ")) %>%
#   group_by(iso3, indicator, sex) %>%
#   fill(value, .direction = "down") %>%
#   filter(year == "2021", !(iso3 == "ZAF" & indicator == "art")) %>%
#   type_convert() %>%
#   select(-name) %>%
#   rename(tot_plhiv = value)

# pop %>%
#   filter(iso3 == area_id) %>%
#   group_by(iso3, kp, sex) %>%
#   summarise(population = sum(mean)) %>%
#   left_join(unaids_sex_num) %>%
#   group_by(sex) %>%
#   summarise(tot_prev = sum(tot_plhiv)/sum(population))

region_res <- bind_rows(pse_count, plhiv, art) %>%
  group_by(indicator, region) %>%
  summarise(across(as.character(1:1000), sum)) %>%
  bind_rows(
    bind_rows(pse_count, plhiv, art) %>%
      group_by(indicator) %>%
      summarise(across(as.character(1:1000), sum)) %>%
      mutate(region = "SSA")
  )

region_res <- region_res %>%
  bind_rows(
    region_res %>%
      group_by(region) %>%
      summarise(across(as.character(1:1000), ~.x[indicator == "kpart"]/.x[indicator == "kplhiv"])) %>%
      mutate(indicator = "art_cov")
  )

region_qtls <- apply(region_res[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))

region_res <- region_res %>%
  select(indicator, region) %>%
  ungroup() %>%
  mutate(mean = rowMeans(region_res[as.character(1:1000)])) %>%
  cbind(data.frame(t(region_qtls)))

colnames(region_res) <- c("indicator", "region", "mean", "lower", "median", "upper")

bind_rows(
  region_res %>%
    filter(indicator == "pse_count") %>%
    left_join(ssa_15to49 %>% select(region, denominator = totpop)),
  region_res %>%
    filter(indicator == "kplhiv") %>%
    left_join(ssa_15to49 %>% select(region, denominator = hivpop))
)  %>%
  mutate(across(mean:upper, ~round(.x*100/denominator, 1))) %>%
  factor_region() %>%
  arrange(region) %>%
  select(indicator, region, mean, lower, upper)

kp_res <- bind_rows(pse_count, plhiv, art) %>%
  group_by(indicator, kp) %>%
  summarise(across(as.character(1:1000), sum))

kp_qtls <- apply(kp_res[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))

kp_res <- kp_res %>%
  select(indicator, kp) %>%
  ungroup() %>%
  mutate(mean = rowMeans(kp_res[as.character(1:1000)])) %>%
  cbind(data.frame(t(kp_qtls)))

colnames(kp_res) <-c("indicator", "region", "mean", "lower", "median", "upper")

bind_rows(
  kp_res %>%
    filter(indicator == "pse_count") %>%
    mutate(denominator = filter(ssa_15to49, region == "SSA")$totpop),
  kp_res %>%
    filter(indicator == "kplhiv") %>%
    mutate(denominator = filter(ssa_15to49, region == "SSA")$hivpop) 
  ) %>%
  # mutate(across(mean:upper, ~round(.x*100/denominator, 1))) %>%
  mutate(across(mean:upper, ~signif(.x, 2))) %>% 
  select(indicator, region, mean, lower, upper) %>%
  filter(indicator == "kplhiv")
  

kplhiv_art %>%
  lapply("[[", "region") %>%
  bind_rows(.id = "kp") %>%d
  filter(indicator == "prev", region == "SSA")

kplhiv_art %>%
  lapply("[[", "country") %>%
  bind_rows(.id = "kp") %>%
  filter(indicator == "pse_nat", kp == "FSW") %>%
  summarise(median = median(mean))

spec_dat %>%
  bind_rows(.id = "kp") %>%
  filter(age %in% 15:49,
         year == 2021) %>%
  bind_rows(mutate(., sex = "both")) %>%
  group_by(sex) %>%
  summarise(prev = sum(hivpop)/sum(totpop)) %>%
  left_join(kplhiv_art %>%
              lapply("[[", "region") %>%
              bind_rows(.id = "kp") %>%
              filter(indicator == "prev", region == "SSA") %>%
              left_join(kp_to_sex())) %>%
  mutate(ratio = median/prev) %>%
  arrange(kp)

######

kplhiv_art %>%
  lapply("[[", "country") %>%
  bind_rows(.id = "kp") %>%
  filter(indicator == "pse_nat") %>%
  group_by(kp) %>%
  reframe(calculate_quantile(median))


####
# 
# prev_res %>%
#   mutate(across(lower:upper, invlogit)) %>%
#   filter(is.na(iso3)) %>%
#   left_join(region) %>%
#   ggplot(aes(x=invlogit(logit_gen_prev), y=median)) +
#     geom_line(aes(group = region)) +
#     geom_ribbon(aes(ymax = upper, ymin = lower, group =region), alpha = 0.4) +
#     geom_pointrange(data = kplhiv_art %>%
#                       lapply("[[", "country") %>%
#                       bind_rows(.id = "kp") %>%
#                       filter(indicator == "prev") %>%
#                       left_join(national_matched_genpop %>% filter(indicator == "prevalence") %>% select(-c(indicator, lower, median, upper))),
#                     aes(ymax = upper, ymin = lower, x= mean, color = iso3)) +
#     facet_wrap(~kp)

# art_res %>%
#   filter(is.na(iso3)) %>%
#   mutate(source = "Without method fixed effect") %>%
#   bind_rows(foo %>% filter(is.na(iso3)) %>% mutate(source = "With method fixed effect")) %>%
#   mutate(across(lower:upper, invlogit)) %>%
#   ggplot(aes(x=invlogit(logit_gen_art), y=median)) +
#   # geom_pointrange(aes(ymax = upper, ymin = lower, color=iso3)) +
#     geom_line(aes(color=source), size=1) +
#     geom_ribbon(aes(ymin = lower, ymax = upper, fill=source), alpha = 0.2) +
#     geom_point(data = art_df %>% filter(kp != "TG"), aes(y=value)) +
#     geom_abline(aes(intercept=0, slope=1), linetype =3 ) +
#     facet_wrap(~kp) +
#     standard_theme() +
#     scale_y_continuous(labels = scales::label_percent()) +
#     scale_x_continuous(labels = scales::label_percent()) +
#     scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
#     scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
#     labs(y="KP ART coverage", x = "Total population ART coverage", color=element_blank(), fill=element_blank())




remaining_num <- kplhiv_art %>%
  lapply("[[", "country") %>%
  bind_rows(.id = "kp") %>%
  filter(indicator == "kplhiv") %>%
  group_by(iso3, indicator) %>%
  summarise(mean = sum(mean)) %>%
  left_join(
    spec_dat %>%
      bind_rows() %>%
      filter(year == 2021) %>%
      group_by(iso3) %>%
      summarise(hivpop = sum(hivpop))
  ) %>%
  mutate(mean = hivpop - mean,
         kp = "Remainder") %>%
  select(iso3, kp, indicator, mean)

plot_order <- c("SEN", "GMB", "GNB", "GIN", "SLE", "LBR", "MLI", "BFA", "CIV", "GHA", "TGO", "BEN", "NER", "NGA", "CMR", "TCD", "CAF", "SSD", "ERI", "ETH", "GAB", "COG", "GNQ", "COD", "UGA", "KEN", "RWA", "BDI", "TZA", "AGO", "ZMB", "MWI", "MOZ", "BWA", "ZWE", "NAM", "SWZ", "LSO", "ZAF")

# kplhiv_art <- readRDS("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/kplhiv_art.rds")

kplhiv_proportion_plot <- kplhiv_art %>%
  lapply("[[", "country") %>%
  bind_rows(.id = "kp") %>%
  filter(indicator == "kplhiv") %>%
  bind_rows(remaining_num) %>%
  group_by(iso3) %>%
  mutate(mean = mean/sum(mean)) %>%
  filter(kp != "Remainder") %>%
  name_kp(F) %>%
  # filter(iso3 != "ZAF",
  #        indicator == "kplhiv") %>%
  ggplot(aes(x=fct_rev(fct_relevel(iso3, plot_order)), y=mean, fill=fct_rev(kp))) +
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

kplhiv_plot <- kplhiv_art %>%
  lapply("[[", "region") %>%
  bind_rows(.id = "kp") %>%
  name_kp(F) %>%
  filter(indicator == "kplhiv", region != "SSA")%>%
  name_region(F) %>%
  ggplot(aes(x=region, y=mean, group=kp, fill=kp)) +
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

write_csv(prev_fixed, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_fixed.csv")

#####

write_csv(prev_res %>%
  filter(is.na(iso3)) %>%
  mutate(across(lower:upper, invlogit)), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_estimates.csv")

write_csv(kplhiv_art %>%
            lapply("[[", "country") %>%
            bind_rows(.id = "kp") %>%
            filter(indicator == "prev") %>% 
            left_join(
              national_matched_genpop %>% 
                filter(indicator == "prevalence") %>% 
                select(iso3, kp, provincial_value = median)
              ), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_national_matched_estimates.csv")

######

write_csv(pse_fixed, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_fixed.csv")

write_csv(kplhiv_art %>%
            lapply("[[", "country") %>%
            bind_rows(.id = "kp") %>%
            filter(indicator == "pse_urban"), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_estimates.csv")

#####

write_csv(art_res %>%
            mutate(across(lower:upper, invlogit)) %>%
            filter(is.na(iso3)), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_estimates.csv")

write_csv(kplhiv_art %>%
            lapply("[[", "country") %>%
            bind_rows(.id = "kp") %>%
            filter(indicator == "art_cov") %>% 
            left_join(
              national_matched_genpop %>% 
                filter(indicator == "art_coverage") %>% 
                select(iso3, kp, provincial_value = median)
            ), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_national_matched_estimates.csv")


#########
pse_fixed %>%
  full_join(pse_dat %>%
              count(kp, method)) %>%
  full_join(pse_dat %>%
              distinct(kp, ref, method) %>%
              count(kp, method) %>%
              rename(n_studies = n)) %>%
  filter(kp != "TG", !is.na(method)) %>%
  mutate(across(c(lower:upper), exp),
         est = paste0(round(median,2), " (", round(lower,2), "-", round(upper,2),")"),
         # est = ifelse(method == "3S-CRC", 1, est),
         method = fct_relevel(method, 
                              c("3S-CRC", "2S-CRC", "SS-PSE", "Object multiplier", "Event multiplier", "Service multiplier", "PLACE/Mapping",  "Multiple methods - empirical", "Multiple methods - mixture"))
  ) %>%
  arrange(method) %>%
  dplyr::select(method, kp, est, n_studies, n) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Figs/Table 2 pse method.csv")


#3##

msm_15to29 <- prev_res %>%
  mutate(median = round(invlogit(median), 3),
         lower = round(invlogit(lower), 3),
         upper = round(invlogit(upper), 3),
         gen_prev = round(invlogit(logit_gen_prev), 3)) %>%
  filter(is.na(iso3),
         (region == "ESA" & gen_prev %in% c(0.011, 0.151)) | (region == "WCA" & gen_prev %in% c(0.011, 0.066))) %>%
  select(region, kp, gen_prev, lower, median, upper) %>%
  arrange(region, gen_prev) %>%
  filter(kp %in% c("MSM", "TG")) %>%
  mutate(ind = "prev") %>%
  rename(gen = gen_prev) %>%
  bind_rows(
    art_res %>%
      mutate(median = round(invlogit(median), 3),
             lower = round(invlogit(lower), 3),
             upper = round(invlogit(upper), 3),
             gen_art = round(invlogit(logit_gen_art), 3)) %>%
      filter(is.na(iso3),
             gen_art %in% c(0.4, 0.75, 0.8)) %>%
      select(kp, gen_art, lower, median, upper) %>%
      arrange(gen_art) %>%
      filter(kp %in% c("MSM", "TG")) %>%
      mutate(ind = "art") %>%
      rename(gen = gen_art)
  )

# write_csv(msm_15to29, "~/Downloads/msm_15to29.csv")

msm_15to29 %>%
  mutate(source = "a15to29") %>%
  bind_rows(read_csv("~/Downloads/msm_15to49.csv") %>%
              mutate(source = "a15to49"))

