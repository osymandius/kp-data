library(INLA)
library(tidyverse)
library(countrycode)
library(sf)
library(lme4)
library(spud)
library(naomi)

setwd(rprojroot::find_rstudio_root_file())

region <- read.csv("~/Documents/GitHub/fertility_orderly/global/region.csv")

ssa_names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- sort(countrycode(ssa_names, "country.name", "iso3c"))

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

naomi_dat <- lapply(ssa_iso3[ssa_iso3 != "SSD"], function(iso3) {
  
  message(iso3)
  
  sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))
  folder <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = "Shared Documents/Data/Spectrum files/2022 naomi preliminary")
  
  if(iso3 == "MOZ") {
    folder <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = "Shared Documents/Data/Spectrum files/2021 naomi")
  }
  
  path <- filter(folder$list(),
                 str_detect(name, fixed(iso3, ignore_case=TRUE)),
                 str_detect(name, "zip"))$name
  
  # path <- grep(iso3, folder$list()$name, value=TRUE, ignore.case = TRUE)
  
  if(length(path) > 1)
    stop("More than one Naomi fit found")
  
  if(length(path) == 0)
    stop("No Naomi fit found")
  
  if(iso3 != "MOZ") {
    path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/Spectrum files/2022 naomi preliminary", path)
  } else {
    path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/Spectrum files/2021 naomi", path)
  }
  
  indicators <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = path)
  # indicators <- read_output_package(indicators)
  
  tmpd <- tempfile()
  on.exit(unlink(tmpd))
  utils::unzip(indicators, exdir = tmpd)
  out <- list()
  out$indicators <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "indicators.csv"))
  out$meta_age_group <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "meta_age_group.csv"))
  out$meta_period <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "meta_period.csv"))
  out$meta_area <- read_sf(list.files(tmpd, full.names = TRUE, pattern = "boundaries.geojson"))
  out$meta_indicator <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "meta_indicator.csv"))
  class(out) <- "naomi_output"
  
  indicators <- add_output_labels(out)
  
  time_point <- unique(indicators$calendar_quarter)[2]
  
  indicators <- filter(indicators, calendar_quarter == time_point)
  areas <- out$meta_area
  
  out <- list()
  out$indicators <- indicators
  out$areas <- areas
  out
  
})
names(naomi_dat) <- ssa_iso3[ssa_iso3 != "SSD"]

long_nd <- lapply(naomi_dat, "[[", "indicators") %>%
  bind_rows(.id = "iso3")

areas <- lapply(naomi_dat, "[[", "areas") %>%
  lapply(sf::st_make_valid)

lvl_map <- read.csv("global/iso_mapping_fit.csv")
admin1_lvl <- lvl_map %>% select(iso3, admin1_level)

levels <- admin1_lvl %>%
  filter(iso3 %in% names(naomi_dat)) %>%
  arrange(iso3) %>%
  pull(admin1_level)

# debugonce(dfertility::make_adjacency_matrix)
# adj <- dfertility::make_adjacency_matrix(areas, levels)
# spdep::nb2INLA("admin1_level_adj.adj", adj)

areas <- areas %>%
  do.call(rbind, .) %>%
  separate(area_id, 3, remove = FALSE, into = c("iso3", NA))

long_nd <- long_nd %>%
  left_join(admin1_lvl) %>%
  filter(area_level == admin1_level)

areas <- areas %>%
  left_join(admin1_lvl) %>%
  filter(area_level == admin1_level) %>%
  select(iso3, area_id, geometry) %>%
  mutate(id.area = row_number())

genpop_pred <- data.frame(kp = c("FSW", "MSM", "PWID", "TG")) %>%
  mutate(sex = c("female", "male", "both", "female")) %>%
  left_join(
    long_nd %>%
      filter(age_group == "Y015_049",
             indicator %in% c("prevalence", "art_coverage"))
  ) %>%
  mutate(logit_gen_var = logit(mean)) %>%
  left_join(region) %>%
  mutate(iso3 = factor(iso3, levels = ssa_iso3))

genpop_pred <- genpop_pred %>%
  select(region, iso3, kp, area_id, indicator, logit_gen_var)

df_logit_prev <- data.frame(logit_gen_prev = logit(seq(0.001, 0.07, 0.005)),
               region = "WCA") %>%
      bind_rows(
        data.frame(logit_gen_prev = logit(seq(0.006, 0.35, 0.005)),
                   region = "ESA"),
        genpop_pred %>%
          filter(indicator == "prevalence") %>%
          select(region:area_id, logit_gen_prev = logit_gen_var)
          # left_join(areas %>% select(area_id, id.area) %>% st_drop_geometry())
      ) %>%
  mutate(iso3 = factor(iso3, levels = ssa_iso3)) %>%
  arrange(iso3)

ref.iid.prec.prior <- list(prec= list(prior = "normal", param = c(1.6, 4)))
iso.iid.prec.prior <- list(prec= list(prior = "normal", param = c(3, 1)))
spatial.prec.prior <- list(prec= list(prior = "normal", param = c(-0.75, 6.25)))

prec.prior <- list(prec= list(prior = "normal", param = c(3,3)))

prev_mod <- lapply(c("FSW", "PWID"), function(kp_id) {
  
  out <- list()
  
  # prev_df <- prev_df %>% filter(kp == "FSW")
  # kp_id <- "PWID"
  
  int <- prev_df %>%
    ungroup() %>%
    mutate(method = ifelse(is.na(method), "selfreport", as.character(method))) %>%
    filter(kp == kp_id) %>%
    group_by(ref) %>%
    mutate(id.ref = cur_group_id()) %>%
    ungroup() %>%
    filter(!is.na(ref)) %>%
    mutate(obs_iid = row_number())
  
  prev_inla <- df_logit_prev %>% 
    filter(is.na(kp) | kp == kp_id) %>%
    mutate(denominator = 1) %>%
    bind_rows(int) %>%
    mutate(
      region = factor(region)) %>%
    left_join(geographies %>% st_drop_geometry()) %>%
    left_join(areas %>% st_drop_geometry()) %>%
    mutate(id.iso3 = ifelse(is.na(iso3), 39, id.iso3),
           id.area = ifelse(is.na(area_id), 578, id.area)) %>%
    ungroup() %>%
    select(iso3, area_id, logit_gen_prev, obs_iid, method, positive, negative, region, denominator, id.ref, id.iso3, id.area)
  
nat_level_obs <- prev_inla %>%
    filter(area_id == iso3) %>%
    ungroup() %>%
    group_by(id.ref) %>%
    mutate(id.ref.nat = cur_group_id(),
           id.ref = NA)

prev_inla <- prev_inla %>%
  filter(area_id != iso3 | is.na(iso3)) %>%
  bind_rows(nat_level_obs)
  
  # prev_formula <- positive ~ logit_gen_prev + method + f(id.ref, model = "iid") + f(id.iso3, model = "iid", hyper = iso.iid.prec.prior)
  prev_formula <- positive ~ 
    logit_gen_prev + 
    region + 
    region*logit_gen_prev + 
    method + 
    f(id.iso3, 
      model = "besag", 
      scale.model = TRUE, 
      graph = "national_level_adj.adj",
      hyper = prec.prior
      ) + 
    f(id.area, model = "besag", 
      scale.model = TRUE, 
      graph = "admin1_level_adj.adj",
      hyper = prec.prior
      ) + 
    f(id.ref, model = "iid") +
    f(id.ref.nat,
      model = "iid",
      hyper = prec.prior
      )
  # prev_formula <- positive ~ logit_gen_prev + region + region*logit_gen_prev + method + f(id.iso3, model = "iid") + f(id.ref, model = "iid")
  # prev_formula <- positive ~ logit_gen_prev + region + region*logit_gen_prev + method + f(id.iso3, model = "iid", hyper = iso.iid.prec.prior) + f(id.ref, model = "iid")
  
  # prev_formula <- positive ~ logit_gen_prev
  # prev_formula <- positive ~ logit_gen_prev + method + f(id.ref, model = "iid") + f(id.iso3, model = "iid", hyper = iso.iid.prec.prior)
  # prev_formula <- positive ~ logit_gen_prev + f(id.prev, model = "rw2") + method + f(id.ref, model = "iid") + f(id.iso3, model = "iid", hyper = iso.iid.prec.prior)
  
  prev_fit <- INLA::inla(prev_formula,
                         data = prev_inla,
                         family = "betabinomial", 
                         Ntrials = prev_inla$denominator,
                         # offset = log(denominator),
                         control.compute = list(config = TRUE,
                                                dic = TRUE),
                         control.predictor=list(compute=TRUE),
                         verbose = FALSE)
  
  # fit <- gamlss(cbind(positive, negative) ~ logit_gen_prev,
  #                data = na.omit(prev_inla),
  #                family = BB(mu.link = "logit"),
  #                control = gamlss.control(n.cyc=200))
  
  # data.frame(logit_kp_prev = predict(fit, newdata = data.frame(logit_gen_prev = -6:0)),
  #            logit_gen_prev = -6:0) %>%
  #     ggplot(aes(y=logit_kp_prev, x=logit_gen_prev)) +
  #       geom_line() +
  #       geom_point(data = prev_df %>% filter(kp %in% c("FSW"), !is.na(ref)), aes(x=logit_gen_prev, y=logit_kp_prev, color=region, size=denominator), alpha = 0.3) +
  #       geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  #       # geom_smooth(data = prev_df %>% filter(kp %in% c("FSW"), !is.na(ref)), aes(x=logit_gen_prev, y=logit_kp_prev, color=region), method = "lm", se = FALSE) +
  #       moz.utils::standard_theme() +
  #       # scale_y_continuous(labels = convert_logis_labels) +
  #       # scale_x_continuous(labels = convert_logis_labels) +
  #       labs(y = "KP HIV prevalence", x = "Age/sex matched total\npopulation HIV prevalence")+
  #       theme(panel.border = element_rect(fill=NA, color="black")) +
  #       facet_wrap(~kp, ncol=1)
  
  
  # df <- prev_inla %>% mutate(logit_kp_prev = logit(positive/(positive+negative))) %>% filter(!is.na(logit_kp_prev)) %>% filter(positive != 0)
  # mod <- lm(logit_kp_prev ~ logit_gen_prev, weights = denominator, data = df)
  # # data.frame(logit_kp_prev = predict(mod, data.frame(logit_gen_prev = -6:0)),
  # #            logit_gen_prev = -6:0) %>%
  #   ggplot() +
  #     # geom_line() +
  #     geom_point(data = prev_df %>% filter(kp %in% c("FSW"), !is.na(ref)), aes(x=logit_gen_prev, y=logit_kp_prev, color=region, size=denominator), alpha = 0.3) +
  #     geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  #     geom_smooth(data = prev_df %>% filter(kp %in% c("FSW"), !is.na(ref)), aes(x=logit_gen_prev, y=logit_kp_prev, color=region), method = "lm", se = FALSE) +
  #     moz.utils::standard_theme() +
  #     # scale_y_continuous(labels = convert_logis_labels) +
  #     # scale_x_continuous(labels = convert_logis_labels) +
  #     labs(y = "KP HIV prevalence", x = "Age/sex matched total\npopulation HIV prevalence")+
  #     theme(panel.border = element_rect(fill=NA, color="black")) +
  #     facet_wrap(~kp, ncol=1)
  # 
  # prev_fit$dic$dic
  
  #' FSW
  #' Base model: 4537
  #' Quadratic model: 4540
  #' Linear, no region: 4540
  #' RW2: 4542
  #'
  #' MSM
  #' Base model: 2575
  #' Quadratic model: 2572
  #' Linear, no region: 2572
  #' RW2: 2573
  #' 
  #' PWID:
  #' Base model: 775
  #' Quadratic model: 786
  #' Linear, no region: 786
  #' RW2: 745
  
  
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
  
  # # country_iid_precision <- matrix(inla.rmarginal(length(prev_samples), prev_fit$marginals.hyperpar$`Precision for id.iso3`), nrow=nrow(prev_samples))
  # 
  # 
  # 
  # country_iid_samples <- matrix(
  #   rnorm(length(prev_samples), 0, prev_fit$summary.hyperpar$mean[2]^-0.5),
  #   nrow=nrow(prev_samples))
  # 
  # country_iid_samples <- rnorm(120000, 0, prev_fit$summary.hyperpar$mean[2]^-0.5)
  # country_iid_samples <- rnorm(1000, 0, 0.1)
  # # 
  # prev_samples <- prev_samples + country_iid_samples
  # 
  # prev_samples <- prev_samples + rep(country_iid_samples, each = nrow(prev_samples))
  
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
  # 
  # prev %>% 
  #   filter(!is.na(area_id)) %>% 
  #   left_join(areas) %>% 
  #   ggplot(aes(fill = invlogit(median))) + 
  #     geom_sf(aes(geometry = geometry))
      
  
  # ggpubr::ggarrange(p1, p2)
  # 
  # prev %>%
  #   select(area_id, median) %>%
  #   left_join(areas) %>%
  #   ggplot() +
  #   geom_sf(aes(geometry = geometry, fill = invlogit(median))) +
  #   scale_fill_continuous(labels = scales::label_percent()) +
  #   labs(fill = "FSW Prevalence") +
  #   standard_theme() +
  #   coord_sf(datum=NA) +
  #   theme(legend.key.width = unit(2, "cm"))
  # 
  # prev %>%
  #   left_join(prev_df %>% distinct(area_id) %>% mutate(has_data = 1)) %>%
  #   mutate(has_data = ifelse(is.na(has_data), 0, 1)) %>%
  #   ggplot(aes(x=invlogit(logit_gen_prev), y=invlogit(median), color=factor(has_data))) +
  #     geom_pointrange(aes(ymin = invlogit(lower), ymax = invlogit(upper))) +
  #     standard_theme() +
  #     scale_y_continuous(labels = scales::label_percent()) +
  #     scale_x_continuous(labels = scales::label_percent())
  #     
  # # 
  # prev_fit$summary.random$id.iso3 %>%
  #   left_join(prev_inla %>% distinct(iso3, id.iso3), by=c("ID" = "id.iso3")) %>%
  #   left_join(genpop_pred %>% filter(kp == "FSW") %>% mutate(gen_prev = invlogit(logit_gen_prev))) %>%
  #   ggplot(aes(x=logit_gen_prev, y=mean)) +
  #     geom_point() +
  #     geom_smooth(method = "lm") +
  #     labs(y="iso3 IID random effect (unscaled)", x="Genpop HIV prevalence") +
  #     scale_x_continuous(labels = scales::label_percent()
  #     ) +
  #     standard_theme()
  
  # filter(prev, is.na(iso3)) %>% 
  #   select(logit_gen_prev, lower:upper, region) %>% 
  #   mutate(across(!region, invlogit)) %>%
  #   ggplot(aes(x=logit_gen_prev, y=median)) + 
  #     geom_line(aes(color=region)) + 
  #     geom_ribbon(aes(ymin=lower, ymax=upper, fill=region), alpha= 0.3)
  #     # geom_pointrange(data = filter(prev, !is.na(iso3)) %>% 
  #     #                   select(logit_gen_prev, lower:upper, iso3) %>% 
  #     #                   mutate(across(!iso3, invlogit)),
  #     #                 aes(ymin=lower, ymax=upper))
  # 
  prev_fixed <- data.frame(prev_fit$summary.fixed) %>%
    rownames_to_column() %>%
    mutate(kp = kp_id)
  
  out <- list()
  out$prev_samples <- prev_samples
  out$prev <- prev
  out$prev_fixed <- prev_fixed
  out
  
})

names(prev_mod) <- c("FSW", "PWID")

prev_mod_msm_tg <- lapply("MSM", function(kp_id) {
  
  out <- list()
  
  int <- prev_df %>%
    ungroup() %>%
    mutate(method = ifelse(is.na(method), "selfreport", as.character(method))) %>%
    filter(kp %in% c("MSM", "TG")) %>%
    group_by(ref) %>%
    mutate(id.ref = cur_group_id()) %>%
    ungroup() 
  
  prev_inla <- 
    bind_rows(
      df_logit_prev %>% filter(kp %in% c("TG", "MSM")),
      df_logit_prev %>% 
        filter(is.na(kp)) %>% 
        mutate(kp = "MSM"),
      df_logit_prev %>% 
        filter(is.na(kp)) %>% 
        mutate(kp = "TG")
    ) %>%
    arrange(iso3, kp) %>%
    mutate(denominator = 1) %>%
    bind_rows(int) %>%
    mutate(
      region = factor(region)) %>%
    left_join(geographies %>% st_drop_geometry()) %>%
    left_join(areas %>% st_drop_geometry()) %>%
    mutate(id.iso3 = ifelse(is.na(iso3), 39, id.iso3),
           id.area = ifelse(is.na(area_id), 578, id.area)) %>%
    ungroup() %>%
    select(iso3, area_id, kp, logit_gen_prev, method, positive, negative, region, denominator, id.ref, id.iso3, id.area)
  
  nat_level_obs <- prev_inla %>%
    filter(area_id == iso3) %>%
    ungroup() %>%
    group_by(id.ref) %>%
    mutate(id.ref.nat = cur_group_id(),
           id.ref = NA)
  
  prev_inla <- prev_inla %>%
    filter(area_id != iso3 | is.na(iso3)) %>%
    bind_rows(nat_level_obs)
  
  # prev_formula <- positive ~ logit_gen_prev + method + f(id.ref, model = "iid") + f(id.iso3, model = "iid", hyper = iso.iid.prec.prior)
  prev_formula <- positive ~ 
    logit_gen_prev + 
    region + 
    region*logit_gen_prev + 
    method + 
    f(id.iso3, 
      model = "besag", 
      scale.model = TRUE, 
      graph = "national_level_adj.adj",
      hyper = prec.prior
    ) + 
    f(id.area, model = "besag", 
      scale.model = TRUE, 
      graph = "admin1_level_adj.adj",
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
  
  
  prev_fixed <- data.frame(prev_fit$summary.fixed) %>%
    rownames_to_column() %>%
    mutate(kp = kp_id)
  
  out <- list()
  out$prev_samples <- prev_samples
  out$prev <- prev
  out$prev_fixed <- prev_fixed
  out
  
})

names(prev_mod_msm_tg) <- "MSM-TG"

msm_idx <- which(prev_mod_msm_tg$`MSM-TG`$prev$kp == "MSM")
tg_idx <- which(prev_mod_msm_tg$`MSM-TG`$prev$kp == "TG")

prev_mod$MSM$prev_samples <- prev_mod_msm_tg$`MSM-TG`$prev_samples[msm_idx,]
prev_mod$MSM$prev <- prev_mod_msm_tg$`MSM-TG`$prev %>% filter(kp == "MSM")

prev_mod$TG$prev_samples <- prev_mod_msm_tg$`MSM-TG`$prev_samples[tg_idx,]
prev_mod$TG$prev <- prev_mod_msm_tg$`MSM-TG`$prev %>% filter(kp == "TG")

prev_res <- lapply(prev_mod, "[[", "prev") %>%
  bind_rows(.id = "kp")

prev_fixed <- lapply(prev_mod, "[[", "prev_fixed") %>%
  bind_rows(.id = "kp")


#### ART

art_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_final.csv", show_col_types = FALSE)

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
         provincial_value < 1, ## FIX THIS
         !is.na(provincial_value)) %>%
  mutate(value = ifelse(value == 1, 0.99, value),
         value = ifelse(value ==0, 0.01, value),
         provincial_value = ifelse(provincial_value == 1, 0.99, provincial_value),
         provincial_value = ifelse(provincial_value ==0, 0.01, provincial_value),
         logit_kp_art = logit(value),
         logit_gen_art = logit(provincial_value),
         logit_gen_art2 = logit_gen_art,
         positive = round(value * denominator),
         negative = round(denominator - positive),
         method = factor(method, levels = c("lab", "selfreport"))
  ) %>%
  group_by(year, kp, iso3) %>%
  mutate(idx = cur_group_id())

df_logit_art <- data.frame(logit_gen_art = logit(seq(0.01, 0.99, 0.01)),
                            region = "WCA") %>%
  bind_rows(
    data.frame(logit_gen_art = logit(seq(0.006, 0.35, 0.005)),
               region = "ESA"),
    genpop_pred %>%
      filter(indicator == "art_coverage") %>%
      select(region:area_id, logit_gen_art = logit_gen_var)
    # left_join(areas %>% select(area_id, id.area) %>% st_drop_geometry())
  ) %>%
  mutate(iso3 = factor(iso3, levels = ssa_iso3)) %>%
  arrange(iso3)

###

art_mod <- lapply(c("FSW", "MSM", "PWID"), function(kp_id) {
  
  # kp_id <- "PWID"
  
  int <- art_df %>%
    ungroup() %>%
    mutate(method = ifelse(is.na(method), "selfreport", as.character(method))) %>%
    group_by(ref) %>%
    filter(kp == kp_id) %>%
    # filter(kp == kp_id) %>%
    mutate(id.ref = cur_group_id(),
           id.ref = ifelse(is.na(ref), NA, id.ref),
           # id.iso3 = cur_group_id(),
           logit_gen_art2 = logit_gen_art) %>%
    ungroup()
  
  art_inla <- df_logit_art %>% 
    filter(is.na(kp) | kp == kp_id) %>%
    mutate(denominator = 1) %>%
    bind_rows(int) %>%
    mutate(
      region = factor(region)) %>%
    left_join(geographies %>% st_drop_geometry()) %>%
    left_join(areas %>% st_drop_geometry()) %>%
    mutate(id.iso3 = ifelse(is.na(iso3), 39, id.iso3),
           id.area = ifelse(is.na(area_id), 578, id.area)) %>%
    ungroup() %>%
    select(iso3, area_id, logit_gen_art,  method, positive, negative, region, denominator, id.ref, id.iso3, id.area)
  
  nat_level_obs <- art_inla %>%
    filter(area_id == iso3) %>%
    ungroup() %>%
    group_by(id.ref) %>%
    mutate(id.ref.nat = cur_group_id(),
           id.ref = NA)
  
  art_inla <- art_inla %>%
    filter(area_id != iso3 | is.na(iso3)) %>%
    bind_rows(nat_level_obs)
  
  art_formula <- positive ~ 
    logit_gen_art + 
    f(id.iso3, model = "besag", scale.model = TRUE, graph = "national_level_adj.adj", hyper = prec.prior) + 
    f(id.area, model = "besag", scale.model = TRUE, graph = "admin1_level_adj.adj", hyper = prec.prior) + 
    f(id.ref, model = "iid", hyper = prec.prior) +
    f(id.ref.nat, model = "iid", hyper = prec.prior)
  
  if(kp_id == "PWID") {
    
    art_formula <- positive ~ 
      f(logit_gen_art, mean.linear = 0.66, prec.linear = 25, model = "linear") + 
      f(id.iso3, model = "besag", scale.model = TRUE, graph = "national_level_adj.adj", hyper = prec.prior) + 
      f(id.area, model = "besag", scale.model = TRUE, graph = "admin1_level_adj.adj", hyper = prec.prior) + 
      f(id.ref, model = "iid", hyper = prec.prior) +
      f(id.ref.nat, model = "iid", hyper = prec.prior)
    
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
  
  out <- list()
  out$art <- art
  out$art_samples <- art_samples
  out
  
})

names(art_mod) <- c("FSW", "MSM", "PWID")

art_res <- lapply(art_mod, "[[", "art") %>%
  bind_rows(.id = "kp")

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
  # filter(!str_detect(method, "methods")) %>%
  mutate(
    # method = ifelse(method == "Service multiplier", "Object/event multiplier", method),
    # method = ifelse(method == "Object/event multiplier", "Multiplier", method),
    # method = ifelse(str_detect(method, "CRC"), "CRC", method)
    fe_method = case_when(
      str_detect(method, "methods") ~ "other methods",
      method == "PLACE/Mapping" ~ method,
      TRUE ~ "empirical"
    )
  )

pse_dat <- pse_dat %>%
  mutate(population_proportion = pse/population,
  ) %>%
  filter(population_proportion != 0, !is.na(population_proportion), population_proportion < 1) %>%
  left_join(region %>% dplyr::select(region, iso3)) %>%
  mutate(
    logit_proportion = logit(population_proportion),
    fe_method = factor(fe_method, levels=c("empirical", unique(pse_dat$fe_method)[unique(pse_dat$fe_method) != "empirical" & !is.na(unique(pse_dat$fe_method))])),
  ) %>%
  ungroup %>%
  dplyr::select(iso3, year, kp, fe_method, method, logit_proportion, population_proportion, ref) %>%
  filter(iso3 != "LBR",
         !(iso3 == "BFA" & kp == "PWID"))
  
method.iid.prec.prior <- list(prec= list(prior = "normal", param = c(4, 1)))

pse_mod <- lapply(c("FSW", "MSM", "PWID"), function(kp_id) {
    
    pse_inla <- crossing(iso3 = ssa_iso3) %>%
      bind_rows(pse_dat %>%
                  filter(kp == kp_id) %>%
                  group_by(ref) %>%
                  mutate(id.ref = cur_group_id(),
                         id.ref = ifelse(is.na(ref), NA, id.ref)) %>%
                  ungroup %>%
                  arrange(fe_method) %>%
                  mutate(id.method = as.numeric(fct_inorder(method)))) %>%
      left_join(geographies %>% st_drop_geometry()) %>%
      dplyr::select(iso3, logit_proportion, fe_method, id.method, method, id.iso3, id.ref)
    
    pse_formula <- logit_proportion ~ 
      f(id.iso3, model = "besag", scale.model = TRUE, graph = "national_level_adj.adj", hyper=spatial.prec.prior) +
      fe_method +
      f(id.method, model = "iid", hyper = method.iid.prec.prior) +
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
    
    pse_fixed <- extract_df %>%
      mutate(lower = qtls[,1],
             median = qtls[,2],
             upper = qtls[,3])
    
    # pse_fixed <- data.frame(pse_fit$summary.fixed) %>%
    #   rownames_to_column() %>%
    #   dplyr::select(rowname, starts_with("X")) %>%
    #   type.convert(as.is = FALSE) %>%
    #   mutate(kp = kp_id)
    
    out <- list()
    out$pse <- pse
    out$pse_samples <- pse_samples
    out$pse_fixed <- pse_fixed
    out
    
})

names(pse_mod) <- c("FSW", "MSM", "PWID")

pse_res <- lapply(pse_mod, "[[", "pse") %>%
  bind_rows(.id = "kp")

pse_fixed <- lapply(pse_mod, "[[", "pse_fixed") %>%
  bind_rows(.id = "kp")


#######

pop <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

pop <- lapply(file.path("archive/aaa_scale_pop", pop, "interpolated_population.csv"), read_csv, show_col_types = FALSE) %>%
  bind_rows()

pop <- pop %>%
  filter(year %in% 2015:2020,
         str_length(area_id) == 3) %>%
  five_year_to_15to49("population") %>%
  group_by(area_id, sex) %>%
  group_split() %>%
  lapply(function(x) {
    extrap_pop <- Hmisc::approxExtrap(x$year, log(x$population), c(2021,2022))
    ar <- unique(x$area_id)
    se <- unique(x$sex)
    x %>%
      bind_rows(
        data.frame(year = c(2021, 2022),
                   population = exp(extrap_pop$y),
                   sex = se,
                   area_id = ar,
                   age_group = "Y015_049"
                   )
      )
  }) %>%
  bind_rows() %>%
  filter(year == 2022) %>%
  moz.utils::sex_aggregation("population")

pop <- pop %>%
  mutate(iso3 = area_id,
         kp = case_when(
    sex == "female" ~ "FSW",
    sex == "male" ~ "MSM",
    sex == "both" ~ "PWID")
  )

pop_l <- pop %>%
  left_join(region) %>%
  group_by(kp) %>%
  group_split() %>%
  setNames(c("FSW", "MSM", "PWID"))


wpp_urban <- read.csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/wpp_urban_proportions.csv") %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c"),
         urban_prop = urban_prop/100) %>%
  filter(!is.na(iso3),
         iso3 %in% ssa_iso3) %>%
  mutate(iso3 = factor(iso3, levels = ssa_iso3)) %>%
  arrange(iso3)


kplhiv_art <- Map(function(prev, pse, art, pop) {
  # 
  # prev_s <- prev_mod$FSW$prev_samples[84:121,]
  # pse_s <- pse_mod$FSW$pse_samples
  # art_s <- art_mod$FSW$art_samples[84:121,]
  # pop <- pop_l$FSW
  
  # 1 to 577?
  prev_s <- prev$prev_samples[84:121,]
  pse_s <- pse$pse_samples
  art_s <- art$art_samples[84:121,]
  
  pop_curr <- pop %>%
    mutate(iso3 = factor(iso3, levels = ssa_iso3)) %>%
    arrange(iso3)
  
  urban_prop_s <- matrix(rep(rbeta(1000, 5, 3), 38), nrow = nrow(pse_s), byrow = TRUE)
  rural_pse_s <- invlogit(pse_s) * urban_prop_s
  
 # data.frame(ntl =  ((invlogit(apply(pse_s, 1, median)) * pop_curr$population * wpp_urban$urban_prop) + (pop_curr$population * apply(rural_pse_s, 1, median) * (1-wpp_urban$urban_prop)))/pop_curr$population,
 #            urban = invlogit(apply(pse_s, 1, median))) %>%
 #   mutate(flag = ifelse(urban<ntl, 1, 0))
 # 
 # 
 # 
 #  
  pse_count_samples <- (invlogit(pse_s) * pop_curr$population * wpp_urban$urban_prop) + (pop_curr$population * rural_pse_s * (1-wpp_urban$urban_prop))
  kplhiv_samples <- invlogit(prev_s) * pse_count_samples
  kpart_samples <- kplhiv_samples * invlogit(art_s)
  
  # kplhiv_qtls <- apply(kplhiv_samples, 1, quantile, c(0.025, 0.5, 0.975))
  
  df <- data.frame(iso3 = ssa_iso3) %>%
    left_join(region)
  
  pse_count <- df %>%
    mutate(indicator = "pse_count") %>%
    cbind(pse_count_samples)
  
  pse <- df %>%
    mutate(indicator = "pse") %>%
    cbind(invlogit(pse_s))
  
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
  
  region_res <- bind_rows(pse_count, plhiv, art) %>%
    group_by(indicator, region) %>%
    summarise(across(as.character(1:1000), sum)) %>%
    bind_rows(
      bind_rows(pse_count, plhiv, art) %>%
        group_by(indicator) %>%
        summarise(across(as.character(1:1000), sum)) %>%
        mutate(region = "SSA")
    )

  region_qtls <- apply(region_res[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))
  
  region_res <- region_res %>%
    select(indicator, region) %>%
    cbind(data.frame(t(region_qtls)))
  
  colnames(region_res) <- c("indicator", "region", "lower", "median", "upper")
  
  country_res <- bind_rows(pse_count, plhiv, art, pse, prev, art_cov)
  
  country_qtls <- apply(country_res[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))
  
  country_res <- country_res %>%
    select(iso3, indicator) %>%
    cbind(t(country_qtls))
  
  colnames(country_res) <- c("iso3", "indicator", "lower", "median", "upper")
  
  out <- list()
  out$region <- region_res
  out$country <- country_res
  out
  
  }, prev_mod[c("FSW", "MSM", "PWID")], pse_mod, art_mod, pop_l) %>%
  setNames(c("FSW", "MSM", "PWID"))

region_pop <- pop %>%
  left_join(region) %>%
  filter(sex == "both") %>%
  group_by(region) %>%
  summarise(population = sum(population)) %>%
  bind_rows(
    pop %>%
      mutate(region = "SSA") %>%
      filter(sex == "both") %>%
      group_by(region) %>%
      summarise(population = sum(population))
  )

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

region_unaids_num <- unaids_num %>%
  left_join(region) %>%
  group_by(region, indicator) %>%
  summarise(value = sum(value)) %>%
  bind_rows(
    unaids_num %>%
      group_by(indicator) %>%
      summarise(value = sum(value)) %>%
      mutate(region = "SSA")
  )

bind_rows(
  kplhiv_art %>%
    lapply("[[", "region") %>%
    bind_rows(.id = "kp") %>%
    filter(indicator == "pse_count") %>%
    left_join(region_pop %>% rename(denominator = population)),
  kplhiv_art %>%
    lapply("[[", "region") %>%
    bind_rows(.id = "kp") %>%
    filter(indicator == "kplhiv") %>%
    left_join(region_unaids_num %>% ungroup() %>% filter(indicator == "plhiv") %>% select(-indicator) %>% rename(denominator = value))
  ) %>%
  mutate(across(lower:upper, ~.x/denominator),
         num = median * denominator)

########################

prev_s <- rbind(prev_mod$FSW$prev_samples[84:121,], prev_mod$MSM$prev_samples[84:121,], prev_mod$PWID$prev_samples[84:121,])
pse_s <- rbind(pse_mod$FSW$pse_samples, pse_mod$MSM$pse_samples, pse_mod$PWID$pse_samples)
art_s <- rbind(art_mod$FSW$art_samples[84:121,], art_mod$MSM$art_samples[84:121,], art_mod$PWID$art_samples[84:121,])

data.frame(
  lower = round(invlogit(apply(prev_mod$PWID$prev_samples[esa_idx,], MARGIN = 1, FUN = quantile, probs = 0.025)), 2),
  val = round(invlogit(apply(prev_mod$PWID$prev_samples[esa_idx,], MARGIN = 1, FUN = median)), 2),
  upper = round(invlogit(apply(prev_mod$PWID$prev_samples[esa_idx,], MARGIN = 1, FUN = quantile, probs = 0.975)), 2),
  iso3 = prev_mod$PWID$prev[esa_idx,] %>% pull(iso3))

pop_curr <- pop %>%
  mutate(iso3 = factor(iso3, levels = ssa_iso3),
         kp = factor(kp, levels = c("FSW","MSM", "PWID"))) %>%
  arrange(kp, iso3)

urban_prop_s <- matrix(rbeta(length(pse_s), 5, 3), nrow = nrow(pse_s))
rural_pse_s <- invlogit(pse_s) * urban_prop_s

pse_count_samples <- (pop_curr$population * invlogit(pse_s) * wpp_urban$urban_prop) + (pop_curr$population * rural_pse_s * 1-wpp_urban$urban_prop)
kplhiv_samples <- invlogit(prev_s) * pse_count_samples
kpart_samples <- kplhiv_samples * invlogit(art_s)

# kplhiv_qtls <- apply(kplhiv_samples, 1, quantile, c(0.025, 0.5, 0.975))

df <- crossing(kp = c("FSW", "MSM", "PWID"),
               data.frame(iso3 = ssa_iso3) %>%
                 left_join(region)
) %>%
  mutate(iso3 = factor(iso3, levels = ssa_iso3),
         kp = factor(kp, levels = c("FSW","MSM", "PWID"))) %>%
  arrange(kp, iso3)

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

unaids_sex_num <- bind_rows(
  read.csv("~/Downloads/People living with HIV_People living with HIV - Adults (15-49)_Population All adults (15-49).csv") %>% mutate(sex = "both"),
  read.csv("~/Downloads/People living with HIV_People living with HIV - Adults (15-49)_Population Female adults (15-49).csv") %>% mutate(sex = "female"),
  read.csv("~/Downloads/People living with HIV_People living with HIV - Adults (15-49)_Population Male adults (15-49).csv") %>% mutate(sex = "male")
) %>%
  mutate(indicator = "plhiv")

unaids_sex_num <- unaids_sex_num %>%
  select(!contains(c("lower", "upper", "Footnote"))) %>%
  mutate(iso3 = countrycode::countrycode(Country, "country.name", "iso3c")) %>%
  filter(iso3 %in% ssa_iso3) %>%
  select(-Country) %>%
  pivot_longer(-c(iso3, indicator, sex)) %>%
  mutate(year = str_remove(name, "X"),
         value = str_remove_all(value, " ")) %>%
  group_by(iso3, indicator, sex) %>%
  fill(value, .direction = "down") %>%
  filter(year == "2020", !(iso3 == "ZAF" & indicator == "art")) %>%
  type_convert() %>%
  select(-name) %>%
  rename(tot_plhiv = value)

pop_l %>%
  lapply(as.data.frame) %>%
  bind_rows() %>%
  left_join(unaids_sex_num) %>%
  group_by(sex) %>%
  summarise(tot_prev = sum(tot_plhiv)/sum(population))

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
  cbind(data.frame(t(region_qtls)))

colnames(region_res) <- c("indicator", "region",   "lower", "median", "upper")

bind_rows(
  region_res %>%
    filter(indicator == "pse_count") %>%
    left_join(region_pop %>% rename(denominator = population)),
  region_res %>%
    filter(indicator == "kplhiv") %>%
    left_join(region_unaids_num %>% ungroup() %>% filter(indicator == "plhiv") %>% select(-indicator) %>% rename(denominator = value))
)  %>%
  filter(region == "SSA") %>%
  group_by(indicator, region) %>%
  mutate(across(lower:upper, ~sum(.x)/denominator))
  # mutate(across(lower:upper, ~100*(.x/denominator))) %>%
  # arrange(region)

kp_res <- bind_rows(pse_count, plhiv, art) %>%
  group_by(indicator, kp) %>%
  summarise(across(as.character(1:1000), sum))

kp_qtls <- apply(kp_res[as.character(1:1000)], 1, quantile, c(0.025, 0.5, 0.975))

kp_res <- kp_res %>%
  select(indicator, kp) %>%
  cbind(data.frame(t(kp_qtls)))

colnames(kp_res) <- c("indicator", "kp", "lower", "median", "upper")

bind_rows(
  kp_res %>%
    filter(indicator == "pse_count") %>%
    mutate(denominator = 497983646),
  kp_res %>%
    filter(indicator == "kplhiv") %>%
    mutate(denominator = 19393000)
)
  mutate(across(lower:upper, ~100*(.x/denominator))) %>%
  arrange(kp)

######


####

prev_res %>%
  mutate(across(lower:upper, invlogit)) %>%
  ggplot(aes(x=invlogit(logit_gen_prev), y=median)) +
    geom_pointrange(aes(ymax = upper, ymin = lower, color=iso3)) +
    facet_wrap(~kp)

art_res %>%
  filter(is.na(iso3)) %>%
  mutate(source = "Without method fixed effect") %>%
  bind_rows(foo %>% filter(is.na(iso3)) %>% mutate(source = "With method fixed effect")) %>%
  mutate(across(lower:upper, invlogit)) %>%
  ggplot(aes(x=invlogit(logit_gen_art), y=median)) +
  # geom_pointrange(aes(ymax = upper, ymin = lower, color=iso3)) +
    geom_line(aes(color=source), size=1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill=source), alpha = 0.2) +
    geom_point(data = art_df %>% filter(kp != "TG"), aes(y=value)) +
    geom_abline(aes(intercept=0, slope=1), linetype =3 ) +
    facet_wrap(~kp) +
    standard_theme() +
    scale_y_continuous(labels = scales::label_percent()) +
    scale_x_continuous(labels = scales::label_percent()) +
    scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
    scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
    labs(y="KP ART coverage", x = "Total population ART coverage", color=element_blank(), fill=element_blank())




remaining_num <- kplhiv_art %>%
  lapply("[[", "country") %>%
  bind_rows(.id = "kp") %>%
  filter(indicator == "kplhiv") %>%
  group_by(iso3, indicator) %>%
  summarise(median = sum(median)) %>%
  left_join(unaids_num %>% mutate(indicator = ifelse(indicator == "plhiv", "kplhiv", "kpart"))) %>%
  mutate(median = value - median,
         kp = "Remainder") %>%
  select(iso3, kp, indicator, median)

plot_order <- c("SEN", "GMB", "GNB", "GIN", "SLE", "LBR", "MLI", "BFA", "CIV", "GHA", "TGO", "BEN", "NER", "NGA", "CMR", "TCD", "CAF", "SSD", "ETH", "GAB", "COG", "GNQ", "COD", "UGA", "KEN", "RWA", "BDI", "TZA", "AGO", "ZMB", "MWI", "MOZ", "BWA", "ZWE", "NAM", "SWZ", "LSO", "ZAF")


kplhiv_proportion_plot <- kplhiv_art %>%
  lapply("[[", "country") %>%
  bind_rows(.id = "kp") %>%
  filter(indicator == "kplhiv") %>%
  bind_rows(remaining_num) %>%
  # filter(iso3 != "ZAF",
  #        indicator == "kplhiv") %>%
  ggplot(aes(x=fct_rev(fct_relevel(iso3, levels = plot_order)), y=median, fill=fct_rev(kp))) +
  geom_col(position = "fill") +
  standard_theme() +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_discrete(labels = ~countrycode::countrycode(.x, "iso3c", "country.name")) +
  theme(legend.position = "right") +
  scale_fill_manual(values = 
                      c(
                        wesanderson::wes_palette("Zissou1")[1],
                        wesanderson::wes_palette("Moonrise2")[2],
                        wesanderson::wes_palette("Zissou1")[4],
                        wesanderson::wes_palette("Rushmore1")[3]
                        
                      )) +
  labs(x=element_blank(), y="Proportion of total PLHIV", fill=element_blank()) +
  coord_flip()

kplhiv_proportion_plot <- kplhiv_art %>%
  lapply("[[", "country") %>%
  bind_rows(.id = "kp") %>%
  filter(indicator == "kplhiv") %>%
  bind_rows(remaining_num) %>%
  group_by(iso3) %>%
  mutate(median = median/sum(median)) %>%
  filter(kp != "Remainder") %>%
  # filter(iso3 != "ZAF",
  #        indicator == "kplhiv") %>%
  ggplot(aes(x=fct_rev(fct_relevel(iso3, levels = plot_order)), y=median, fill=fct_rev(kp))) +
  geom_col(position = "stack") +
  standard_theme() +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_discrete(labels = ~countrycode::countrycode(.x, "iso3c", "country.name")) +
  scale_fill_manual(values = 
                      c(
                        wesanderson::wes_palette("Zissou1")[1],
                        # wesanderson::wes_palette("Moonrise2")[2],
                        wesanderson::wes_palette("Zissou1")[4],
                        wesanderson::wes_palette("Rushmore1")[3]
                        
                      )) +
  labs(x=element_blank(), y="Proportion of total PLHIV", fill=element_blank(), tag = "A") +
  coord_flip() +
  theme(legend.position = "right",
        plot.tag = element_text(size = rel(2.0), face = "bold")
        )

kplhiv_plot <- kplhiv_art %>%
  lapply("[[", "region") %>%
  bind_rows(.id = "kp") %>%
  filter(indicator == "kplhiv", region != "SSA")%>%
  ggplot(aes(x=region, y=median, group=kp, fill=kp)) +
  geom_col(position = position_dodge(.9)) +
  geom_linerange(aes(ymin=lower, ymax = upper), position=position_dodge(.9)) +
  scale_y_continuous(labels = scales::label_number(scale = 1E-3)) +
  # scale_y_log10(labels = scales::label_number()) +
  scale_x_discrete(labels = c("Eastern and\nSouthern Africa", "Western and\nCentral Africa")) +
  scale_fill_manual(values = c(wesanderson::wes_palette("Zissou1")[c(1)],
                               wesanderson::wes_palette("Zissou1")[c(4)],
                               wesanderson::wes_palette("Rushmore1")[3]))+
  standard_theme() +
  labs(x=element_blank(), y="KPLHIV (thousands)", fill=element_blank(), tag = "B") +
  theme(plot.tag = element_text(size = rel(2.0), face = "bold"))

png("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Figs/Fig 7 kplhiv_plot.png", width = 700, height=1000)
ggpubr::ggarrange(kplhiv_proportion_plot, kplhiv_plot, nrow =2, heights = c(1.5,1))
dev.off()

write_csv(prev_fixed, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_fixed.csv")

write_csv(prev_res %>%
  filter(is.na(iso3)) %>%
  mutate(across(lower:upper, invlogit)), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_estimates.csv")

write_csv(prev_res %>%
  filter(!is.na(iso3)) %>%
    mutate(across(lower:upper, invlogit)), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_national_matched_estimates.csv")

######

write_csv(pse_fixed, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_fixed.csv")

write_csv(pse_res %>%
            mutate(across(lower:upper, invlogit)), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_estimates.csv")

#####

write_csv(art_res %>%
            mutate(across(lower:upper, invlogit)) %>%
            filter(is.na(iso3)), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_estimates.csv")

write_csv(art_res %>%
            filter(!is.na(iso3)) %>%
            mutate(across(lower:upper, invlogit)), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_national_matched_estimates.csv")


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

art_res %>%
  filter(!is.na(iso3)) %>%
  mutate(median = 100*round(invlogit(median), 2),
         lower = 100*round(invlogit(lower), 2),
         upper = 100*round(invlogit(upper), 2),
         country = countrycode(iso3, "iso3c", "country.name"),
         res = paste0(median, " (", lower, ", ", upper, ")")
  ) %>%
  left_join(region) %>%
  arrange(kp, region, country) %>%
  select(region, country, kp, res) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/art_coverage_country_results.csv")

prev_res %>%
  filter(!is.na(iso3)) %>%
  mutate(median = 100*round(invlogit(median), 2),
         lower = 100*round(invlogit(lower), 2),
         upper = 100*round(invlogit(upper), 2),
         country = countrycode(iso3, "iso3c", "country.name"),
         res = paste0(median, " (", lower, ", ", upper, ")")
  ) %>%
  left_join(region) %>%
  arrange(kp, region, country) %>%
  select(region, country, kp, res) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/prev_country_results.csv")

pse_res %>%
  filter(!is.na(iso3)) %>%
  mutate(median = 100*round(invlogit(median), 3),
         lower = 100*round(invlogit(lower), 3),
         upper = 100*round(invlogit(upper), 3),
         country = countrycode(iso3, "iso3c", "country.name"),
         res = paste0(median, " (", lower, ", ", upper, ")")
  ) %>%
  left_join(region) %>%
  arrange(kp, region, country) %>%
  select(region, country, kp, res) %>%
  arrange(kp, region, country) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/pse_country_results.csv")

pse_res %>%
  mutate(median = 100*invlogit(median)) %>%
  left_join(region) %>%
  group_by(kp) %>%
  summarise(l = quantile(median, 0.25),
            m = quantile(median, 0.5),
            u = quantile(median, 0.75))

prev_res %>%
  mutate(median = round(invlogit(median), 3),
         lower = round(invlogit(lower), 3),
         upper = round(invlogit(upper), 3),
         gen_prev = round(invlogit(logit_gen_prev), 3)) %>%
  filter(is.na(iso3),
         (region == "ESA" & gen_prev %in% c(0.011, 0.151)) | (region == "WCA" & gen_prev %in% c(0.011, 0.066))) %>%
  select(region, kp, gen_prev, lower, median, upper) %>%
  arrange(region, gen_prev, kp) 

art_res %>%
  mutate(median = round(invlogit(median), 3),
         lower = round(invlogit(lower), 3),
         upper = round(invlogit(upper), 3),
         gen_art = round(invlogit(logit_gen_art), 3)) %>%
  filter(is.na(iso3),
         gen_art %in% c(0.4, 0.75, 0.8)) %>%
  select(kp, gen_art, lower, median, upper) %>%
  arrange(gen_art) %>%
  mutate(across(lower:upper, ~100*(.x-gen_art)))

#####
pse_final %>% 
  filter(kp == "MSM") %>% 
  mutate(source = "Current") %>%
  bind_rows(read_csv("~/Downloads/pse_final_sourced_pre_refgp.csv") %>%
              mutate(source = "Pre RefGp") %>%
              filter(kp == "MSM")) %>%
  filter(population_proportion < 0.15) %>%
  ggplot(aes(x=iso3, y=population_proportion*100)) +
    geom_boxplot(aes(color=source))

pse_estimates %>% 
  filter(kp == "MSM") %>% 
  mutate(source = "Current") %>%
  bind_rows(read_csv("~/Downloads/pse_estimates_pre_refgp.csv") %>%
              mutate(source = "Pre RefGp") %>%
              filter(kp == "MSM") %>%
              left_join(region))  %>%
  ggplot(aes(x=iso3, y=median)) +
  geom_point(aes(color=source)) +
  standard_theme() +
  scale_y_continuous(labels = scales::label_percent()
  ) +
  facet_wrap(~region, scales = "free_x")

pse_estimates %>% 
  filter(kp == "MSM") %>% 
  mutate(source = "current") %>%
  bind_rows(read_csv("~/Downloads/pse_estimates_pre_refgp.csv") %>%
              mutate(source = "prerefgp") %>%
              filter(kp == "MSM") %>%
              left_join(region)) %>%
  left_join(pop_l$MSM) %>%
  select(iso3, median, population, source) %>%
  mutate(msm_pop = median*population) %>%
  select(-c(median, population)) %>%
  pivot_wider(names_from = "source", values_from = "msm_pop") %>%
  mutate(diff = current - prerefgp) %>%
  left_join(region) %>%
  ggplot(aes(x=iso3, y=diff)) +
    geom_col() +
    facet_wrap(~region, scales = "free_x")

pse_estimates %>% 
  filter(kp == "MSM") %>% 
  mutate(source = "current") %>%
  bind_rows(read_csv("~/Downloads/pse_estimates_pre_refgp.csv") %>%
              mutate(source = "prerefgp") %>%
              filter(kp == "MSM") %>%
              left_join(region)) %>%
  left_join(pop_l$MSM) %>%
  left_join(prev_country_estimates %>%
              mutate(source = "current") %>%
              bind_rows(read_csv("~/Downloads/prev_national_matched_estimates_pre_refgp.csv") %>%
                          mutate(source = "prerefgp")) %>%
              filter(kp == "MSM") %>%
              select(iso3, source, prev = median)) %>%
  select(iso3, median, population, prev, source, region) %>%
  left_join(wpp_urban) %>%
  mutate(
    urban_pop = population * urban_prop,
    rural_pop = population * (1-urban_prop),
    rural_pse = median * 0.6,
    msm_urban = median * urban_pop,
    msm_rural = rural_pse * rural_pop,
    msm_pop = msm_urban + msm_rural,
    msm_pos = msm_pop*prev) %>%
  select(iso3, region, median, prev, source, msm_pop, msm_pos) %>%
  group_by(source, region) %>%
  summarise(msm_pop = sum(msm_pop),
            msm_pos = sum(msm_pos))

prev_s <- prev_mod$MSM$prev_samples[84:121,]
pse_s <- pse_mod$MSM$pse_samples

pop_curr <- pop_l$MSM %>%
  mutate(iso3 = factor(iso3, levels = ssa_iso3)) %>%
  arrange(iso3)

urban_prop_s <- matrix(rbeta(length(pse_s), 5, 3), nrow = nrow(pse_s))
rural_pse_s <- invlogit(pse_s) * urban_prop_s

pse_count_samples <- (pop_curr$population * invlogit(pse_s) * wpp_urban$urban_prop) + (pop_curr$population * rural_pse_s * 1-wpp_urban$urban_prop)
kplhiv_samples <- invlogit(prev_s) * pse_count_samples

pse_res %>% 
  mutate(median = invlogit(median)) %>%
  ungroup() %>%
  group_by(kp) %>%
  summarise(mid = median(median),
            lower = quantile(median, 0.25),
            upper = quantile(median, 0.75))