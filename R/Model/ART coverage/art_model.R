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

invlogit <- function(x) {exp(x)/(1+exp(x))}
logit <- function(x) {log(x/(1-x))}

region <- read.csv("~/Documents/GitHub/fertility_orderly/global/region.csv") %>%
  mutate(iso3 = toupper(iso3))

convert_logis_labels <- function(x) {
  paste0(round(plogis(x)*100), "%")
}

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

############## ART Coverage ############

art_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_final.csv")

imp_denomin <- art_dat %>%
  filter(!is.na(denominator),
         denominator != 0) %>%
  group_by(kp) %>%
  summarise(quant = quantile(denominator, 0.25))


art_df <- art_dat %>%
  bind_rows() %>%
  left_join(region %>% select(region, iso3)) %>%
  mutate(denominator = case_when(
    (is.na(denominator) | denominator == 0) & kp == "FSW" ~ filter(imp_denomin, kp == "FSW")$quant,
    (is.na(denominator) | denominator == 0) & kp == "MSM" ~ filter(imp_denomin, kp == "MSM")$quant,
    (is.na(denominator) | denominator == 0) & kp == "PWID" ~ filter(imp_denomin, kp == "PWID")$quant,
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

df_logit <- data.frame(logit_gen_art = logit(seq(0.01, 0.99, 0.01)))


# art_res <- lapply(c("FSW", "MSM", "PWID"), function(kp_id) {
#   
#   art_df <- art_df %>%
#     filter(kp == kp_id)
#   
#   ## Prediction data frames
#   df_natural <- data.frame(provincial_value = seq(0.25, 0.99,0.01))
#   
#   
#   ## Quasibinomial model formula using positives and negatives [same result using the ART coverage and the weight argument in glm()]
#   formula <- cbind(positive, negative) ~ logit_gen_art
#   mod <- glm(formula, family = "quasibinomial", data = art_df)
#   
#   ## Predict on both the natural and link [logit] scale
#   qb_dat <-  df_natural %>%
#     cbind(df_logit) %>%
#     mutate(logit_fit = predict.glm(mod, newdata = df_logit, type = "link", se.fit = TRUE)$fit,
#            se = predict.glm(mod, newdata = df_logit, type = "link", se.fit = TRUE)$se.fit,
#            logit_lower = logit_fit - 1.96*se,
#            logit_upper = logit_fit + 1.96*se,
#            lower = invlogit(logit_lower),
#            upper = invlogit(logit_upper),
#            fit = invlogit(logit_fit),
#            source = "quasibinomial",
#            kp = kp_id)
#   
#   # df_logit_r <- crossing(logit_gen_art = logit(seq(0.25, 0.99, 0.01)),
#   #                      region = c("WCA", "ESA"))
#   # df_natural_r <- crossing(provincial_value = seq(0.25, 0.99, 0.01),
#   #                        region = c("WCA", "ESA"))
#   # 
#   # formula <- cbind(positive, negative) ~ logit_gen_art + (1|idx)
#   # mod <- glmer(formula, family = "binomial", data = art_df)
#   # 
#   # br_dat <-  df_natural %>%
#   #   mutate(fit = predict(mod, newdata = df_logit, type = "response", re.form=NA),
#   #          type = "natural") %>%
#   #   # bind_rows(df_logit %>% cbind(
#   #   #   fit = predict.glm(mod, newdata = df_logit, type = "link"),
#   #   #   type = "logit"
#   #   # )) %>%
#   #   mutate(source = "binomial + iid",
#   #          kp = kp_id)
#   # 
#   # formula <- logit_kp_art ~ logit_gen_art
#   # mod <- MASS::rlm(formula, data = art_df, weights = denominator)
#   # 
#   # rob_dat <-  df_natural %>%
#   #   mutate(fit = invlogit(predict.glm(mod, newdata = df_logit, type = "response")),
#   #          type = "natural") %>%
#   #   # bind_rows(df_logit %>% cbind(
#   #   #   fit = predict.glm(mod, newdata = df_logit, type = "link"),
#   #   #   type = "logit"
#   #   # )) %>%
#   #   mutate(source = "robust",
#   #          kp = kp_id)
#   # 
#   # 
#   # ## Logit linear model
#   # formula <- logit_kp_art ~ logit_gen_art
#   # mod <- glm(formula, family = "gaussian", data = art_df)
#   # 
#   # logit_dat <- df_natural %>%
#   #   mutate(fit = invlogit(predict.glm(mod, newdata = df_logit, type = "response")),
#   #          type = "natural") %>%
#   #   # bind_rows(df_logit %>% cbind(
#   #   #   fit = predict.glm(mod, newdata = df_logit, type = "link"),
#   #   #   type = "logit"
#   #   # )) %>%
#   #   mutate(source = "logit linear",
#   #          kp = kp_id)
#   # 
#   # qb_dat %>%
#   #   bind_rows(logit_dat) %>%
#   #   bind_rows(br_dat) %>%
#   #   bind_rows(rob_dat)
#   
#   
#   
#   
# })

art_res <- lapply(c("FSW", "MSM", "PWID"), function(kp_id) {
  
  art_df <- art_df %>%
    filter(kp == kp_id) %>%
    group_by(ref) %>%
    mutate(id.ref = cur_group_id(),
           id.ref = ifelse(is.na(ref), NA, id.ref)) %>%
    ungroup()
  
  
  art_inla <- crossing(df_logit
                        # region = c("WCA", "ESA")
  )%>%
    mutate(denominator = 1) %>%
    bind_rows(art_df %>%
                ungroup) %>%
    select(logit_gen_art, positive, negative, region, denominator, method, id.ref)
  
  # art_formula <- positive ~ logit_gen_art + f(idx, model = "iid")
  # # *region + f(idx, model = "iid")
  # 
  # art_fit <- INLA::inla(art_formula,
  #                        data = art_inla,
  #                        family = "binomial", 
  #                        Ntrials = denominator,
  #                        control.compute = list(config = TRUE),
  #                        control.predictor=list(compute=TRUE),
  #                        verbose = TRUE)
  # 
  # fitted_val <- get_mod_results_test(art_fit, art_inla, "positive")
  # 
  # res <- fitted_val %>%
  #   rename(logit_lower = lower,
  #          logit_fit = median,
  #          logit_upper = upper
  #   ) %>%
  #   mutate(lower = invlogit(logit_lower),
  #          upper = invlogit(logit_upper),
  #          fit = invlogit(logit_fit),
  #          provincial_value = invlogit(logit_gen_art),
  #          kp = kp_id,
  #          model = "binomial + iid")
  # 
  # ############
  # 
  # art_formula <- positive ~ logit_gen_art + f(idx, model = "iid")
  # 
  # art_fit <- INLA::inla(art_formula,
  #                        data = art_inla,
  #                        family = "betabinomial", 
  #                        Ntrials = denominator,
  #                        # offset = log(denominator),
  #                        control.compute = list(config = TRUE),
  #                        control.family = list(link = "logit"),
  #                        control.predictor=list(compute=TRUE),
  #                        verbose = TRUE)
  # 
  # fitted_val <- get_mod_results_test(art_fit, art_inla, "positive")
  # 
  # res <- res %>%
  #   bind_rows(
  #     fitted_val %>%
  #       rename(logit_fit = median,
  #              logit_lower = lower,
  #              logit_upper = upper) %>%
  #       mutate(
  #         lower = invlogit(logit_lower),
  #         upper = invlogit(logit_upper),
  #         fit = invlogit(logit_fit),
  #         provincial_value = invlogit(logit_gen_art),
  #         kp = kp_id,
  #         model = "betabinomial + iid")
  #   )
  # 
  ############
  
  art_formula <- positive ~ logit_gen_art + method + f(id.ref, model = "iid")
  
  art_fit <- INLA::inla(art_formula,
                         data = art_inla,
                         family = "betabinomial", 
                         Ntrials = art_inla$denominator,
                         # offset = log(denominator),
                         control.compute = list(config = TRUE),
                         control.family = list(link = "logit"),
                         control.predictor=list(compute=TRUE),
                         verbose = TRUE)
  
  fitted_val <- get_mod_results_test(art_fit, art_inla, "positive")
  
  res <- 
    # res %>%
    # bind_rows(
    fitted_val %>%
      rename(logit_fit = median,
             logit_lower = lower,
             logit_upper = upper) %>%
      mutate(
        lower = invlogit(logit_lower),
        upper = invlogit(logit_upper),
        fit = invlogit(logit_fit),
        provincial_value = invlogit(logit_gen_art),
        kp = kp_id,
        model = "betabinomial")
    
  
  #########
  
  # prev_formula <- positive ~ logit_gen_prev + f(idx, model = "iid")
  # 
  # prev_fit <- INLA::inla(prev_formula,
  #                        data = prev_inla,
  #                        family = "nbinomial", 
  #                        offset = log(denominator),
  #                        control.compute = list(config = TRUE),
  #                        control.predictor=list(compute=TRUE),
  #                        verbose = TRUE)
  # 
  # fitted_val <- get_mod_results_test(prev_fit, prev_inla, "positive")
  # 
  # res <- res %>%
  #   bind_rows(
  #     fitted_val %>%
  #       rename(log_fit = median,
  #              log_lower = lower,
  #              log_upper = upper) %>%
  #       mutate(
  #         lower = exp(log_lower),
  #         upper = exp(log_upper),
  #         fit = exp(log_fit),
  #         provincial_value = invlogit(logit_gen_prev),
  #         kp = kp_id,
  #         model = "nbinom + iid")
  # )
})

write.csv(art_res %>%
  bind_rows(), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_estimates.csv")

p1 <- art_res %>%
  bind_rows() %>%
  filter(model == "betabinomial") %>%
  ggplot(aes(x=logit_gen_art, y=logit_fit)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin = logit_lower, ymax = logit_upper), alpha=0.3) +
  # geom_line(size=1, aes(color = region)) +
  # geom_ribbon(aes(ymin = logit_lower, ymax = logit_upper, fill = region), alpha=0.3) +
  geom_point(data = art_df %>% filter(kp %in% c("MSM", "PWID", "FSW")), aes(y=logit_kp_art, color=region), alpha = 0.3) +
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  moz.utils::standard_theme() +
  # scale_x_continuous(labels = scales::label_percent(), limits = c(0,0.5)) +
  # scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  scale_y_continuous(labels = convert_logis_labels) +
  scale_x_continuous(labels = convert_logis_labels) +
  labs(y = "KP ART coverage", x = "Age/sex matched total population ART coverage")+
  theme(panel.border = element_rect(fill=NA, color="black")) +
  facet_wrap(~kp, ncol=1)
  # facet_grid(model~kp)

p2 <- art_res %>%
  bind_rows() %>%
  filter(model == "betabinomial") %>%
  ggplot(aes(x=provincial_value, y=fit)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.3) +
  # geom_line(size=1, aes(color=source)) +
  # geom_ribbon(aes(ymin = lower, ymax = upper, fill=source), alpha=0.3) +
  geom_point(data = art_df %>% filter(kp %in% c("MSM", "PWID", "FSW")), aes(y=value, color=region), alpha = 0.3) +
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  moz.utils::standard_theme() +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  labs(y = "KP ART coverage", x = "Age/sex matched total population ART coverage")+
  theme(panel.border = element_rect(fill=NA, color="black")) +
  facet_wrap(~kp, ncol=1)
  # facet_grid(model~kp)

png(file="~/Dropbox/Work Streams/2021/Key populations/Paper/Data consolidation paper/Figs/ART coverage/art_results.png", width=700, height=850)
ggpubr::ggarrange(p1, p2, nrow=1, common.legend = TRUE, legend = "bottom")
dev.off()

#####

art_res %>%
  bind_rows() %>%
  ggplot(aes(x=logit_gen_art, y=logit_fit)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin = logit_lower, ymax = logit_upper), alpha=0.3) +
  geom_point(data = art_df %>% filter(kp %in% c("MSM", "PWID", "FSW")), aes(y=logit_kp_art), alpha = 0.3) +
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  moz.utils::standard_theme() +
  # scale_x_continuous(labels = scales::label_percent(), limits = c(0,0.5)) +
  # scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  scale_y_continuous(labels = convert_logis_labels) +
  scale_x_continuous(labels = convert_logis_labels) +
  labs(y = "KP HIV artalence", x = "General population HIV artalence")+
  theme(panel.border = element_rect(fill=NA, color="black")) +
  facet_wrap(~kp, ncol=3)

art_res %>%
  bind_rows() %>%
  ggplot(aes(x=provincial_value, y=fit)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.3) +
  geom_point(data = art_df %>% filter(kp %in% c("MSM", "PWID", "FSW")), aes(y=value), alpha = 0.3) +
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  moz.utils::standard_theme() +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  labs(y = "KP HIV artalence", x = "General population HIV artalence")+
  theme(panel.border = element_rect(fill=NA, color="black")) +
  facet_wrap(~kp, ncol=3)
