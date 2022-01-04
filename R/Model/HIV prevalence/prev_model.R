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
  
  samples_ident <- ident %>%
    mutate(lower = qtls[1,],
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

prev_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_final.csv")

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
    (is.na(denominator) | denominator == 0) & kp == "FSW", filter(imp_denomin, kp == "FSW")$quant,
    (is.na(denominator) | denominator == 0) & kp == "MSM", filter(imp_denomin, kp == "MSM")$quant,
    (is.na(denominator) | denominator == 0) & kp == "PWID", filter(imp_denomin, kp == "PWID")$quant,
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

{
# prev_df <- prev_df %>%
#   filter(kp == "FSW")
# 
# df_natural <- data.frame(provincial_value = seq(0.01, 0.4,0.01))
# df_logit <- data.frame(logit_gen_prev = logit(seq(0.01, 0.4, 0.01)))
# 
# formula <- cbind(positive, negative) ~ logit_gen_prev + (1|idx)
# mod <- glmer(formula, family = "binomial", data = prev_df)
# 
# df_logit <- data.frame(logit_gen_prev = logit(seq(0.01, 0.4, 0.01)), idx =99999)
# 
# df_natural %>%
#   cbind(invlogit(merTools::predictInterval(mod, newdata = df_logit))) %>%
#   rename(lower = lwr,
#          upper = upr) %>%
#   
#   mutate(source = "binomial + iid",
#          kp = "FSW")

# 
# prev_res <- lapply(c("FSW", "MSM", "PWID"), function(kp_id) {
#   
#   prev_df <- prev_df %>%
#     filter(kp == kp_id)
#   
#   ## Prediction data frames
#   df_natural <- data.frame(provincial_value = c(seq(0.005, 0.01, 0.001), seq(0.01, 0.4,0.01)))
#   df_logit <- data.frame(logit_gen_prev = logit(c(seq(0.005, 0.01, 0.001), seq(0.01, 0.4, 0.01))))
#   
#   ## Quasibinomial model formula using positives and negatives [same result using the ART coverage and the weight argument in glm()]
#   formula <- cbind(positive, negative) ~ logit_gen_prev
#   mod <- glm(formula, family = "quasibinomial", data = prev_df)
#   
#   qb_dat <-  df_logit %>% cbind(
#     logit_fit = predict.glm(mod, newdata = df_logit, type = "link", se.fit=TRUE)$fit,
#     se = predict.glm(mod, newdata = df_logit, type = "link", se.fit=TRUE)$se.fit
#   ) %>%
#     mutate(
#       logit_lower = logit_fit - 1.96*se,
#       logit_upper = logit_fit + 1.96*se,
#       lower = plogis(logit_lower),
#       upper = plogis(logit_upper),
#       fit = plogis(logit_fit),
#       kp = kp_id,
#       source = "quasibinomial"
#     ) %>%
#     cbind(df_natural)
#   
#   # formula <- cbind(positive, negative) ~ logit_gen_prev + (1|idx)
#   # mod <- glmer(formula, family = "binomial", data = prev_df)
#   # 
#   # df_logit <- data.frame(logit_gen_prev = logit(seq(0.01, 0.4, 0.01)), idx =99999)
#   # 
#   # br_dat <- df_natural %>%
#   #   cbind(invlogit(merTools::predictInterval(mod, newdata = df_logit))) %>%
#   #   rename(lower = lwr,
#   #          upper = upr) %>%
#   # 
#   #   mutate(source = "binomial + iid",
#   #          kp = kp_id)
#   # 
#   # bind_rows(qb_dat, br_dat)
# })
# 
# names(prev_res) <- c("FSW", "MSM", "PWID")
# prev_res <- prev_res %>%
#   bind_rows(.id = "kp")
# 
# 
# 
# prev_res %>%
#   bind_rows() %>%
#   ggplot(aes(x=logit_gen_prev, y=logit_fit)) +
#   geom_line(size=1, aes(color=source)) +
#   geom_ribbon(aes(ymin = logit_lower, ymax=logit_upper, fill=source), alpha=0.3) +
#   geom_point(data = prev_df %>% filter(kp %in% c("MSM", "PWID", "FSW")), aes(y=logit_kp_prev), alpha = 0.3) +
#   geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
#   scale_y_continuous(labels = convert_logis_labels) +
#   scale_x_continuous(labels = convert_logis_labels) +
#   moz.utils::standard_theme() +
#   labs(y = "Logit KP HIV prevalence", x = "Logit general population HIV prevalence")+
#   theme(panel.border = element_rect(fill=NA, color="black"),
#         legend.position = "none") +
#   facet_wrap(~kp, ncol=3)
# 
# prev_res %>%
#   bind_rows() %>%
#   ggplot(aes(x=provincial_value, y=fit)) +
#   geom_line(size=1, aes(color=source)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper, fill=source), alpha=0.3) +
#   geom_point(data = prev_df %>% filter(kp %in% c("MSM", "PWID", "FSW")), aes(y=value), alpha = 0.3) +
#   geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
#   moz.utils::standard_theme() +
#   scale_x_continuous(labels = scales::label_percent(), limits = c(0,0.5)) +
#   scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
#   labs(y = "KP HIV prevalence", x = "General population HIV prevalence")+
#   theme(panel.border = element_rect(fill=NA, color="black")) +
#   facet_wrap(~kp, ncol=3)
# 
# p2
# ggpubr::ggarrange(p1, p2, ncol=2)

# prev_df %>%
#   filter(!is.na(iso3), kp %in%c("MSM", "PWID", "FSW")) %>%
#   ggplot(aes(x=logit_gen_prev, y=logit_kp_prev)) +
#     geom_point(alpha = 0.3) +
#     geom_line(data = prev_res, aes(x=logit_gen_prev, y=logit_fit), size=1) +
#     geom_ribbon(data = prev_res, aes(x=logit_gen_prev, ymin = logit_lower, ymax=logit_upper), alpha=0.3) +
#     geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
#     moz.utils::standard_theme() +
#     # scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
#     # scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
#     labs(y = "Logit KP HIV prevalence", x = element_blank())+
#     theme(panel.border = element_rect(fill=NA, color="black"),
#           legend.position = "none") +
#     facet_wrap(~kp, ncol=1)
#   # lims(x=c(0,1), y=c(0,1))

# p2 <- prev_df %>%
#   filter(!is.na(iso3), kp %in% c("FSW")) %>%
#   ggplot(aes(x=provincial_value, y=value, group=region)) +
#   geom_point(aes(color=region), alpha = 0.3) +
#   geom_line(data = prev_res %>% filter(kp == "FSW"), aes(x=gen_prev, y=invlogit(median), color=region), size=1) +
#   geom_ribbon(data = prev_res %>% filter(kp == "FSW"), aes(x=gen_prev, ymin = invlogit(lower), ymax=invlogit(upper), fill=region), alpha=0.3) + 
#   geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
#   scale_x_continuous(labels = scales::label_percent(), limits = c(0,1)) +
#   scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
#   moz.utils::standard_theme() +
#   scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
#   scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
#   labs(y = "KP HIV prevalence", x = element_blank())+
#   theme(panel.border = element_rect(fill=NA, color="black"),
#         legend.position = "none") +
#   facet_wrap(~kp, ncol=1)
# 
# p3 <- prev_df %>%
#   filter(!is.na(iso3), kp %in% c("MSM", "PWID")) %>%
#   ggplot(aes(x=logit_gen_prev, y=logit_kp_prev)) +
#   geom_point(alpha = 0.3) +
#   geom_line(data = prev_out %>% mutate(region = "SSA"), aes(color=region, x=logit_gen_prev, y=median), size=1) +
#   geom_ribbon(data = prev_out %>% mutate(region = "SSA"), aes(x=logit_gen_prev, ymin = lower, ymax=upper), alpha=0.3) + 
#   geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
#   moz.utils::standard_theme() +
#   scale_color_manual(values = "black") +
#   scale_y_continuous(labels = scales::t)
# # scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
# labs(y = "Logit KP HIV prevalence", x = "Logit general population HIV prevalence") +
#   theme(panel.border = element_rect(fill=NA, color="black"),
#         legend.position = "none") +
#   facet_wrap(~kp, ncol=1)
# # lims(x=c(0,1), y=c(0,1))
# 
# p4 <- prev_df %>%
#   filter(!is.na(iso3), kp %in% c("MSM", "PWID")) %>%
#   ggplot(aes(x=provincial_value, y=value)) +
#   geom_point(alpha = 0.3) +
#   geom_line(data = prev_out %>% mutate(region = "SSA"), aes(color=region, x=gen_prev, y=invlogit(median)), size=1) +
#   geom_ribbon(data = prev_out %>% mutate(region = "SSA"), aes(x=gen_prev, ymin = invlogit(lower), ymax=invlogit(upper)), alpha=0.3) + 
#   geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
#   scale_x_continuous(labels = scales::label_percent(), limits = c(0,1)) +
#   scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
#   moz.utils::standard_theme() +
#   scale_color_manual(values = "black") +
#   # scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
#   labs(y = "KP HIV prevalence", x = "General population HIV prevalence") +
#   theme(panel.border = element_rect(fill=NA, color="black"),
#         legend.position = "none") +
#   facet_wrap(~kp, ncol=1)
# 
# leg <- ggpubr::get_legend(data.frame(x=1, y=1, region = c("SSA", "WCA", "ESA")) %>%
#                             ggplot(aes(x=x, y=y,color=region)) +
#                             geom_line(size=1) +
#                             scale_color_manual(values = c(wesanderson::wes_palette("Zissou1")[c(1)], "black", wesanderson::wes_palette("Zissou1")[c(4)]))+
#                             moz.utils::standard_theme() +
#                             labs(color = "Region")
# )
# 
# prev_plot_a <- ggpubr::ggarrange(p1, p2, nrow=1)
# prev_plot_b <- ggpubr::ggarrange(p3, p4, nrow=1)
# ggpubr::ggarrange(prev_plot_a, prev_plot_b, ncol=1, heights = c(1.1,2), legend.grob = leg, legend = "bottom")
# 
# mod <- lm(log_kp_prev ~ log_gen_prev, data = prev_inla %>% filter(!is.na(iso3)))
# summary(mod)
# 
# mod.res = resid(mod) 
}
#########################

df_logit <- data.frame(logit_gen_prev = logit(c(seq(0.0005, 0.01, 0.001), seq(0.01, 0.5, 0.01)))) %>%
  bind_rows(
    read_csv("R/Model/HIV prevalence/national_genpop_prev.csv") %>%
      mutate(logit_gen_prev = logit(mean)) %>%
      select(-mean)
  )


prev_res <- lapply(c("FSW", "MSM", "PWID"), function(kp_id) {
  
  prev_df <- prev_df %>%
    filter(kp == kp_id) %>%
    group_by(ref) %>%
    mutate(id.ref = cur_group_id(),
           id.ref = ifelse(is.na(ref), NA, id.ref)) %>%
    ungroup()
  
  prev_inla <- crossing(df_logit %>% filter(is.na(kp) | kp == kp_id)
                        # region = c("WCA", "ESA")
                        )%>%
    mutate(denominator = 1) %>%
    bind_rows(prev_df %>%
                ungroup) %>%
    select(iso3, logit_gen_prev, method, positive, negative, region, denominator, id.ref)
  
  {
  # prev_formula <- positive ~ logit_gen_prev + f(idx, model = "iid")
  # # *region + f(idx, model = "iid")
  # 
  # prev_fit <- INLA::inla(prev_formula,
  #                        data = prev_inla,
  #                        family = "binomial", 
  #                        Ntrials = denominator,
  #                        control.compute = list(config = TRUE),
  #                        control.predictor=list(compute=TRUE),
  #                        verbose = TRUE)
  # 
  # fitted_val <- get_mod_results_test(prev_fit, prev_inla, "positive")
  # 
  # res <- fitted_val %>%
  #   rename(logit_lower = lower,
  #          logit_fit = median,
  #          logit_upper = upper
  #   ) %>%
  #   mutate(lower = invlogit(logit_lower),
  #          upper = invlogit(logit_upper),
  #          fit = invlogit(logit_fit),
  #          provincial_value = invlogit(logit_gen_prev),
  #          kp = kp_id,
  #          model = "binomial + iid")
  # 
  # ############
  # 
  # prev_formula <- positive ~ logit_gen_prev + f(idx, model = "iid")
  # 
  # prev_fit <- INLA::inla(prev_formula,
  #                        data = prev_inla,
  #                        family = "betabinomial", 
  #                        Ntrials = denominator,
  #                        # offset = log(denominator),
  #                        control.compute = list(config = TRUE),
  #                        control.predictor=list(compute=TRUE),
  #                        verbose = TRUE)
  # 
  # fitted_val <- get_mod_results_test(prev_fit, prev_inla, "positive")
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
  #         provincial_value = invlogit(logit_gen_prev),
  #         kp = kp_id,
  #              model = "betabinomial + iid")
  #   )
    }
  ############
  
  prev_formula <- positive ~ logit_gen_prev + method + f(id.ref, model = "iid")
  
  prev_fit <- INLA::inla(prev_formula,
                         data = prev_inla,
                         family = "betabinomial", 
                         Ntrials = prev_inla$denominator,
                         # offset = log(denominator),
                         control.compute = list(config = TRUE),
                         control.predictor=list(compute=TRUE),
                         verbose = TRUE)
  
  message(prev_fit$summary.fixed$mean)
  
  fitted_val <- get_mod_results_test(prev_fit, prev_inla, "positive")
  
  res <- 
    # res %>%
    bind_rows(
      fitted_val %>%
        rename(logit_fit = median,
               logit_lower = lower,
               logit_upper = upper) %>%
        mutate(
          lower = invlogit(logit_lower),
          upper = invlogit(logit_upper),
          fit = invlogit(logit_fit),
          provincial_value = invlogit(logit_gen_prev),
          kp = kp_id,
          model = "betabinomial")
    )
  
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

write_csv(prev_res %>%
  bind_rows() %>%
  filter(is.na(iso3)), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_estimates.csv")

write_csv(prev_res %>%
  bind_rows() %>%
  filter(!is.na(iso3)) %>%
  select(iso3, kp, fit), "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_national_matched_estimates.csv")

p1 <- prev_res %>%
  bind_rows() %>%
  filter(model == "betabinomial") %>%
  ggplot(aes(x=logit_gen_prev, y=logit_fit)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin = logit_lower, ymax = logit_upper), alpha=0.3) +
  geom_point(data = prev_df %>% filter(kp %in% c("MSM", "PWID", "FSW")), aes(y=logit_kp_prev, color=region), alpha = 0.3) +
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  moz.utils::standard_theme() +
  scale_y_continuous(labels = convert_logis_labels) +
  scale_x_continuous(labels = convert_logis_labels) +
  labs(y = "KP HIV prevalence", x = "Age/sex matched total population HIV prevalence")+
  theme(panel.border = element_rect(fill=NA, color="black")) +
  facet_wrap(~kp, ncol=1)

p2 <- prev_res %>%
  bind_rows() %>%
  filter(model == "betabinomial") %>%
  ggplot(aes(x=provincial_value, y=fit)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.3) +
  geom_point(data = prev_df %>% filter(kp %in% c("MSM", "PWID", "FSW")), aes(y=value, color=region), alpha = 0.3) +
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  moz.utils::standard_theme() +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0,0.5)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  labs(y = "KP HIV prevalence", x = "Age/sex matched total population HIV prevalence")+
  theme(panel.border = element_rect(fill=NA, color="black")) +
  facet_wrap(~kp, ncol=1)

png(file="~/Dropbox/Work Streams/2021/Key populations/Paper/Data consolidation paper/Figs/Prevalence/prev_results.png", width=700, height=850)
ggpubr::ggarrange(p1, p2, nrow=1, common.legend = TRUE, legend = "bottom")
dev.off()

prev_res %>%
  bind_rows() %>%
  filter(model == "betabinomial",
         provincial_value %in% c(0.01, 0.3)
  ) %>%
  select(provincial_value, fit, kp) %>%
  mutate(ratio = fit/provincial_value) %>%
  arrange(provincial_value)
