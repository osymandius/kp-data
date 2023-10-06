library(tidyverse)
library(DiagrammeR)
library(countrycode)
library(knitr)
library(sf)
# library(kableExtra)
library(moz.utils)

region <- region()
ssa_names <- ssa_names()
ssa_iso3 <- ssa_iso3()

group_proportion <- function(df, variables) {
  df %>%
    dplyr::group_by(across(all_of(variables))) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(prop = n/sum(n))
}

invlogit <- function(x) {exp(x)/(1+exp(x))}
logit <- function(x) {log(x/(1-x))}

pse_final <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final_sourced.csv", show_col_types = F)
prev_final <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_final_sourced.csv", show_col_types = F)
art_final <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_final.csv", show_col_types = F)
kplhiv_art <- readRDS("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/kplhiv_art.rds")

## Abstract

kplhiv_art$country %>%
  lapply(data.frame) %>%
  bind_rows() %>%
  filter(indicator == "pse_urban_prop") %>%
  left_join(region) %>%
  bind_rows(mutate(., region = "SSA")) %>%
  group_by(kp, region) %>%
  reframe(calculate_quantile(median)) %>%
  factor_region() %>%
  arrange(kp, region) %>%
  filter(region != "SSA")

# kplhiv_art$region %>%
#   lapply(data.frame) %>%
#   bind_rows() %>%
#   filter(indicator == "pse_urban_prop") %>%
#   group_by(kp, region) %>%
#   reframe(calculate_quantile(median)) %>%
#   factor_region() %>%
#   arrange(kp, region)

mf <- crossing(region %>%
                 select(-four_region) %>%
                 bind_rows(region %>% select(iso3) %>% mutate(region = "SSA")),
               kp = c("FSW", "MSM", "PWID", "TGW")
)

pse_n <- pse_final %>%
  left_join(region) %>%
  count(region, kp) %>%
  bind_rows(
    pse_final %>% count(kp) %>% mutate(region = "SSA")
  ) %>%
  rename(pse_n = n)

prev_n <- prev_final %>%
  left_join(region) %>%
  count(region, kp) %>%
  bind_rows(
    prev_final %>% count(kp) %>% mutate(region = "SSA")
  ) %>%
  rename(prev_n = n)

art_n <- art_final %>%
  left_join(region) %>%
  count(region, kp) %>%
  bind_rows(
    art_final %>% count(kp) %>% mutate(region = "SSA")
  ) %>%
  rename(art_n = n)


pse_tab <- mf %>%
  left_join(
    pse_final %>%
      distinct(iso3, kp) %>%
      mutate(has = 1)
  ) %>%
  mutate(has = ifelse(is.na(has), 0, has)) %>%
  group_by(region, kp) %>%
  summarise(has = sum(has)) %>%
  left_join(mf %>% distinct(region, iso3) %>% count(region) %>% rename(region_n = n)) %>%
  mutate(pse_prop = paste0(round(100*has/region_n), " (", has, "/", region_n, ")")) %>%
  left_join(pse_n) %>%
  select(-has)

prev_tab <- mf %>%
  left_join(
    prev_final %>%
      distinct(iso3, kp) %>%
      mutate(has = 1)
  ) %>%
  mutate(has = ifelse(is.na(has), 0, has)) %>%
  group_by(region, kp) %>%
  summarise(has = sum(has)) %>%
  left_join(mf %>% distinct(region, iso3) %>% count(region) %>% rename(region_n = n)) %>%
  mutate(prev_prop = paste0(round(100*has/region_n), " (", has, "/", region_n, ")")) %>%
  left_join(prev_n) %>%
  select(-has)

art_tab <- mf %>%
  left_join(
    art_final %>%
      distinct(iso3, kp) %>%
      mutate(has = 1)
  ) %>%
  mutate(has = ifelse(is.na(has), 0, has)) %>%
  group_by(region, kp) %>%
  summarise(has = sum(has)) %>%
  left_join(mf %>% distinct(region, iso3) %>% count(region) %>% rename(region_n = n)) %>%
  mutate(art_prop = paste0(round(100*has/region_n), " (", has, "/", region_n, ")")) %>%
  left_join(art_n) %>%
  select(-has)

summary_table <- pse_tab %>%
  left_join(prev_tab) %>%
  left_join(art_tab) %>%
  select(-region_n) %>%
  mutate(region = factor(region, levels = c("SSA", "ESA", "WCA"))) %>%
  arrange(region) %>%
  select(region, kp, pse_n, pse_prop, prev_n, prev_prop, art_n, art_prop)
  # kable(
  #   align='c',linesep='',booktabs=TRUE,escape=FALSE,
  #   col.names = linebreak(c("Region", "KP", rep(c("Data\npoints", "Countries with\ndata (\\%; n/N)"), 3)), align = "c"),
  #   caption = "Availability of population size, HIV prevalence, and ART coverage data by key population and region") %>%
  # add_header_above(.,
  #   c(" " = 1,
  #     " " = 1,
  #     "PSE" = 2,
  #     "HIV prevalence" = 2,
  #     "ART coverage" = 2),
  #   escape = FALSE
  #   ) %>%
  # kable_styling(position = "center")
  
openxlsx::write.xlsx(summary_table, "~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Figs/Table 1 summary table.xlsx", sheetName = "raw")

pse_input_count_text <- readRDS("R/Report/R objects for report/PSE/pse_input_count_text.rds") %>%
  mutate(across(c("FSW", "MSM", "PWID", "TGW"), ~str_remove(.x, "FSW|MSM|PWID|TGW")),
         across(c("FSW", "MSM", "PWID", "TGW"), ~str_trim(.x))
  )

length(unique(pse_final$study_idx))

pse_final %>%
  filter(year %in% 2010:2012) %>%
  distinct(study_idx, kp) %>%
  count(kp)

pse_final %>%
  filter(year %in% 2018:2020) %>%
  distinct(study_idx, kp) %>%
  count(kp)

msm_region_proportions <- pse_final %>%
  filter(kp == "MSM") %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c")) %>%
  left_join(region) %>%
  mutate(under_1 = ifelse(prop_estimate < 0.01, 1, 0)) %>%
  group_proportion(c("under_1"))

msm_region_proportions %>%
  group_by(region) %>%
  summarise(n = sum(n))

msm_u1 <- pse_final %>%
  filter(kp == "MSM") %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c")) %>%
  left_join(region) %>%
  name_region(T) %>%
  ggplot(aes(x=region, y=prop_estimate)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,.1), breaks = c(0, 0.01, 0.025, 0.05, 0.075, 0.1)) +
  geom_hline(aes(yintercept = 0.01), color="red", linetype = 2, size=1) +
  moz.utils::standard_theme() +
  labs(x=element_blank(), y="Population size estimate proportion")

png("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/S1 msm_u1.png", width = 400, height = 500)
msm_u1
dev.off()

length(unique(prev_final$study_idx))

prev_input_count_text <- readRDS("R/Report/R objects for report/Prevalence/prev_input_count_text.rds") %>%
  mutate(across(c("FSW", "MSM", "PWID", "TGW"), ~str_remove(.x, "FSW|MSM|PWID|TGW")),
         across(c("FSW", "MSM", "PWID", "TGW"), ~str_trim(.x))
  )

prev_denom <- prev_final %>% 
  mutate(has_denominator = ifelse(!is.na(denominator), 1, 0)) %>% 
  group_proportion("has_denominator")

prev_final %>%
  group_proportion("method")

length(unique(art_final$study_idx))

art_final %>%
  mutate(method = case_when(method == "Self-report" ~ "self-report",
                            method == "vls" ~ "VLS",
                            TRUE ~ method)) %>%
  group_proportion("method")

art_input_count_text <- readRDS("R/Report/R objects for report/ART coverage/art_input_count_text.rds") %>%
  mutate(across(c("FSW", "MSM", "PWID", "TGW"), ~str_remove(.x, "FSW|MSM|PWID|TGW")),
         across(c("FSW", "MSM", "PWID", "TGW"), ~str_trim(.x))
  )

art_final %>% 
  mutate(has_denominator = ifelse(!is.na(denominator), 1, 0)) %>% 
  group_proportion("has_denominator")

####################

kplhiv_art$country %>%
  lapply(data.frame) %>%
  bind_rows(.id = "kp") %>%
  filter(indicator == "pse_urban_prop") %>%
  left_join(region) %>%
  # bind_rows(mutate(., region = "SSA")) %>%
  group_by(kp) %>%
  reframe(calculate_quantile(median)) %>%
  # factor_region() %>%
  arrange(kp) %>%
  mutate(iqr = paste0(round(`0.25`, 2), "-", round(`0.75`, 2))) %>%
  select(-c(`0.25`, `0.75`))

res_1529 <- readRDS("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/kplhiv_art_1529.rds")

res_1529 %>%
  lapply("[[", "country") %>%
  bind_rows(.id = "kp") %>%
  filter(indicator == "pse_urban", kp == "MSM") %>%
  reframe(calculate_quantile(median))

pse_final %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c")) %>%
  left_join(region) %>%
  filter(kp == "PWID", region == "ESA") %>%
  distinct(iso3)


pse_method <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_fixed.csv")

filter(pse_method, method == "PLACE/mapping")

## !! JE edit: recode country names
# cname_recode <- c("Central African Republic" = "Cen. Afr. Repub.",
#                   "Congo - Kinshasa" = "Dem. Rep. Congo",
#                   "Congo - Brazzaville" = "Rep. Congo")

pse_estimates <- 
  # read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_estimates.csv") %>%
  kplhiv_art$country %>%
    lapply(data.frame) %>%
    bind_rows(.id = "kp") %>%
    filter(indicator == "pse_urban_prop") %>%

  mutate(area_name = countrycode::countrycode(iso3, "iso3c", "country.name", custom_match = moz.utils::cc_plot())) %>%
  left_join(region) %>%
  mutate(region = ifelse(iso3 == "BDI", "ESA", region),
         four_region = ifelse(iso3 == "BDI", "Eastern", four_region)
  )

pse_estimates <- pse_estimates %>%
  left_join(pse_final %>%
              mutate(iso3 = countrycode(country.name, "country.name", "iso3c")) %>%
              filter(iso3 != "LBR") %>%
              select(iso3, kp) %>%
              distinct() %>%
              mutate(has_data = 1)) %>%
  mutate(has_data = ifelse(is.na(has_data), 0, 1),
         has_data = factor(has_data, labels = c("No", "Yes")))

iso3_sort <- pse_estimates %>%
  distinct(area_name, iso3) %>%
  left_join(region) %>%
  arrange(four_region) %>%
  mutate(iso3_idx = as.numeric(fct_inorder(area_name)))

pse_estimates <- pse_estimates %>%
  select(-area_name) %>%
  left_join(iso3_sort) %>%
  mutate(xmin = iso3_idx - 0.48,
         xmax = iso3_idx + 0.48,
         background_col = ifelse(is.na(area_name), 1, 0))

pse_data_overlay <- pse_final %>%
  left_join(pse_estimates %>% select(iso3, iso3_idx) %>% distinct()) %>%
  left_join(region) %>%
  filter(iso3 != "LBR")

grey <- read_sf(grey_areas())


## !! JE edit: South Sudan boundaries very large vs. rest of boundaries. Reducing
##             to make plot size more manageable.

geographies <- read_sf(national_areas()) %>%
  st_make_valid()

## JE: trim Prince Edward Islands from map

bbox <- c(xmin = -17.5327797,
          ymin = -35, ## -46.9697266,
          xmax = 51.4113159179688, 
          ymax = 37.3404121398926)

geographies <- st_crop(geographies, bbox) %>%
  mutate(iso3 = area_id)

pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")





make_pse_map <- function(x) {
  
  kp_name <- c("FSW" = "Female sex workers",  "MSM" = "Men who have sex with men", "PWID" = "People who inject drugs" , "TGW" = "Transgender women")
  kp_tag <- c("FSW" = "A",  "MSM" = element_blank(), "PWID" = element_blank() , "TGW" = element_blank())
  fig3a_tag_color <- c("FSW" = "black", "MSM" = NA, "PWID" = NA, "TGW" = NA)
  kp_accuracy = c("FSW" = 0.1,  "MSM" = 0.1, "PWID" = 0.01 , "TGW" = 0.01)
  
  pse_estimates %>%
    filter(kp == x) %>%
    left_join(select(geographies, iso3)) %>%
    ggplot() +
    geom_sf(data = grey, aes(geometry = geometry), fill="darkgrey", size = 0.15) +
    geom_sf(aes(geometry = geometry, fill=median), size = 0.15) +
    # viridis::scale_fill_viridis(labels = scales::label_percent()) +
    scale_fill_gradientn(colours = rev(pal), labels = scales::label_percent(accuracy = kp_accuracy[x])) +
    labs(fill = "KPSE\nproportion ", title = kp_name[x]) +
    coord_sf(datum = NA, expand = FALSE) +
    theme_minimal(6) +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      legend.position = "bottom",
      legend.key.width = unit(1.15, "lines"),
      legend.key.height = unit(0.7, "lines"),
      legend.text = element_text(size = rel(1.0), face = "plain"),
      legend.title = element_text(size = rel(1.2), face = "bold"),
      legend.box.spacing = unit(0, "points"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = rel(1.4)),
      plot.tag = element_text(size = rel(2.0), face = "bold", color = fig3a_tag_color[x]),
      legend.margin = margin(5.5, 0, 0, 0, "points"),
      plot.background = element_rect(color = NA)
    )
}

fig3a_fsw <- make_pse_map("FSW")
fig3a_msm <- make_pse_map("MSM")
fig3a_pwid <- make_pse_map("PWID")
fig3a_tg <- make_pse_map("TGW")

fig3a <- gridExtra::arrangeGrob(fig3a_fsw, fig3a_msm, fig3a_pwid, fig3a_tg, nrow = 1)

# fig3a_pos <- ggpubr::ggarrange(fig3a_fsw, fig3a_msm)
# ggpubr::ggarrange(fig3a_pos, fig3a_pwid, nrow=2)
# 
# ggpubr::ggarrange(fig3a_fsw, fig3a_msm, fig3a_pwid, nrow=1)
# 
## ggsave("fig3a.png", fig3a, h = 2.7, w = 6)

## Figure 3B: log KPSE proportion w/ uncertainty ranges
## * Dimension: height = 5.5in, width = 6in

fig3b <- pse_estimates %>%
  name_kp() %>%
  ggplot() +
  geom_jitter(
    data = pse_data_overlay %>%
      filter(kp != "SW",
             prop_estimate < 1,
             prop_estimate != 0) %>%
      name_kp(),
    aes(x=iso3_idx, y=prop_estimate),
    alpha = 0.2, width = 0.35, size = 0.25
  ) +
  geom_segment(aes(x = xmin, xend = xmax, y = median, yend = median, color=has_data), size=1) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = lower, ymax = upper, fill=has_data), alpha=0.3, show.legend = FALSE) +
  geom_hline(data = data.frame(yintercept = 0.01, kp = "MSM", iso3 = c("AGO", "ZWE")) %>% name_kp(), linetype = "11", size = 0.5, aes(yintercept = yintercept), color="red") +
  scale_color_manual(values = c(wesanderson::wes_palette("Darjeeling1")[3], wesanderson::wes_palette("Darjeeling2")[c(2)])) +
  scale_fill_manual(values = c(wesanderson::wes_palette("Darjeeling1")[3], wesanderson::wes_palette("Darjeeling2")[c(2)])) +
  scale_x_continuous(breaks = 1:39, labels = iso3_sort$area_name) +
  facet_grid(kp ~ factor(four_region,
                         levels=c("Western", "Central", "Eastern", "Southern"),
                         labels = c("Western Africa", "Central Africa",
                                    "Eastern Africa", "Southern Africa")),
             scales = "free", space = "free_x") +
  scale_y_log10(breaks = scales::log_breaks(), labels = scales::label_percent(accuracy = 0.01)) +
  coord_cartesian(ylim = c(0.0001, .1)) +
  labs(x = element_blank(),
       y = "Population proportion",
       color = "Country has local surveillance data",
       shape = "Data reported\nas national",
       tag = "B") +
  theme_minimal(6) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = rel(1.4)),
        axis.text = element_text(size = rel(1.1)), 
        axis.title = element_text(size = rel(1.25)),
        legend.text = element_text(size = rel(1.1)), 
        strip.background = element_rect(fill = NA, colour = "white"), 
        panel.background = element_rect(fill = NA, color = "black")) +
  theme(axis.text.x = element_text(size = rel(1.15), angle = 45, hjust=1),
        axis.text.y = element_text(size = rel(1.15)),
        axis.title.y = element_text(size = rel(1.25), face = "bold"),
        legend.text = element_text(size = rel(1.4)),
        legend.title = element_text(size = rel(1.4)),
        panel.background = element_rect(fill=NA, color="black"),
        strip.text = element_text(size = rel(1.4), face = "bold"),
        legend.box.spacing = unit(0, "points"),
        axis.title.x = element_blank(),
        plot.tag = element_text(size = rel(2.0), face = "bold"),
        plot.background = element_rect(fill = "white", color = NA))

## ggsave("fig3b.png", fig3b, height = 5, width = 6)

fig3 <- gridExtra::arrangeGrob(fig3a, fig3b, ncol = 1, heights = c(2.7, 5))

ggsave("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Figs/Fig 3 PSE.png", fig3, height = 10, width = 8)

########################

prev_final %>%
  mutate(bigger = value > provincial_value) %>%
  filter(!is.na(bigger)) %>%
  group_by(kp, bigger) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::mutate(prop = n/sum(n))

prev_final %>%
  filter(!is.na(provincial_value)) %>%
  group_by(kp) %>%
  count()

corr_df <- distinct(prev_df, kp, region) %>%
  arrange(kp)

prev_corr <- Map(function(kp_c, region_c) {
  prev_corr <- glm(value ~ logit_gen_prev,
                   family = "binomial",
                   data = prev_df %>% filter(kp == kp_c, region == region_c) %>% mutate(denominator = round()),
                   weights = denominator)
  
  summary(prev_corr)$r.squared
}, corr_df$kp, corr_df$region)

prev_estimates <- kplhiv_art$prev_continuous_res %>%
  bind_rows(.id = "kp") %>%
  filter(is.na(iso3)) %>%
  rename(logit_fit = median,
         logit_upper = upper,
         logit_lower = lower) %>%
  mutate(provincial_value = invlogit(logit_gen_prev),
         fit = invlogit(logit_fit),
         lower = invlogit(logit_lower),
         upper = invlogit(logit_upper)
  )

national_matched_genpop

prev_country_estimates <- kplhiv_art$country %>%
  lapply(data.frame) %>%
  bind_rows() %>%
  filter(indicator == "prevalence") %>%
  left_join(national_matched_genpop %>% rename(provincial_value = mean)) %>%
  mutate(logit_gen_prev = logit(provincial_value),
         logit_fit = logit(median),
         logit_lower = logit(lower),
         logit_upper = logit(upper)
  ) %>%
  left_join(
    crossing(kp = c("FSW", "MSM", "PWID", "TGW"),
             iso3 = ssa_iso3) %>%
      left_join(prev_final %>%
                  distinct(iso3, kp) %>%
                  mutate(has_data = 1)
      )
  ) %>%
  mutate(has_data = ifelse(is.na(has_data), 0, 1),
         has_data = factor(has_data, labels = c("No", "Yes")))

convert_logis_labels <- function(x) {
  paste0(round(plogis(x)*100, 1), "%")
}

p1 <- prev_estimates %>%
  name_region(F) %>%
  name_kp(F) %>%
  ggplot(aes(x=qlogis(provincial_value), y=logit_fit)) +
  geom_point(data = prev_final %>% 
               filter(kp %in% c("MSM", "PWID", "FSW", "TGW")) %>%
               name_kp(F) %>%
               left_join(region) %>%
               name_region(F), 
             aes(y=qlogis(value), color=region), shape=16, stroke = 0, alpha = 0.4) +
  geom_line(aes(color=region), size=1) +
  geom_ribbon(aes(ymin = logit_lower, ymax = logit_upper, fill=region), alpha=0.3, show.legend = F) +
  geom_linerange(data = prev_country_estimates %>% name_kp(F), aes(ymin = logit_lower, ymax = logit_upper)) +
  geom_point(data = prev_country_estimates %>% name_kp(F), size=2.5, aes(shape = has_data)) +
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  # geom_text(data = data.frame(kp = c("FSW", "MSM", "PWID", "TG"), label = c("FSW", "MSM", "PWID", "TG")), aes(x=logit(0.25), y=logit(0.9), label = label)) +
  moz.utils::standard_theme() +
  scale_y_continuous(labels = convert_logis_labels) +
  scale_x_continuous(labels = convert_logis_labels, limits = c(NA, logit(0.4))) +
  scale_shape_manual(values = c(1, 16)) +
  scale_manual("color", 2) +
  scale_manual("fill", 2) +
  labs(shape = "Country has local surveillance data", color = element_blank(), y = "KP HIV prevalence (logit scale)", x = "Total population HIV prevalence (logit scale)", tag = "A")+
  theme(panel.border = element_rect(fill=NA, color="black"),
        legend.position = "right",
        aspect.ratio = 1,
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold"),
        panel.spacing.x = unit(1, "lines")) +
  facet_wrap(~kp, nrow=1)

p2 <- prev_estimates %>%
  name_region(F) %>%
  name_kp(F) %>%
  ggplot(aes(x=provincial_value, y=fit)) +
  # geom_line(size=1) +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.3, show.legend = F) +
  geom_point(data = prev_final %>% 
               filter(kp %in% c("MSM", "PWID", "FSW", "TGW")) %>%
               name_kp(F) %>%
               left_join(region) %>%
               name_region(F), 
             aes(y=value, color=region), shape=16, stroke = 0, alpha = 0.4) +
  geom_line(aes(color=region), size=1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill=region), alpha=0.3, show.legend = F) +
  geom_linerange(data = prev_country_estimates %>% name_kp(F), aes(y = median, ymin = lower, ymax = upper)) +
  geom_point(data = prev_country_estimates %>% name_kp(F), size=2.5, aes(y=median, shape = has_data)) +
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  # geom_text(data = data.frame(kp = c("FSW", "MSM", "PWID", "TG"), label = c("FSW", "MSM", "PWID", "TG")), aes(x=0.3, y=0.9, label = label)) +
  moz.utils::standard_theme() +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0,0.4)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  scale_shape_manual(values = c(1, 16)) +
  scale_manual("color", 2) +
  scale_manual("fill", 2) +
  labs(shape = "Country has local surveillance data", color = element_blank(), y = "KP HIV prevalence", x = "Total population HIV prevalence", tag = "B")+
  theme(panel.border = element_rect(fill=NA, color="black"),
        legend.position = "right",
        aspect.ratio = 1,
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text = element_text(color = "white"),
        panel.spacing.x = unit(1, "lines")) +
  facet_wrap(~kp, nrow=1)

p3 <- prev_estimates %>%
  name_region(F) %>%
  name_kp(F) %>%
  ggplot(aes(x=provincial_value, y=fit/provincial_value)) +
  # geom_line(size=1) +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.3, show.legend = F) +
  geom_point(data = prev_final %>% 
               filter(kp %in% c("MSM", "PWID", "FSW", "TGW")) %>%
               
               name_kp(F) %>%
               left_join(region) %>%
               name_region(F), 
             aes(y=value/provincial_value, color=region), shape=16, stroke = 0, alpha = 0.4) +
  geom_line(aes(color=region), size=1) +
  geom_ribbon(aes(ymin = lower/provincial_value, ymax = upper/provincial_value, fill=region), alpha=0.3, show.legend = F) +
  # geom_text(data = data.frame(kp = c("FSW", "MSM", "PWID", "TG"), label = c("FSW", "MSM", "PWID", "TG")), aes(x=0.3, y=100, label = label)) +
  moz.utils::standard_theme() +
  scale_y_log10() +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0,0.4)) +
  scale_manual("color", 2) +
  scale_manual("fill", 2) +
  geom_hline(aes(yintercept = 1), linetype = 2) +
  labs(shape = "Country has local surveillance data", color = element_blank(), y="Prevalence ratio", x = "Total population HIV prevalence", tag = "C")+
  theme(panel.border = element_rect(fill=NA, color="black"),
        legend.position = "right",
        aspect.ratio = 1,
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        strip.text = element_text(color = "white"),
        panel.spacing.x = unit(1, "lines")) +
  facet_wrap(~kp, nrow=1)

prev_fig <- ggpubr::ggarrange(p1, p2, p3, ncol=1, common.legend = TRUE, legend = "bottom")
png("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Figs/Fig 5 prevalence.png", width = 1100, height = 800)
prev_fig
dev.off()


art_estimates <- kplhiv_art$art_continuous_res %>%
  lapply(data.frame) %>%
  bind_rows(.id = "kp") %>%
  filter(is.na(iso3)) %>%
  rename(logit_fit = median,
         logit_upper = upper,
         logit_lower = lower) %>%
  mutate(provincial_value = invlogit(logit_gen_art),
         fit = invlogit(logit_fit),
         lower = invlogit(logit_lower),
         upper = invlogit(logit_upper)
  )

national_matched_genpop

art_country_estimates <- kplhiv_art$country %>%
  lapply(data.frame) %>%
  bind_rows() %>%
  filter(indicator == "art_coverage") %>%
  left_join(national_matched_genpop %>% rename(provincial_value = mean)) %>%
  mutate(logit_gen_art = logit(provincial_value),
         logit_fit = logit(median),
         logit_lower = logit(lower),
         logit_upper = logit(upper)
  ) %>%
  left_join(
    crossing(kp = c("FSW", "MSM", "PWID", "TGW"),
             iso3 = ssa_iso3) %>%
      left_join(art_final %>%
                  distinct(iso3, kp) %>%
                  mutate(has_data = 1)
      )
  ) %>%
  mutate(has_data = ifelse(is.na(has_data), 0, 1),
         has_data = factor(has_data, labels = c("No", "Yes")))


res_1529$MSM$country %>% 
  filter(indicator == "art_cov") %>%
  mutate(source = "15-29") %>%
  select(iso3, median, source) %>%
  bind_rows(
    kplhiv_art$MSM$country %>% 
      filter(indicator == "art_cov") %>%
      mutate(source = "15-49") %>%
      select(iso3, median, source)
  ) %>%
    pivot_wider(names_from = source, values_from = median) %>%
  mutate(diff = `15-29` - `15-49`) %>%
  reframe(calculate_quantile(diff))

imp_denomin <- art_final %>%
  filter(!is.na(denominator),
         denominator != 0) %>%
  group_by(kp) %>%
  summarise(quant = quantile(denominator, 0.25))

convert_logis_labels <- function(x) {
  paste0(plyr::round_any(plogis(x)*100, accuracy = 1, round), "%")
}

p5 <- art_estimates %>%
  bind_rows() %>%
  name_kp(F) %>%
  # filter(model == "betabinomial") %>%
  ggplot(aes(x=qlogis(provincial_value), y=logit_fit)) +
    geom_point(data = art_final %>% 
                 filter(kp %in% c("MSM", "PWID", "FSW", "TGW")) %>%
                 name_kp(F) %>%
                 left_join(region) %>%
                 name_region(F), 
               aes(y=qlogis(value), color=region), shape=16, stroke = 0, alpha = 0.6, size=2.5) +
    geom_line(size=1) +
    geom_ribbon(aes(ymin = logit_lower, ymax = logit_upper), alpha=0.2) +
    geom_linerange(data = art_country_estimates %>% name_kp(F), aes(ymin = logit_lower, ymax = logit_upper)) +
    geom_point(data = art_country_estimates %>% name_kp(F), size=2.5, aes(shape = has_data)) +
    geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
    moz.utils::standard_theme() +
    scale_y_continuous(labels = convert_logis_labels, breaks = logit(c(0.1, 0.25, 0.5, 0.75, 0.9))) +
    scale_x_continuous(labels = convert_logis_labels, breaks = logit(c(0.1, 0.25, 0.5, 0.75, 0.9))) +
    scale_shape_manual(values = c(1, 16)) +
    scale_manual("color", 2) +
    labs(shape = "Country has local surveillance data", y = "KP ART coverage (logit scale)", x = "Total population ART coverage (logit scale)", color = element_blank(), tag = "A")+
  theme(panel.border = element_rect(fill=NA, color="black"),
        panel.spacing = unit(2, "lines"),
        axis.text = element_text(size = 14),
        aspect.ratio = 1,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(face="bold")) +
  facet_wrap(~kp, nrow=1) +
  coord_cartesian(ylim = c(logit(0.05), logit(0.95)), xlim = c(logit(0.05), logit(0.95)))

p6 <- art_estimates %>%
  bind_rows() %>%
  name_kp(F) %>%
  # filter(model == "betabinomial") %>%
  ggplot(aes(x=provincial_value, y=fit)) +
    geom_point(data = art_final %>% 
                 filter(kp %in% c("MSM", "PWID", "FSW", "TGW")) %>%
                 name_kp(F) %>%
                 left_join(region) %>%
                 name_region(F), 
               aes(y=value, color=region), shape=16, stroke = 0, alpha = 0.6, size=2.5) +
    geom_line(size=1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) +
    geom_linerange(data = art_country_estimates %>% name_kp(F), aes(y=median, ymin = lower, ymax = upper)) +
    geom_point(data = art_country_estimates %>% name_kp(F), size=2.5, aes(y=median, shape = has_data)) +
    geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
    moz.utils::standard_theme() +
    scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
    scale_y_continuous(labels = scales::label_percent()) +
    scale_shape_manual(values = c(1, 16)) +
    scale_manual("color", 2) +
    labs(shape = "Country has local surveillance data", y = "KP ART coverage", x = "Total population ART coverage", color = element_blank(), tag = "B")+
    theme(panel.border = element_rect(fill=NA, color="black"),
          panel.spacing = unit(2, "lines"),
          aspect.ratio = 1,
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          strip.text = element_text(color = "white")) +
    facet_wrap(~kp, nrow=1) +
    coord_cartesian(ylim = c(0.1,1), xlim = c(0.1,1)) 

art_fig <- ggpubr::ggarrange(p5, p6, ncol=1, common.legend = TRUE, legend = "bottom")

png("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Figs/Fig 6 art coverage.png", width = 1200, height = 600)
art_fig
dev.off()

prev_estimates %>%
  mutate(provincial_value = round(provincial_value, 3)) %>%
  filter(region == "ESA",
         provincial_value %in% c(0.011, 0.151)) %>%
  arrange(provincial_value, kp) %>%
  select(kp, provincial_value, median, lower, upper) %>%
  mutate(across(median:upper, ~.x*100),
         across(median:upper, ~round(.x, 0)))

art_estimates %>%
  filter(provincial_value %in% c(0.4, 0.8)) %>%
  arrange(provincial_value, kp) %>%
  select(kp, provincial_value, median, lower, upper) %>%
  mutate(across(median:upper, ~.x*100),
         across(median:upper, ~round(.x, 0)),
         across(median:upper, ~.x-(provincial_value*100)))

#### Final results para


kplhiv_art$region$All %>% 
  filter(indicator %in% c("pse_prop", "plhiv_prop")) %>%
  mutate(across(lower:upper, ~round(.x*100, 1)))

kplhiv_art$region[!names(kplhiv_art$region) == "All"] %>%
  lapply(data.frame) %>%
  bind_rows(.id = "kp") %>%
  filter(region == "SSA", indicator %in% c("pse_prop", "plhiv_prop")) %>%
  mutate(across(lower:upper, ~round(.x*100, 1)))

kplhiv_art$region[!names(kplhiv_art$region) == "All"] %>%
  lapply(data.frame) %>%
  bind_rows(.id = "kp") %>%
  filter(region == "SSA", indicator == "kplhiv") %>%
  mutate(across(lower:upper, ~signif(.x, 2)))

### Discussion

kplhiv_art$region[!names(kplhiv_art$region) == "All"] %>%
  lapply(data.frame) %>%
  bind_rows(.id = "kp") %>%
  filter(indicator == "pse_count",
         region == "SSA")

kplhiv_art$country$MSM %>%
  filter(indicator == "pse_urban_prop", kp == "MSM",
         median > 0.01)

#### Supplementary figs

total_studies <- bind_rows(pse_final %>% mutate(indicator = "PSE"),
          prev_final %>% mutate(indicator = "HIV prevalence"),
          art_final %>% mutate(indicator = "ART coverage")) %>%
  distinct(indicator, study_idx) %>%
  arrange(study_idx)

ethic <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1murnx2dH0W_94gFFuJ-oSW0ugdmv8WPinmog0_V-0FI/edit#gid=1735078680") %>%
  filter(`Study idx` %in% unique(total_studies$study_idx))

ethic$year <- as.character(ethic$year)

all_kp <- data.frame(kp = c("FSW", "MSM", "PWID", "TG")) %>%
  bind_cols(ethic %>%
              filter(kp == "ALL") %>%
              select(`Study idx`, iso3, year, Study)
  )

ethic <- ethic %>% 
  select(`Study idx`, iso3, year, kp, Study) %>%
  filter(!`Study idx` %in% all_kp$`Study idx`) %>%
  bind_rows(all_kp) %>%
  rename(study_idx = `Study idx`)


studies_used <- total_studies %>%
  left_join(ethic) %>%
  mutate(iso3 = countrycode(iso3, "iso3c", "country.name", custom_match = cc_plot()),
         Study = str_to_title(Study)) %>%
  select(-study_idx)

write_csv(studies_used, "~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/ST3 studies.csv")

crossing(iso3 = ssa_iso3,
         kp = c("FSW", "MSM", "PWID", "TG")) %>%
  left_join(pse_final %>%
              ungroup() %>%
              group_by(iso3, kp) %>%
              filter(year == max(year)) %>%
              distinct(iso3, kp, year) %>%
              rename(pse = year)) %>%
  left_join(
    prev_final %>%
      ungroup() %>%
      group_by(iso3, kp) %>%
      filter(year == max(year)) %>%
      distinct(iso3, kp, year) %>%
      rename(prevalence = year) %>%
      left_join(
        art_final %>%
          ungroup() %>%
          group_by(iso3, kp) %>%
          filter(year == max(year)) %>%
          distinct(iso3, kp, year) %>%
          rename(art_coverage = year)
      )
  ) %>%
  pivot_wider(names_from = "kp", values_from = c("pse", "prevalence", "art_coverage")) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/data availability.csv", na="")

crossing(iso3 = ssa_iso3,
         kp = c("FSW", "MSM", "PWID", "TG")) %>%
  left_join(pse_final %>%
              ungroup() %>%
              group_by(iso3, kp) %>%
              filter(year == max(year)) %>%
              distinct(iso3, kp, year) %>%
              rename(pse = year)) %>%
  left_join(
    prev_final %>%
      ungroup() %>%
      group_by(iso3, kp) %>%
      filter(year == max(year)) %>%
      distinct(iso3, kp, year) %>%
      rename(prevalence = year) %>%
      left_join(
        art_final %>%
          ungroup() %>%
          group_by(iso3, kp) %>%
          filter(year == max(year)) %>%
          distinct(iso3, kp, year) %>%
          rename(art_coverage = year)
      )
  ) %>%
  pivot_longer(-c(iso3, kp)) %>%
  filter(!is.na(value)) %>%
  mutate(foo = 1) %>%
  group_by(iso3) %>%
  summarise(tot = sum(foo)) %>%
  ungroup() %>%
  summarise(med = median(tot))

## pse method

pse_fixed <- read_csv("R/pse_fixed.csv")
pse_random <- read_csv("R/pse_random.csv")

pse_fixed %>%
  bind_rows(pse_random) %>%
  mutate(fe_method = factor(fe_method, levels = c("empirical", "PLACE/Mapping", "other methods"))) %>%
  arrange(fe_method) %>%
  select(-id.method) %>%
  pivot_wider(names_from = "kp", values_from = c(fixed, random)) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/ST5 PSE method.csv")

pse_method_plot <- pse_random %>%
  separate(random, into = c("estimate", "lower", "upper"), sep = " ") %>%
  mutate(across(c(lower, upper), ~str_remove_all(.x, "\\(|,|\\)"))) %>%
  type.convert(as.is = T) %>%
  bind_rows(
    pse_fixed %>%
      separate(fixed, into = c("estimate", "lower", "upper"), sep = " ") %>%
      mutate(across(c(lower, upper), ~str_remove_all(.x, "\\(|,|\\)"))) %>%
      type.convert(as.is = T) %>%
      filter(!(kp == "TGW" & fe_method == "other methods")) %>%
      rename(method = fe_method) 
  ) %>%
  mutate(
    method = ifelse(method == "other methods", "MM - mixed", method),
    method = ifelse(method == "Multiple methods - empirical", "MM - empirical", method),
    method = fct_inorder(method)
        ) %>%
  name_kp(F) %>%
  ggplot(aes(x=estimate, y=fct_rev(method))) +
    geom_pointrange(aes(xmin = lower, xmax = upper)) +
    geom_vline(aes(xintercept = 0), linetype = 2) +
    facet_wrap(~kp) +
    standard_theme() +
    labs(y=element_blank(), x="Log odds ratios")
    
png("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/S3 PSE method effects.png", width = 600, height = 500)
pse_method_plot
dev.off()

## Supplementary country table figs

country_res %>%
  filter(indicator == "pse_urban_prop") %>%
  bind_rows(
    region_res %>%
      filter(indicator == "pse_urban_prop") %>%
      mutate(iso3 = "ZZZ")
  ) %>%
  mutate(across(lower:upper, ~round(100*.x, 2)),
         text = paste0(median, " (", lower, ", ", upper, ")")) %>%
  select(kp, region, iso3, text) %>%
  pivot_wider(names_from = kp, values_from = text) %>%
  factor_region() %>%
  arrange(region, iso3) %>%
  mutate(iso3 = countrycode::countrycode(iso3, "iso3c", "country.name", custom_match = cc_plot())) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/S7 urban PSE.csv")

country_res %>%
  filter(indicator == "prevalence") %>%
  bind_rows(
    region_res %>%
      filter(indicator == "prevalence") %>%
      mutate(iso3 = "ZZZ")
  ) %>%
  mutate(across(lower:upper, ~round(100*.x, 0)),
         text = paste0(median, " (", lower, ", ", upper, ")")) %>%
  select(kp, region, iso3, text) %>%
  pivot_wider(names_from = kp, values_from = text) %>%
  factor_region() %>%
  arrange(region, iso3) %>%
  mutate(iso3 = countrycode::countrycode(iso3, "iso3c", "country.name", custom_match = cc_plot())) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/S8 prevalence.csv")

country_res %>%
  filter(indicator == "art_coverage") %>%
  bind_rows(
    region_res %>%
      filter(indicator == "art_coverage") %>%
      mutate(iso3 = "ZZZ")
  )  %>%
  mutate(across(lower:upper, ~round(100*.x, 0)),
         text = paste0(median, " (", lower, ", ", upper, ")")) %>%
  select(kp, region, iso3, text) %>%
  pivot_wider(names_from = kp, values_from = text) %>%
  factor_region() %>%
  arrange(region, iso3) %>%
  mutate(iso3 = countrycode::countrycode(iso3, "iso3c", "country.name", custom_match = cc_plot())) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Supplementary figs/S9 art coverage.csv")

