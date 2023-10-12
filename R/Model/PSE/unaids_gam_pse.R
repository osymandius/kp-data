library(tidyverse)
library(countrycode)
devtools::install_github("mrc-ide/moz.utils")
library(moz.utils)
library(lme4)

gam <- lapply(1:3, function(x) {
  dat <- readxl::read_excel("~/Downloads/Copy of KP proportions regression - Copy.xlsx", sheet = x)
  colnames(dat) <- c("country", "year", "pse_prop", "outside_range", "u1")
  dat
})

names(gam) <- c("FSW", "MSM", "PWID")

gam <- gam %>%
  bind_rows(.id = "kp") %>%
  mutate(iso3 = countrycode(country, "country.name", "iso3c")) %>%
  filter(iso3 %in% ssa_iso3(),
         !(iso3 == "NER" & kp == "FSW")) %>%
  select(kp, iso3, year, pse_prop, outside_range, u1) %>%
  left_join(read_sf(moz.utils::national_areas()) %>%
              mutate(iso3 = area_id) %>%
              arrange(iso3) %>%
              group_by(iso3) %>%
              mutate(id.iso3 = cur_group_id()) %>%
              select(area_id, id.iso3) %>%
              st_drop_geometry(), by = c("iso3" = "area_id")) %>%
  mutate(idx = row_number(),
         id.iso3_rep = id.iso3,
         logit_pse = plogis(pse_prop)) %>%
  group_by(kp) %>%
  mutate(mean_centred_pse = pse_prop/mean(pse_prop),
         outside_range = ifelse(is.na(outside_range), 0, 1),
         u1 = ifelse(is.na(u1), 0, 1),
         exclude = outside_range + u1
         # u1 = case_when(
         #   kp == "MSM" & is.na(u1) ~ 0,
         #   kp == "MSM" & u1 == "Y" ~ 1,
         #   TRUE ~ NA
         #   
         # )
         )

gam %>%
  ggplot(aes(x=year, y=pse_prop)) +
  geom_point(aes(color = iso3), show.legend = F) +
  geom_line(aes(color = iso3), show.legend = F)  +
  facet_wrap(~kp)

# mod <- lmer(plogis(pse_prop) ~ year + (1 + year|iso3), data = gam)
fsw_mod <- lmer(pse_prop*100 ~ year + (1+ year|iso3), data = gam %>% filter(kp == "FSW",
                                                                            pse_prop < 0.02))

msm_mod18 <- lmer(pse_prop*100 ~ year + (1|iso3), data = gam %>% filter(kp == "MSM",
                                                                        outside_range == 0,
                                                                        year < 2021,
                                                                        pse_prop < 0.02))

msm_mod21 <- lmer(pse_prop*100 ~ year + (1|iso3), data = gam %>% filter(kp == "MSM",
                                                                            outside_range == 0,
                                                                            u1 == 0,
                                                                            pse_prop < 0.02))

msm_mod_all <- lmer(pse_prop*100 ~ year + (1|iso3), data = gam %>% filter(kp == "MSM",
                                                                              pse_prop < 0.02))

pwid_mod <- lmer(pse_prop*100 ~ year + (1|iso3), data = gam %>% filter(kp == "PWID",
                                                                       pse_prop < 0.02))

summary(fsw_mod)

summary(msm_mod18)
summary(msm_mod21)
summary(msm_mod_all)

summary(pwid_mod)

pred <- lapply(c(fsw_mod, msm_mod18, msm_mod21, msm_mod_all, pwid_mod), function(x) {
  df <- data.frame(year = 2010:2022) %>%
    mutate(pred = predict(x, data.frame(year = 2010:2022), re.form = NA),
           median = qlogis(pred)) 
})

names(pred) <- c("FSW_All", "MSM_Spectrum Quickstart threshold", "MSM_1% threshold", "MSM_All", "PWID_All")
pred <- bind_rows(pred, .id = "kp") %>%
  separate(kp, into = c("kp", "source"), sep = "_") %>%
  mutate(source = factor(source, levels = c("All", "Spectrum Quickstart threshold", "1% threshold")))

gam %>%
  filter(pse_prop < 0.02) %>%
  name_kp(F) %>%
  mutate(source = case_when(
    outside_range == 1 ~ "Spectrum Quickstart threshold",
    u1 == 1 ~ "1% threshold",
    TRUE ~ NA)) %>%
  mutate(source = factor(source, levels = c("All", "Spectrum Quickstart threshold", "1% threshold"))) %>%
  ggplot(aes(x=year, y=pse_prop)) +
    geom_point(aes(shape = factor(exclude), color = source), size = 2.5, show.legend = F) +
    # geom_line() +
    geom_line(data = pred %>% 
                filter(!(kp == "MSM" & source == "Spectrum Quickstart threshold" & year > 2020)) %>%
                name_kp(F), aes(y=median, color = source), linewidth = 1.5) +
    standard_theme() +
    scale_y_continuous(labels = scales::label_percent(), expand = expansion(0, 0)) +
    scale_x_continuous(labels = scales::label_number(accuracy = 1, big.mark = "")) +
    scale_shape_manual(values = c(16, 1)) +
    theme(panel.grid = element_blank(),
          panel.spacing.x = unit(2, "lines")) +
    coord_cartesian(ylim = c(0, 0.015)) +
    scale_manual("color", 4) +
    labs(x=element_blank(), y="PSE proportion (of total population)", color = element_blank()) +
    facet_wrap(~kp)

gam %>%
  filter(kp == "FSW") %>%
  group_by(iso3) %>%
  filter(n() > 1) %>%
  ggplot(aes(x=year, y=pse_prop)) +
    geom_point() +
    geom_line() +
    facet_wrap(~countrycode(iso3, "iso3c", "country.name")) +
    standard_theme() +
    scale_percent() +
    scale_x_continuous(labels = scales::label_number(accuracy = 1, big.mark = "")) +
    labs(x=element_blank(), y="PSE proportion (total population)", title = "FSW PSE proportions") +
    expand_limits(y=0) +
    theme(panel.grid = element_blank())
