pse_dat <- pse_dat %>%
  filter(kp == "MSM") %>%
  mutate(source = "15-29") %>%
  bind_rows(read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final_sourced.csv") %>%
              mutate(source = "15-49")) %>%
  filter(prop_estimate < 0.1)

pse_dat %>%
  left_join(region) %>%
  ggplot(aes(x=source, y=prop_estimate)) +
    geom_boxplot() +
    standard_theme() +
    geom_hline(aes(yintercept = 0.01), linetype = 2, color = "red") +
    scale_percent() +
    labs(x=element_blank(), y="PSE proportion", title = "Raw PSE proportions")

res15 <- readRDS("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/kplhiv_art_15-29.rds")

res <- readRDS("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/kplhiv_art.rds")

res <- res %>%
  lapply("[[", "country") %>%
  bind_rows(.id = "kp") %>%
  filter(kp == "MSM") %>%
  mutate(source = "15-49")

res15 <- res15 %>%
  lapply("[[", "country") %>%
  bind_rows(.id = "kp") %>%
  filter(kp == "MSM") %>%
  mutate(source = "15-29")

res <- res15 %>%
  bind_rows(res)

res %>%
  left_join(region) %>%
  filter(indicator == "pse_urban") %>%
  ggplot(aes(x=source, y=median, color = region)) +
    geom_point() +
    scale_manual("color", 2) +
    scale_percent() +
    standard_theme() +
    expand_limits(y=0) +
    labs(y="PSE proportion", x=element_blank(), title = "Modelled urban PSE proportions") +
    geom_hline(aes(yintercept = 0.01), linetype = 2, color = "red")

prev15 <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_estimates_15-29.csv") %>%
  mutate(source = "15-29")

prev <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_estimates.csv") %>%
  mutate(source = "15-49")

prev %>%
  bind_rows(prev15) %>%
  filter(kp == "MSM") %>%
  mutate(provincial_value = invlogit(logit_gen_prev),
         logit_fit = logit(median),
         logit_lower = logit(lower),
         logit_upper = logit(upper),
         fit = median) %>%
  ggplot(aes(x=qlogis(provincial_value), y=logit_fit, group = source)) +
  geom_line(aes(color=source), size=1) +
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  # geom_text(data = data.frame(kp = c("FSW", "MSM", "PWID", "TG"), label = c("FSW", "MSM", "PWID", "TG")), aes(x=logit(0.25), y=logit(0.9), label = label)) +
  moz.utils::standard_theme() +
  scale_y_continuous(labels = convert_logis_labels) +
  scale_x_continuous(labels = convert_logis_labels, limits = c(NA, logit(0.4))) +
  scale_color_manual(values = c(wesanderson::wes_palette("Zissou1")[c(4,1)])) +
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(4,1)])  +
  labs(y = "KP HIV prevalence\n(logit scale)", x = "Total population HIV prevalence (logit scale)")+
  theme(panel.border = element_rect(fill=NA, color="black"),
        legend.position = "right",
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold")) +
  facet_wrap(~region, nrow=1)


art15 <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_national_matched_estimates_15-29.csv") %>%
  mutate(source = "15-29")

art <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_national_matched_estimates.csv") %>%
  mutate(source = "15-49")

art %>%
  filter(kp == "MSM") %>%
  bind_rows(art15 %>%
              filter(kp == "MSM") %>%
              select(-provincial_value) %>%
              left_join(art %>% filter(kp == "MSM") %>% select(iso3, provincial_value))) %>%
  ggplot(aes(x=provincial_value, y=median, color=source)) +
    geom_point(size = 2) +
    geom_abline(aes(intercept = 0, slope = 1), linetype = 2) +
    standard_theme() +
    scale_manual("color", 2) +
    scale_percent() +
    expand_limits(y=c(0,1), x=c(0,1)) +
    scale_x_continuous(labels = scales::label_percent()) +
    labs(x="Total population male ART coverage", y="MSM ART coverage") +
    theme(aspect.ratio = 1)

art %>%
  filter(kp == "MSM") %>%
  bind_rows(art15 %>%
              filter(kp == "MSM") %>%
              select(-provincial_value) %>%
              left_join(art %>% filter(kp == "MSM") %>% select(iso3, provincial_value))) %>%
  select(iso3, median, source) %>%
  pivot_wider(names_from = source, values_from = median) %>%
  mutate(diff = `15-29` - `15-49`) %>%
  ggplot(aes(x=1, y=diff)) +
    geom_boxplot() +
    scale_percent() +
    standard_theme()
