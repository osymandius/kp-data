library(countrycode)
library(tidyverse)

region <- read.csv("~/Documents/GitHub/fertility_orderly/global/region.csv") %>%
  mutate(iso3 = toupper(iso3))

iso3_vec <- c("BDI", "BWA", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")

###### Subnational/national PSE from data ########

pse_raw <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/pse_surveillance_only.csv", na="")

pse_raw <- pse_raw %>%
  mutate(
    iso3 = countrycode(country.name, "country.name", "iso3c"),
    is_national = ifelse(country.name == area_name, 1, 0),
    has_age = ifelse(!is.na(age_group), 1, 0)) %>%
  filter(iso3 %in% c(iso3_vec, "ZAF")) %>%
  distinct(kp, area_name, year, pse, .keep_all=TRUE) %>%
  left_join(region)

pse_raw %>%
  group_by(kp, region) %>%
  count(has_age) %>%
  mutate(prop = 1-sum(n[has_age])/sum(n))
    
pse_raw %>%
  filter(kp == "TG") %>%
  group_by(kp, iso3) %>%
  count()

########### UNAIDS nationally adequate size estimates #####

unaids_nat_adequate_pse <- read_csv("~/Dropbox/Work Streams/2021/Key populations/Data consolidation/UNAIDS/nationally_adequate_pse.csv", na = "")

unaids_nat_adequate_pse <- unaids_nat_adequate_pse %>%
  pivot_longer(-c(region:kp), values_to = "pse", names_to = "year") %>%
  mutate(iso3 = countrycode(area_name, "country.name", "iso3c"),
         area_name = countrycode(iso3, "iso3c", "country.name")) %>%
  filter(!is.na(pse), !iso3 %in% c("MDG", "SYC")) %>%
  select(iso3, area_name, kp, year, pse)

id <- lapply(unique(unaids_nat_adequate_pse$iso3), function(x){
  orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

names(id) <- unique(unaids_nat_adequate_pse$iso3)
id <- id[!is.na(id)]

pop <- lapply(paste0("archive/aaa_scale_pop/", id, "/interpolated_population.csv"), read.csv)

pop <- pop %>%
  bind_rows() %>%
  filter(area_id %in% unique(unaids_nat_adequate_pse$iso3),
         age_group %in% filter(naomi::get_age_groups(), age_group_sort_order %in% 16:22)$age_group) %>%
  group_by(area_id, year, sex) %>%
  summarise(population = sum(population))

unaids_nat_adequate_pse <- unaids_nat_adequate_pse %>%
  mutate(sex = case_when(
    kp %in% c("FSW", "TG") ~ "female",
    kp == "MSM" ~ "male",
    kp == "PWID" ~ "both"
  ))

pop <- pop %>%
  bind_rows(
    pop %>%
      group_by(area_id, year) %>%
      summarise(population = sum(population)) %>%
      mutate(sex = "both")
  )

unaids_nat_adequate_pse <- unaids_nat_adequate_pse %>%
  type_convert() %>%
  left_join(pop, by=c("year", "iso3" = "area_id", "sex")) %>%
  mutate(population_proportion = pse/population)

unaids_nat_adequate_pse %>%
  ggplot(aes(x=year>2016, y=population_proportion)) +
  geom_jitter(width=0.1) +
  geom_hline(data = data.frame(kp = "MSM"), aes(yintercept = 0.01), linetype = 3) +
  scale_x_discrete(labels = c("Before\n2016", "2016 and\nbeyond")) +
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(x=element_blank(), y="PSE proportion") +
  facet_wrap(~kp, nrow=1) +
  moz.utils::standard_theme() +
  theme(panel.border = element_rect(fill=NA, color="black"))

###### MSM proportion from data #####

pse_dat %>%
  bind_rows() %>%
  filter(kp == "MSM") %>%
  # mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c")) %>%
  left_join(region) %>%
  ggplot(aes(x=year, y=population_proportion)) +
  geom_jitter() +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,.1)) +
  geom_hline(aes(yintercept = 0.01), color="red", linetype = 2, size=1) +
  moz.utils::standard_theme() +
  labs(x=element_blank(), y="MSM population proportion") +
  facet_wrap(~region)

pse_dat %>%
  bind_rows() %>%
  filter(kp == "MSM") %>%
  # mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c")) %>%
  left_join(region)  %>%
  ggplot(aes(x=year, y=population_proportion, group=year)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,.1)) +
  geom_hline(aes(yintercept = 0.01), color="red", linetype = 2, size=1) +
  moz.utils::standard_theme() +
  labs(x=element_blank(), y="MSM population proportion") +
  facet_wrap(~region) +
  theme(panel.border = element_rect(fill=NA, color="black"))

pse_dat %>%
  bind_rows() %>%
  filter(kp == "MSM") %>%
  # mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c")) %>%
  left_join(region) %>%
  ggplot(aes(x=region, y=population_proportion)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,.1)) +
  geom_hline(aes(yintercept = 0.01), color="red", linetype = 2, size=1) +
  moz.utils::standard_theme() +
  labs(x=element_blank(), y="MSM population proportion")


######## PSE results  ########

pse_out <- pse_out %>%
  left_join(pse_dat %>%
              select(iso3, kp) %>%
              distinct() %>%
              mutate(has_data = 1)) %>%
  mutate(has_data = ifelse(is.na(has_data), "No data", "Data"))

pse_out <- pse_out %>%
  mutate(iso3_idx = as.numeric(factor(iso3)),
         xmin = iso3_idx - 0.48,
         xmax = iso3_idx + 0.48)


pse_dat %>%
  filter(kp != "TG") %>%
  mutate(area_name = countrycode(iso3, "iso3c", "country.name")) %>%
  # filter(population_proportion < 0.1) %>%
  ggplot(aes(x=factor(iso3))) +
    geom_jitter(aes(y=population_proportion), alpha=0.3) +
    # ungeviz::geom_hpline(data=pse_out, aes(y=median, color=factor(has_data)), size=1) +
    geom_segment(data=pse_out, aes(x = xmin, xend = xmax, y = median, yend = median, color=has_data), size=1) +
    geom_rect(data=pse_out, aes(xmin = xmin, xmax = xmax, ymin = lower, ymax = upper, fill=has_data), alpha=0.3, show.legend = FALSE) +
    geom_hline(data = data.frame(yintercept = 0.01, kp = "MSM", iso3 = c("AGO", "ZWE")), linetype = 3, aes(yintercept = yintercept), color="red") +
    facet_wrap(~kp, nrow=3, scales="free") +
    scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
    scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
    # scale_y_continuous(labels = scales::label_percent()) +
    scale_y_log10(breaks = scales::log_breaks(), labels = scales::label_percent(accuracy = 0.001)) + 
    labs(x=element_blank(), y="Population proportion", color = "Informed by:") +
    moz.utils::standard_theme() +
    theme(axis.text.x = element_text(size=10))

############# Prevalence ############

