library(countrycode)
library(tidyverse)
library(sf)

region <- read.csv("~/Documents/GitHub/fertility_orderly/global/region.csv") %>%
  mutate(iso3 = toupper(iso3))

iso3_vec <- c("BDI", "BWA", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")

pse_out <- read_csv("R/Model/PSE/pse_out_surveillance_only.csv")
pse_dat <- read_csv("R/Model/PSE/pse_surveillance_data.csv")

###### Subnational/national PSE from data ########

pse_raw <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_surveillance_only.csv", na="")

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

unaids_nat_adequate_pse %>% 
  filter(year > 2015, kp == "MSM") %>%
  mutate(under_1 = population_proportion<0.01)
  count(under_1)
  
filter(unaids_nat_adequate_pse, year > 2015, kp == "MSM", population_proportion < 0.05)$population_proportion %>%
  median

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

msm_pse_proportions <- pse_dat %>%
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

png(file="~/Dropbox/oli backup/Key populations/Data consolidation paper/Figs/msm_pse_proportions.png", width=250, height=300)
msm_pse_proportions
dev.off()

pse_dat %>%
  bind_rows() %>%
  filter(kp == "MSM") %>%
  left_join(region) %>%
  mutate(under_1 = population_proportion<0.01) %>%
  group_by(region) %>%
  count(under_1)



  


######## PSE results  ########

pse_out <- read_csv("R/Model/PSE/pse_out_surveillance_only.csv")
pse_dat <- read_csv("R/Model/PSE/pse_surveillance_data.csv")

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

# pse_dat <- pse_dat %>%
#   left_join(region) %>%
#   bind_rows(
#     pse_dat %>%
#       left_join(region) %>%
#       mutate(
#         iso3 = case_when(
#           str_detect(four_region, "Central") ~ "CA",
#           str_detect(four_region, "Eastern") ~ "EA",
#           str_detect(four_region, "Western") ~ "WA",
#           str_detect(four_region, "Southern") ~ "SA"
#         ),
#         four_region = paste0(str_sub(four_region, 0, 4), "aa")
#       )
#   ) %>% arrange(four_region)

pse_out <- pse_out %>%
  left_join(pse_dat %>%
              select(iso3, kp) %>%
              distinct() %>%
              mutate(has_data = 1)) %>%
  mutate(has_data = ifelse(is.na(has_data), 0, 1),
         has_data = factor(has_data, labels = c("No data", "Data")))

# regional_pse <- pse_out %>%
#   left_join(region) %>%
#   group_by(four_region, kp) %>%
#   summarise(name = c("lower", "median", "upper"), value = quantile(median, c(0.25, 0.5, 0.75))) %>%
#   pivot_wider(names_from = name, values_from = value) %>%
#   mutate(
#     iso3 = case_when(
#       str_detect(four_region, "Central") ~ "CA",
#       str_detect(four_region, "Eastern") ~ "EA",
#       str_detect(four_region, "Western") ~ "WA",
#       str_detect(four_region, "Southern") ~ "SA"
#     ),
#     four_region = paste0(str_sub(four_region, 0, 4), "aa"),
#     has_data = "Data"
# 
#   ) %>% arrange(kp)

iso3_sort <- pse_out %>%
  distinct(iso3) %>%
  left_join(region) %>%
  arrange(four_region) %>%
  mutate(iso3_idx = as.numeric(fct_inorder(iso3)))

pse_out <- pse_out %>%
  left_join(iso3_sort) %>%
  mutate(xmin = iso3_idx - 0.48,
         xmax = iso3_idx + 0.48,
         background_col = ifelse(is.na(area_name), 1, 0))

foo <- pse_dat %>%
  left_join(pse_out %>% select(iso3, iso3_idx) %>% distinct()) %>%
  left_join(region) %>%
  mutate(is_national = factor(is_national, labels = c("No", "Yes")))

# pse_dat %>%
#   filter(kp != "TG") %>%
#   ggplot(aes(x=iso3)) +
#     geom_jitter(aes(y=population_proportion), alpha=0.3) +
#     geom_segment(data=pse_out, aes(x = xmin, xend = xmax, y = median, yend = median), size=1) +
#     geom_rect(data=pse_out, aes(xmin = xmin, xmax = xmax, ymin = lower, ymax = upper), alpha=0.3, show.legend = FALSE) +
#     geom_hline(data = data.frame(yintercept = 0.01, kp = "MSM", iso3 = c("AGO", "ZWE")), linetype = 3, aes(yintercept = yintercept), color="red") +
#     facet_wrap(~kp, nrow=3, scales = "free") +
#     scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
#     scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
#     # scale_y_continuous(labels = scales::label_percent()) +
#     scale_y_log10(breaks = scales::log_breaks(), labels = scales::label_percent(accuracy = 0.001)) + 
#     labs(x=element_blank(), y="Population proportion", color = "Informed by:") +
#     moz.utils::standard_theme() +
#     theme(axis.text.x = element_text(size=10))

pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")

pse_maps <- lapply(c("FSW", "MSM", "PWID"), function(x) {
  
  pse_out %>%
    filter(kp == x) %>%
    left_join(geographies) %>%
    ggplot() +
    geom_sf(data = grey, aes(geometry = geometry), fill="darkgrey") +
    geom_sf(aes(geometry = geometry, fill=median)) +
    moz.utils::standard_theme() +
    # viridis::scale_fill_viridis(labels = scales::label_percent()) +
    scale_fill_gradientn(colours = pal, labels = scales::label_percent()) +
    labs(fill = "PSE proportion", title = x) +
    coord_sf(datum = NA) +
    theme(legend.key.width = unit(1.5, "cm"),
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
})

pse_maps <- ggpubr::ggarrange(pse_maps[[1]], pse_maps[[2]], pse_maps[[3]], nrow=1)

png(file="~/Dropbox/oli backup/Key populations/Data consolidation paper/Figs/pse_maps.png", width=1800, height=800)
pse_maps
dev.off()


pse_region_data_plot <- pse_out %>%
  ggplot() +
    geom_segment(aes(x = xmin, xend = xmax, y = median, yend = median, color=has_data), size=1) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = lower, ymax = upper, fill=has_data), alpha=0.3, show.legend = FALSE) +
    geom_jitter(data = foo %>% filter(kp != "TG"), aes(x=iso3_idx, y=population_proportion, shape=is_national, size = is_national, alpha=is_national), width = 0.4) +
    geom_hline(data = data.frame(yintercept = 0.01, kp = "MSM", iso3 = c("AGO", "ZWE")), linetype = 3, aes(yintercept = yintercept), color="red") +
    scale_color_manual(values = c(wesanderson::wes_palette("Zissou1")[c(4,1)])) +
    scale_fill_manual(values = wesanderson::wes_palette("Zissou1")[c(4,1)])  +
    scale_alpha_manual(values = c(0.3, 0.5), guide = "none") +
    scale_size_manual(values = c(1.5,2), guide = "none") +
    scale_x_continuous(breaks = 1:38, labels = iso3_sort$iso3) +
    # scale_x_discrete(drop=FALSE, labels = pse_out$iso3) +
    # facet_wrap(~kp, nrow=3, scales="free") +
    facet_grid(kp ~ four_region, scales="free", space = "free") +
    scale_y_log10(breaks = scales::log_breaks(), labels = scales::label_percent(accuracy = 0.001)) + 
    labs(x=element_blank(), y="Population proportion", color = "Informed by:", shape = "Data reported\nas national") +
    moz.utils::standard_theme() +
    theme(axis.text.x = element_text(size=10),
          panel.background = element_rect(fill=NA, color="black"))

png(file="~/Dropbox/oli backup/Key populations/Data consolidation paper/Figs/pse_region_data.png", width=1200, height=600)
pse_region_data_plot
dev.off()

############# Prevalence ############

###### Subnational/national prev from data ########

prev_raw <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/prev.csv", na="NA")
prev_res <- read_csv("R/Model/prev_out.csv")
prev_df <- read_csv("R/Model/prev_surveillance_data.csv")

prev_raw <- prev_raw %>%
  mutate(
    iso3 = countrycode(country.name, "country.name", "iso3c"),
    is_national = ifelse(country.name == area_name, 1, 0),
    has_age = ifelse(!is.na(age_group), 1, 0)) %>%
  filter(iso3 %in% c(iso3_vec, "ZAF"), !is.na(area_name)) %>%
  distinct(kp, area_name, year, prev, .keep_all=TRUE) %>%
  left_join(region)

prev_raw %>% filter(is.na(is_national))

prev_raw %>%
  filter(str_detect(kp, "TG")) %>%
  distinct(region, iso3) %>%
  group_by(region) %>%
  mutate(n=n())

prev_raw %>%
  group_by(kp,region) %>%
  count(is_national) %>%
  mutate(prop = 1-sum(n[is_national])/sum(n))

prev_raw %>%
  filter(kp == "TG") %>%
  group_by(kp, iso3) %>%
  count()

###

convert_logis_labels <- function(x) {
  paste0(round(plogis(x)*100), "%")
}

p1 <- prev_res %>%
  ggplot(aes(x=logit_gen_prev, y=logit_fit)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin = logit_lower, ymax=logit_upper), alpha=0.3) +
  geom_point(data = prev_df %>% filter(kp %in% c("MSM", "PWID", "FSW")), aes(y=logit_kp_prev), alpha = 0.3) +
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  scale_y_continuous(labels = convert_logis_labels) +
  scale_x_continuous(labels = convert_logis_labels) +
  moz.utils::standard_theme() +
  labs(y = "Logit KP HIV prevalence", x = "Logit general population HIV prevalence")+
  theme(panel.border = element_rect(fill=NA, color="black"),
        legend.position = "none") +
  facet_wrap(~kp, ncol=1)

p2 <- prev_res %>%
  ggplot(aes(x=provincial_value, y=fit)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.3) +
  geom_point(data = prev_df %>% filter(kp %in% c("MSM", "PWID", "FSW")), aes(y=value), alpha = 0.3) +
  geom_abline(aes(intercept = 0, slope=1), linetype = 3) +
  moz.utils::standard_theme() +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0,0.5)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,1)) +
  labs(y = "KP HIV prevalence", x = "General population HIV prevalence")+
  theme(panel.border = element_rect(fill=NA, color="black"),
        legend.position = "none") +
  facet_wrap(~kp, ncol=1)

ggpubr::ggarrange(p1, p2, ncol=2)