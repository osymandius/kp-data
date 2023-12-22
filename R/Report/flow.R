library(tidyverse)
library(DiagrammeR)
library(countrycode)
library(DiagrammeRsvg)
library(moz.utils)

ssa_iso3 <- moz.utils::ssa_iso3()

collapse <- function(...) {paste0(..., collapse = "\n")}

setwd(rprojroot::find_rstudio_root_file())

### PSE

pse_remove_label <- list()

pse_total_dat_original <- read_csv("~/Documents/GitHub/kp-data-private/data/complete_dat.csv", show_col_types = F) %>%
  filter(indicator == "pse", iso3 %in% ssa_iso3()) %>%
  mutate(kp = ifelse(kp == "TG", "TGW", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TGW")) %>%
  separate(dataset_id, sep = "_", into=c("dataset", NA), remove = F) %>%
  mutate(dataset = ifelse(dataset == "KP", "KP_Atlas", dataset),
         dataset = ifelse(is.na(dataset), "Surveillance review", dataset)) %>%
  filter(!dataset %in% c("Goals_Nat", "Goals", "Optima")) 

pse_inputs <- pse_total_dat_original %>%
  count(dataset, kp) 

# pse_simple_deduplication <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/2021_11_28_pse_distinct.csv", show_col_types = FALSE)

# write_csv(pse_spreadsheet_extract, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_spreadsheet_extract.csv")
# 
# pse_remove_label$duplicates <- nrow(pse_total_dat) - nrow(pse_simple_deduplication) + pse_spreadsheet_extract %>%
#                                                           filter(!is.na(duplicate_of)) %>%
#                                                           nrow()
# 
pse_remove_label$duplicates <- ""

# pse_surveillance_review_input <- pse_spreadsheet_extract %>%
#   mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c"),
#          kp = ifelse(kp == "TGW", "TG", kp)) %>%
#   filter(is.na(duplicate_of),
#          !is.na(iso3),
#          is.na(uid)) %>%
#   count(kp) %>%
#   mutate(dataset = "Surveillance review")

# pse_inputs <- pse_inputs %>%
#   bind_rows(pse_surveillance_review_input)

pse_inputs <- pse_inputs %>%
  bind_rows(pse_inputs %>%
              group_by(kp) %>%
              summarise(n = sum(n)) %>%
              mutate(dataset = "Total")) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TGW"))

pse_inputs_total <- pse_inputs %>%
  # filter(dataset != "Total") %>%
  group_by(dataset) %>%
  summarise(n = sum(n)) %>%
  mutate(n = paste0("(n = ", n, ")"))

pse_inputs_text <- crossing(dataset = pse_inputs$dataset,
                        kp = c("FSW", "MSM", "PWID", "TGW")) %>%
  left_join(pse_inputs) %>%
  mutate(n = replace_na(n, 0),
         n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(-dataset, ~paste0(cur_column(), " ", .x))) %>%
  left_join(pse_inputs_total) %>%
  select(dataset, n, everything()) %>%
  mutate(dataset = factor(dataset,
                          levels = c("GAM", "KP_Atlas", "GF", "CDC", "Surveillance review",  "Total"),
                          labels = c("UNAIDS GAM", "UNAIDS KP Atlas", "Global Fund", "CDC", "Surveillance review", "Total"))) %>%
  arrange(dataset)

pse_inputs_labels <- apply(pse_inputs_text, 1, collapse)

### Clean data

# Remove duplicates
pse_total_dat <- pse_total_dat_original %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c"),
         data_checked = tolower(data_checked),
         source_found = tolower(source_found)) %>%
  filter(is.na(duplicate_of),
         is.na(is_aggregate),
         !is.na(iso3))

#Remove extrapolated values
modelled_nrow_pse <- pse_total_dat %>%
  filter(simple_method == "Modelled") %>%
  nrow()

pse_remove_label$model <- paste0("Extrapolated/modelled data (n = ", modelled_nrow_pse, ")")

pse_total_dat <- pse_total_dat %>%
  filter(simple_method != "Modelled")

#Remove non-empirical methods
non_emp_nrow_pse <- pse_total_dat %>%
  filter(simple_method == "Non-empirical") %>%
  nrow()

pse_remove_label$non_emp <- paste0("Non-empirical methods (n = ", non_emp_nrow_pse, ")")

pse_total_dat <- pse_total_dat %>%
  filter(simple_method != "Non-empirical")

#Remove non-specific area names
nonspec_nrow_pse <- pse_total_dat %>%
  filter(area_name %in% c("Rural area", "Urban area", "Eight States", "25 districts", "34 counties in Kenya")) %>%
  nrow()

pse_remove_label$nonspec <- paste0("Unspecific area name (n = ", nonspec_nrow_pse, ")")

pse_total_dat <- pse_total_dat %>%
  filter(!area_name %in% c("Rural area", "Urban area", "Eight States", "25 districts", "34 counties in Kenya"))


# #Remove outliers
# pse_remove_label$outlier <- NA
# 
# #Remove unconfirmed data
# pse_remove_label$unconf <- NA

# pse_total_dat %>%
#   filter(!data_checked %in% c("yes", "remove"),
#          (is.na(source_found) | source_found == "no"),
#          year > 2009) %>%
#   count(iso3, kp, year, ref, data_checked) %>%
#   arrange(desc(n)) %>%
#   View()

pse_total_dat <- pse_total_dat %>%
  ungroup() %>%
  mutate(idx = row_number())

truly_checked <- pse_total_dat %>%
  filter(data_checked == "yes",
         year > 2009,
         # source_found != "private"
         # is.na(ethic)
  ) %>%
  mutate(data_checked = "Yes")

mapped_place <- pse_total_dat %>%
  filter(str_detect(method, "apping|PLACE"),
         data_checked == "remove",
         source_found %in% c("yes"),
         is.na(duplicate_of),
         !country.name == area_name,
         year > 2009
         # is.na(ethic)
         ) %>%
  mutate(data_checked = "Yes")

pse_remove_label$unsourced <- paste0("Unsourced (n = ", nrow(pse_total_dat) - nrow(truly_checked) - nrow(mapped_place), ")")
  
duplicate_nrow <- nrow(pse_total_dat_original) -# Total data
  (nonspec_nrow_pse + # non specific area name
     modelled_nrow_pse + # modelled
     non_emp_nrow_pse + # non empirical
     nrow(pse_total_dat) - nrow(truly_checked) - nrow(mapped_place) + # Unsourced
     nrow(bind_rows(truly_checked, mapped_place))) # Cleaned PSE data (the remiander)

pse_remove_label$duplicates <- paste0("Duplicated data (n = ", duplicate_nrow, ")")

pse_cleaned_data <- bind_rows(truly_checked, mapped_place) %>%
  mutate(kp = ifelse(kp == "TG", "TGW", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TGW"))

pse_cleaned_data <- pse_cleaned_data %>%
  mutate(area_name = str_replace(area_name, " and", ","))

setwd(rprojroot::find_rstudio_root_file())

write_csv(pse_cleaned_data %>% select(-c(indicator, ethic)), "src/aaa_assign_populations/pse_cleaned_sourced_data.csv")

pse_cleaned_text <- pse_cleaned_data %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(pse_cleaned_data), ")"),
         name = "Cleaned PSE data") %>%
  select(name, n, everything())

pse_clean_labels <- collapse(pse_cleaned_text)

pse_id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_assign_populations", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022)'), draft = FALSE)
})

# pse_id <- c(pse_id[!is.na(pse_id)], orderly::orderly_search(name = "aaa_assign_populations", query = paste0('latest(parameter:iso3 == "COD")'), draft = FALSE))

# pse_id <- id$id[id$success != "FALSE"]

setwd(rprojroot::find_rstudio_root_file())

pse_final <- lapply(paste0("archive/aaa_assign_populations/", pse_id, "/pse_prevalence.csv"),
       read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  mutate(iso3 = countrycode(country.name, "country.name", "iso3c"))
  
filter(pse_final, is.na(prop_estimate)) %>% View

pse_remove_label$denom <- nrow(pse_final %>% filter(is.na(prop_estimate)))
pse_remove_label$denom <- paste0("No area match (n = ", pse_remove_label$denom, ")")

# pse_final <- filter(pse_final, !is.na(prop_estimate))

pse_final_text <- pse_final %>%
  filter(!is.na(prop_estimate)) %>%
  mutate(kp = ifelse(kp == "TG", "TGW", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TGW")) %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(pse_final %>% filter(!is.na(prop_estimate))), ")"),
         name = "Final PSE data") %>%
  select(name, n, everything())


pse_final_labels <- collapse(pse_final_text)

saveRDS(pse_final_text, "R/Report/R objects for report/PSE/pse_final_count_text.rds")
write_csv(pse_final, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final_sourced.csv")
saveRDS(pse_inputs_text, "R/Report/R objects for report/PSE/pse_input_count_text.rds")
# 
pal <- wesanderson::wes_palette("Zissou1", 10, "continuous")

pse_total_dat <- read_csv("~/Documents/GitHub/kp-data-private/data/complete_dat.csv", show_col_types = F) %>%
  filter(indicator == "pse")
pse_final <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final_sourced.csv", show_col_types = F)
pse_cleaned_data <- read_csv("src/aaa_assign_populations/pse_cleaned_sourced_data.csv", show_col_types = F)

method_counts <- pse_total_dat %>%
  mutate(kp = ifelse(kp == "TG", "TGW", kp)) %>%
  filter(year > 2009, source_found == "yes") %>%
  mutate(
    # year = ifelse(year == 2022, 2021, year),
    year = plyr::round_any(year, 3, floor),
         year = paste0(year, "-", year+2),
         # year = ifelse(year == "2019-2021", "2019-2022", year)
  ) %>%
  distinct(year, kp, study_idx) %>%
  count(year, kp)
  

fig2 <- pse_total_dat %>%
  filter(year > 2009, source_found == "yes") %>%
  mutate(
    # year = ifelse(year == 2022, 2021, year),
    year = plyr::round_any(year, 3, floor),
         year = paste0(year, "-", year+2),
         simple_method = ifelse(str_detect(simple_method, "multiplier|Multiplier"), "Multiplier", simple_method),
         simple_method = ifelse(str_detect(simple_method, "Modelled"), "Modelled/extrapolated", simple_method),
         kp = ifelse(kp == "TG", "TGW", kp)
         # year = ifelse(year == "2019-2021", "2019-2022", year)
  ) %>%
  filter(!is.na(simple_method),
         kp != "TGM") %>%
  distinct(kp, simple_method, year, study_idx) %>%
  group_by(kp, simple_method, year) %>%
  count() %>%
  name_kp(F) %>%
  # ungroup() %>%
  # distinct(simple_method)
  # mutate(simple_method = ifelse(method %in% c("2S-CRC", "3S-CRC", "SS-PSE", "Object/event multiplier", "Service multiplier"), 1, 0)) %>%
  # group_by(year) %>%
  # count(simple_method)
  mutate(simple_method = factor(simple_method,
                         levels= c("3S-CRC", "SS-PSE", "2S-CRC", "Multiplier", "Multiple methods - empirical", "NSUM", "PLACE/mapping", "Multiple methods - mixture", "Modelled/extrapolated", "Non-empirical"))) %>%
  ggplot(aes(x=year, y=n)) +
  geom_col(aes(fill=simple_method), position = "fill") +
  scale_fill_manual(values = pal)+
  scale_y_continuous(labels = scales::label_percent()) +
  facet_wrap(~kp, nrow=1) +
  moz.utils::standard_theme() +
  labs(x=element_blank(), y=element_blank(), fill=element_blank()) +
  coord_cartesian(clip = "off", ylim = c(0,1)) +
  geom_text(data = method_counts %>% filter(kp != "TGM") %>% name_kp(F), aes(y=1.1, label = n), fontface = "bold", size = 4) +
  geom_text(data = data.frame(kp = "Female sex workers"), aes(label = "Number of\nstudies", x=-Inf, y=1.1), hjust=0.8, fontface= "bold", inherit.aes = F) +
  theme(strip.text = element_text(face = "bold", size=16),
        strip.text.x = element_text(margin = margin(b=40)),
        axis.text.x = element_text(angle = 20, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        panel.grid = element_blank())

png("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Figs/Fig 2 PSE methods over time.png", width = 1200, height = 500)
fig2
dev.off()


pse_flow <- grViz("
digraph a_nice_graph {
  
    subgraph cluster_0 {
    # style=filled;
    # color=lightgrey;
  node [fontname = Helvetica, fontcolor = darkslategray,shape = rectangle, color = darkslategray, fontsize = 16]
    inp_gam [label = '@@1-1']
    inp_atlas [label = '@@1-2'];
    inp_gf [label = '@@1-3'];
    inp_cdc [label = '@@1-4'];
    inp_surv [label = '@@1-5'];
    
  node [shape=none, width=0, height=0, label='']
    p0
    
    edge [dir = none, style=invis]
      p0 -> inp_cdc;
      {rank = same; inp_gam -> p0 -> inp_atlas};
      {rank = same; inp_gf -> inp_cdc -> inp_surv};
  
    label = 'Data sources';
    }
    
  node [fontname = Helvetica, fontcolor = darkslategray,shape = rectangle, color = darkslategray, fontsize = 16]
  inp_total [label = '@@1-6']
  m1_dedup [label = '@@2-1']
  m2_nonspec [label = '@@2-2']
  m3_model [label = '@@2-3']
  m4_non_emp [label = '@@2-4']
  m5_unsourced [label = '@@2-5']
  clean [label = '@@3']
  c1_denom [label = '@@2-6']
  final [label = '@@4']
    
  node [shape=none, width=0, height=0, label='']
  p5 -> clean
  p7 -> final
  {rank=same; p1 -> m1_dedup}
  {rank=same; p2 -> m2_nonspec}
  {rank=same; p3 -> m3_model}
  {rank=same; p4 -> m4_non_emp}
  {rank=same; p5 -> m5_unsourced}
  {rank=same; p7 -> c1_denom}
  
  inp_cdc -> inp_total;

  edge [dir = none]
    inp_total -> p1;
    p1 -> p2;
    p2 -> p3;
    p3 -> p4;
    p4 -> p5;
    clean -> p7
    
}

[1]: pse_inputs_labels
[2]: unlist(pse_remove_label)
[3]: pse_clean_labels
[4]: pse_final_labels
")



library(DiagrammeRsvg)
library(rsvg)
pse_flow %>%
  export_svg %>% 
  charToRaw %>% 
  rsvg::rsvg_png("R/Report/R objects for report/PSE/pse_flowchart.png", )

### Prevalence

# prev_spreadsheet_extract <- lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/HIV prevalence/Edited/", full.names = TRUE, pattern = "csv"), read_csv, na= "", show_col_types = FALSE)
# 
# prev_spreadsheet_extract <- c(prev_spreadsheet_extract, 
#               lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/HIV prevalence/Edited/", full.names = TRUE, pattern = "xlsx"), readxl::read_xlsx)
# ) %>%
#   lapply(type_convert) %>%
#   bind_rows()

library(tidyverse)
library(DiagrammeR)
library(countrycode)
library(DiagrammeRsvg)

ssa_iso3 <- moz.utils::ssa_iso3()

collapse <- function(...) {paste0(..., collapse = "\n")}
setwd(rprojroot::find_rstudio_root_file())

prev_total_dat_original <- read_csv("~/Documents/GitHub/kp-data-private/data/complete_dat.csv", show_col_types = F) %>%
  filter(indicator == "prevalence")

prev_total_dat_original <-  prev_total_dat_original %>%
  mutate(prop_estimate = ifelse(prop_estimate > 1, prop_estimate/100, prop_estimate),
         prop_upper = ifelse(prop_upper > 1, prop_upper/100, prop_upper),
         prop_lower = ifelse(prop_lower > 1, prop_lower/100, prop_lower),
         age_group = case_when(
           age_group %in% c("15-24") ~ "Y015_024",
           age_group %in% c("18-24", "19-24", "20-24") ~ "Y020_024",
           age_group %in% c("15+", "18+") ~ "Y015_999",
           age_group == "25-29" ~ "Y025_029",
           age_group %in% c("15-17", "15-19") ~ "Y015_019",
           age_group == "25-34" ~ "Y025_034",
           age_group == "15-49"   ~ "Y015_049",
           age_group == "30-34"   ~ "Y030_034",
           age_group == "35-39"   ~ "Y035_039",
           age_group %in% c("25-49", "25+")   ~ "Y025_049",
           age_group %in% c("45+", "50+","40+") ~ "Y050_999",
           str_detect(age_group, "\\+|\\-") ~ "Y015_049",
           TRUE ~ age_group
           
         ),
         data_checked = tolower(data_checked),
         source_found = tolower(source_found)
  ) %>%
  separate(dataset_id, sep = "_", into=c("dataset", NA), remove = F) %>%
  mutate(dataset = ifelse(dataset == "KP", "KP_Atlas", dataset),
         dataset = ifelse(is.na(dataset), "Surveillance review", dataset)) %>%
  filter(!dataset %in% c("Goals_Nat", "Goals", "Optima input"),
         year > 2009,
         kp != "SW",
         kp != "TGM") %>%
  mutate(kp = ifelse(str_detect(kp, "TG"), "TGW", kp))

prev_remove_label <- list()

prev_inputs <- prev_total_dat_original %>%
  count(dataset, kp) 

prev_remove_label$duplicates <- ""

# prev_surveillance_review_input <- prev_total_dat %>%
#   mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c"),
#          kp = ifelse(kp %in% c("TGW", "TGM", "TGW/GQ"), "TG", kp)) %>%
#   filter(is.na(duplicate_of),
#          !is.na(iso3),
#          is.na(uid)) %>%
#   count(kp) %>%
#   mutate(dataset = "Surveillance review")
# 
# prev_inputs <- prev_inputs %>%
#   bind_rows(prev_surveillance_review_input)

prev_inputs <- prev_inputs %>%
  bind_rows(prev_inputs %>%
              group_by(kp) %>%
              summarise(n = sum(n)) %>%
              mutate(dataset = "Total")) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TGW"),
         !dataset %in% c("Goals_Nat", "Optima input"))

prev_inputs_total <- prev_inputs %>%
  group_by(dataset) %>%
  summarise(n = sum(n)) %>%
  mutate(n = paste0("(n = ", n, ")"))

prev_inputs_text <- crossing(dataset = prev_inputs$dataset,
                            kp = c("FSW", "MSM", "PWID", "TGW")) %>%
  left_join(prev_inputs) %>%
  mutate(n = replace_na(n, 0),
         n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(-dataset, ~paste0(cur_column(), " ", .x))) %>%
  left_join(prev_inputs_total) %>%
  select(dataset, n, everything()) %>%
  mutate(dataset = factor(dataset,
                          levels = c("GAM", "KP Atlas", "GF", "CDC", "Surveillance review",  "Total"),
                          labels = c("UNAIDS GAM", "UNAIDS KP Atlas", "Global Fund", "CDC", "Surveillance review", "Total"))) %>%
  arrange(dataset)

prev_inputs_labels <- apply(prev_inputs_text, 1, collapse)

## Clean prevalence data

prev_total_dat <- prev_total_dat_original %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c")) %>%
  filter(is.na(duplicate_of),
         is.na(is_aggregate),
         !is.na(iso3))

# Remove outliers
# prev_remove_label$unconf <- NA

# prev_total_dat %>%
#   filter(!data_checked %in% c("yes", "remove"),
#          !(data_checked == "no" & iso3 == "ZWE"),
#          (is.na(source_found) | source_found == "no"),
#          year > 2009) %>%
#   count(iso3, kp, year) %>%
#   arrange(desc(n)) %>% 
#   left_join(
#     prev_total_dat %>%
#       filter(!data_checked %in% c("yes", "remove"),
#              !(data_checked == "no" & iso3 == "ZWE"),
#              (is.na(source_found) | source_found == "no"),
#              year > 2009) %>%
#       distinct(iso3, kp, year, ref) %>%
#       arrange(iso3, kp, year) %>%
#       filter(iso3 != "ZWE", !is.na(ref))
#   ) %>%
#   mutate(country = countrycode(iso3, "iso3c", "country.name")) %>%
#   write_csv("~/Downloads/missing_prev.csv", na = "")
# 
# prev_total_dat %>%
#   filter(!data_checked %in% c("yes", "remove"),
#          !(data_checked == "no" & iso3 == "ZWE"),
#          (is.na(source_found) | source_found == "no"),
#          year > 2009) %>%
#   count(iso3, year, ref) %>%
#   arrange(desc(n)) %>%
#   mutate(country = countrycode(iso3, "iso3c", "country.name")) %>%
#   write_csv("~/Downloads/missing_prev_iso.csv", na = "")
# 
prev_total_dat <- prev_total_dat %>%
  ungroup() %>%
  mutate(idx = row_number())

prev_checked <- prev_total_dat %>%
  filter(data_checked == "yes",
         # source_found != "private",
         year > 2009
  ) %>%
  mutate(data_checked = "Yes")

unsourced_nrow_prev <- nrow(prev_total_dat) - nrow(prev_checked)
prev_remove_label$unsourced <- paste0("Unsourced (n = ", unsourced_nrow_prev , ")")

prev_cleaned_data <- bind_rows(prev_checked) %>%
  mutate(method = case_when(
           method %in% c("Lab") ~ "lab",
           str_detect(method, "elf") ~ "selfreport",
           is.na(method) ~ "selfreport",
           TRUE ~ method
         )) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TGW"))

duplicate_nrow_prev <- nrow(prev_total_dat_original) -# Total data
  (unsourced_nrow_prev + # Unsourced
     nrow(prev_cleaned_data)) # Cleaned prev data (the remiander)

prev_remove_label$duplicates <- paste0("Duplicated data (n = ", duplicate_nrow_prev, ")")

write_csv(prev_cleaned_data %>% ungroup %>% mutate(row_id = row_number()), "src/aaa_assign_province/prev_clean_sourced.csv")

prev_cleaned_text <- prev_cleaned_data %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(prev_cleaned_data), ")"),
         name = "Final HIV prevalence data") %>%
  select(name, n, everything())

prev_clean_labels <- collapse(prev_cleaned_text)

setwd(rprojroot::find_rstudio_root_file())

# subgraph input_cluster {
#   node [fontname = Helvetica, fontcolor = darkslategray, shape = rectangle, color = darkslategray];
#   inp_gam [label = '@@1-1'];
#   inp_atlas [label = '@@1-2'];
#   inp_gf [label = '@@1-3'];
#   inp_cdc [label = '@@1-4'];
#   inp_surv [label = '@@1-5'];
# 



#### ART coverage

# art_spreadsheet_extract <- lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/ART coverage/Edited/", full.names = TRUE, pattern = "csv"), read_csv, na= "", show_col_types = FALSE)
# 
# art_spreadsheet_extract <- c(art_spreadsheet_extract, 
#                               lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/ART coverage/Edited/", full.names = TRUE, pattern = "xlsx"), readxl::read_xlsx)
# ) %>%
#   lapply(type_convert) %>%
#   bind_rows()

art_total_dat_original <- read_csv("~/Documents/GitHub/kp-data-private/data/complete_dat.csv") %>%
  filter(indicator == "art")

art_total_dat_original <-  art_total_dat_original %>%
  mutate(prop_estimate = ifelse(prop_estimate > 1, prop_estimate/100, prop_estimate),
         prop_upper = ifelse(prop_upper > 1, prop_upper/100, prop_upper),
         prop_lower = ifelse(prop_lower > 1, prop_lower/100, prop_lower),
         age_group = case_when(
           age_group %in% c("15-24") ~ "Y015_024",
           age_group %in% c("18-24", "19-24", "20-24") ~ "Y020_024",
           age_group %in% c("15+", "18+") ~ "Y015_999",
           age_group == "25-29" ~ "Y025_029",
           age_group %in% c("15-17", "15-19") ~ "Y015_019",
           age_group == "25-34" ~ "Y025_034",
           age_group == "15-49"   ~ "Y015_049",
           age_group == "30-34"   ~ "Y030_034",
           age_group == "35-39"   ~ "Y035_039",
           age_group %in% c("25-49", "25+")   ~ "Y025_049",
           age_group %in% c("45+", "50+","40+") ~ "Y050_999",
           str_detect(age_group, "\\+|\\-") ~ "Y015_049",
           TRUE ~ age_group
         ),
         data_checked = tolower(data_checked),
         source_found = tolower(source_found)
  ) %>%
  separate(dataset_id, sep = "_", into=c("dataset", NA), remove = F) %>%
  mutate(dataset = ifelse(dataset == "KP", "KP_Atlas", dataset),
         dataset = ifelse(is.na(dataset), "Surveillance review", dataset)) %>%
  filter(!dataset %in% c("Goals_Nat", "Goals", "Optima input"),
         year > 2009,
         kp != "SW",
         kp != "TGM") %>%
  mutate(kp = ifelse(str_detect(kp, "TG"), "TGW", kp))

art_remove_label <- list()

art_inputs <- art_total_dat_original %>%
  count(dataset, kp) 

art_remove_label$duplicates <- ""

art_inputs <- art_inputs %>%
  bind_rows(art_inputs %>%
              group_by(kp) %>%
              summarise(n = sum(n)) %>%
              mutate(dataset = "Total")) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TGW"),
         !dataset %in% c("Goals_Nat", "Optima input"))

art_inputs_total <- art_inputs %>%
  group_by(dataset) %>%
  summarise(n = sum(n)) %>%
  mutate(n = paste0("(n = ", n, ")"))

art_inputs_text <- crossing(dataset = art_inputs$dataset,
                             kp = c("FSW", "MSM", "PWID", "TGW")) %>%
  left_join(art_inputs) %>%
  mutate(n = replace_na(n, 0),
         n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(-dataset, ~paste0(cur_column(), " ", .x))) %>%
  left_join(art_inputs_total) %>%
  select(dataset, n, everything()) %>%
  mutate(dataset = factor(dataset,
                          levels = c("KP Atlas", "GF", "Surveillance review",  "Total"),
                          labels = c("UNAIDS KP Atlas", "Global Fund", "Surveillance review", "Total"))) %>%
  arrange(dataset)

art_inputs_labels <- apply(art_inputs_text, 1, collapse)

## Clean artalence data

art_total_dat <- art_total_dat_original %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c")) %>%
  filter(is.na(duplicate_of),
         is.na(is_aggregate),
         !is.na(iso3))


# Remove outliers
# art_remove_label$unconf <- NA

# art_total_dat %>%
#   filter(!data_checked %in% c("yes", "remove"),
#          (is.na(source_found) | source_found == "no"),
#          year > 2009) %>%
#   count(iso3, kp, year, ref) %>%
#   arrange(desc(n)) %>%
#   View()

art_checked <- art_total_dat %>%
  filter(data_checked == "yes",
         # source_found != "private",
         year > 2009
  ) %>%
  mutate(data_checked = "Yes")

unsourced_nrow_art <- nrow(art_total_dat) - nrow(art_checked)
art_remove_label$unsourced <- paste0("Unsourced (n = ", unsourced_nrow_art, ")")

art_selfself <- art_checked %>%
  filter(
    method == "self-self",
    year > 2009
  ) 

art_remove_label$selfself <- paste0("Self-report HIV status (n = ", nrow(art_selfself), ")")

## Remove programme-recruited studies

art_programme <- art_checked %>%
  filter(study_idx %in% c(19, 28, 28, 34, 57, 62, 103, 118, 119, 119, 121, 138, 209, 212, 219, 240, 253, 254, 256, 267))

art_remove_label$programme <- paste0("Clinic-based recruitment (n = ", nrow(art_programme), ")")

art_cleaned_data <- bind_rows(art_checked) %>%
  filter(method != "self-self",
         !study_idx %in% c(19, 28, 28, 34, 57, 62, 103, 118, 119, 119, 121, 138, 209, 212, 219, 240, 253, 254, 256, 267),
         kp %in% c("FSW", "MSM", "PWID", "TGW"))

duplicate_nrow_art <- nrow(art_total_dat_original) -# Total data
  (unsourced_nrow_art + # Unsourced
     nrow(art_cleaned_data)) # Cleaned prev data (the remiander)

art_remove_label$duplicates <- paste0("Duplicated data (n = ", duplicate_nrow_art, ")")

write_csv(art_cleaned_data %>% ungroup %>% mutate(row_id = row_number()), "src/aaa_assign_province/art_clean_sourced.csv")

art_cleaned_text <- art_cleaned_data %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(art_cleaned_data), ")"),
         name = "Final ART coverage data") %>%
  select(name, n, everything())

art_clean_labels <- collapse(art_cleaned_text)

setwd(rprojroot::find_rstudio_root_file())

prev_id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_assign_province", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022)'), draft = FALSE)
})

# prev_id <- id$id

prev_final <- lapply(paste0("archive/aaa_assign_province/", prev_id, "/prev.csv"),
                     function(x) {read_csv(x, show_col_types = FALSE) %>% select(-any_of("...1"))}) %>%
  bind_rows()

prev_final_text <- prev_final %>%
  mutate(kp = ifelse(kp == "TG", "TGW", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TGW")) %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(prev_final), ")"),
         name = "Final HIV prevalence data") %>%
  select(name, n, everything())

prev_remove_label$denom <- nrow(prev_cleaned_data) - nrow(prev_final)
prev_remove_label$denom <- paste0("No area match (n = ", prev_remove_label$denom, ")")

prev_final_labels <- collapse(prev_final_text)

saveRDS(prev_final_text, "R/Report/R objects for report/Prevalence/prev_count_text.rds")
write_csv(prev_final, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_final_sourced.csv")
saveRDS(prev_inputs_text, "R/Report/R objects for report/Prevalence/prev_input_count_text.rds")



art_final <- lapply(paste0("archive/aaa_assign_province/", prev_id[!is.na(prev_id)], "/art.csv"),
                     function(x) {read_csv(x, show_col_types = FALSE) %>% select(-any_of("...1"))}) %>%
  bind_rows()

art_cleaned_data %>% 
  count(study_idx) %>%
  rename(clean = n) %>%
  left_join(art_final %>%
              count(study_idx) %>%
              rename(final = n)
  ) %>%
  filter(clean != final)

art_final_text <- art_final %>%
  mutate(kp = ifelse(kp == "TG", "TGW", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TGW")) %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(art_final), ")"),
         name = "Final ART coverage data") %>%
  select(name, n, everything())

# art_remove_label$denom <- nrow(art_cleaned_data) - nrow(art_final)
# art_remove_label$denom <- paste0("No area match (n = ", art_remove_label$denom, ")")

art_final_labels <- collapse(art_final_text)

# saveRDS(list('art_inputs_labels' = art_inputs_labels,
#              "art_remove_label" = art_remove_label,
#              "art_clean_labels" = art_clean_labels,
#              "art_final_labels" = art_final_labels),
#         "R/Report/R objects for report/Prevalence/art_flow_input.rds")

saveRDS(art_final_text, "R/Report/R objects for report/ART coverage/art_count_text.rds")
write_csv(art_final, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_final.csv")
saveRDS(art_inputs_text, "R/Report/R objects for report/ART coverage/art_input_count_text.rds")

prev_flow <- grViz("
digraph a_nice_graph {
  
    subgraph cluster_0 {
    # style=filled;
    # color=lightgrey;
  node [fontname = Helvetica, fontcolor = darkslategray,shape = rectangle, color = darkslategray]
    inp_gam [label = '@@1-1']
    inp_gf [label = '@@1-2'];
    inp_cdc [label = '@@1-3'];
    inp_surv [label = '@@1-4'];
    
  node [shape=none, width=0, height=0, label='']
    p0
    
    edge [dir = none, style=invis]
      p0 -> inp_surv;
      {rank = same; inp_gf -> inp_atlas};
      {rank = same; inp_cdc -> inp_surv};
  
    label = 'Data sources';
    }
    
  node [fontname = Helvetica, fontcolor = darkslategray,shape = rectangle, color = darkslategray]
  inp_total [label = '@@1-5']
  m1_dedup [label = '@@2-1']
  m3_unsourced [label = '@@2-3']
  clean [label = '@@3']
    
  node [shape=none, width=0, height=0, label='']
  p3 -> clean;
  {rank=same; p1 -> m1_dedup};
  {rank=same; p3 -> m3_unsourced};
  
  inp_surv -> inp_total;

  edge [dir = none]
    inp_total -> p1;
    p1 -> p2;
    p2 -> p3;
    
}

[1]: prev_inputs_labels
[2]: unlist(prev_remove_label)
[3]: prev_clean_labels
[4]: prev_final_labels
")


prev_flow %>%
  export_svg %>% 
  charToRaw %>% 
  rsvg::rsvg_png("R/Report/R objects for report/Prevalence/prev_flowchart.png")

art_flow <- grViz("
digraph a_nice_graph {
  
    subgraph cluster_0 {
    # style=filled;
    # color=lightgrey;
  node [fontname = Helvetica, fontcolor = darkslategray,shape = rectangle, color = darkslategray]
    inp_atlas [label = '@@1-1'];
    inp_gf [label = '@@1-2'];
    inp_surv [label = '@@1-3'];
    
    edge [dir = none, style=invis]
      {rank = same; inp_atlas -> inp_gf -> inp_surv};
  
    label = 'Data sources';
    }
    
  node [fontname = Helvetica, fontcolor = darkslategray,shape = rectangle, color = darkslategray]
  inp_total [label = '@@1-6']
  m1_dedup [label = '@@2-1']
  m2_nonspec [label = '@@2-2']
  m3_unsourced [label = '@@2-3']
  m4_selfself [label = '@@2-4']
  clean [label = '@@3']
    
  node [shape=none, width=0, height=0, label='']
  p4 -> clean;
  {rank=same; p1 -> m1_dedup};
  {rank=same; p2 -> m2_nonspec};
  {rank=same; p3 -> m3_unsourced};
  {rank=same; p4 -> m4_selfself};
  
  inp_gf -> inp_total;

  edge [dir = none]
    inp_total -> p1;
    p1 -> p2;
    p2 -> p3;
    p3 -> p4;
    
}

[1]: art_inputs_labels
[2]: unlist(art_remove_label)
[3]: art_clean_labels
[4]: art_final_labels
")



art_flow %>%
  export_svg %>% 
  charToRaw %>% 
  rsvg::rsvg_png("R/Report/R objects for report/ART coverage/art_flowchart.png")

######