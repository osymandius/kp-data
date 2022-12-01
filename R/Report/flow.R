library(tidyverse)
library(DiagrammeR)
library(countrycode)
library(DiagrammeRsvg)

ssa_names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- countrycode(ssa_names, "country.name", "iso3c")

collapse <- function(...) {paste0(..., collapse = "\n")}

### PSE

pse_remove_label <- list()

pse_total_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/2021_11_28_pse_all_data.csv", show_col_types = FALSE)

pse_inputs <- pse_total_dat %>%
  filter(!dataset %in% c("Goals_Nat", "Optima")) %>%
  count(dataset, kp) 

pse_simple_deduplication <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/2021_11_28_pse_distinct.csv", show_col_types = FALSE)

# pse_spreadsheet_extract <- lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/PSE/Edited/", full.names = TRUE, pattern = "csv"), read_csv, na= "", show_col_types = FALSE)
# 
# pse_spreadsheet_extract <- c(pse_spreadsheet_extract, 
#               lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/PSE/Edited/", full.names = TRUE, pattern = "xlsx"), readxl::read_xlsx)
# ) %>%
#   lapply(type_convert) %>%
#   bind_rows()

pse_spreadsheet_extract <- read_csv("~/Imperial College London/Key population data - WP - General/Combined data/PSE/Edited/combined_pse2.csv")

write_csv(pse_spreadsheet_extract, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_spreadsheet_extract.csv")

pse_remove_label$duplicates <- nrow(pse_total_dat) - nrow(pse_simple_deduplication) + pse_spreadsheet_extract %>%
                                                          filter(!is.na(duplicate_of)) %>%
                                                          nrow()

pse_remove_label$duplicates <- paste0("Duplicated data (n = ", pse_remove_label$duplicates, ")")

pse_surveillance_review_input <- pse_spreadsheet_extract %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c"),
         kp = ifelse(kp == "TGW", "TG", kp)) %>%
  filter(is.na(duplicate_of),
         !is.na(iso3),
         is.na(uid)) %>%
  count(kp) %>%
  mutate(dataset = "Surveillance review")

pse_inputs <- pse_inputs %>%
  bind_rows(pse_surveillance_review_input)

pse_inputs <- pse_inputs %>%
  bind_rows(pse_inputs %>%
              group_by(kp) %>%
              summarise(n = sum(n)) %>%
              mutate(dataset = "Total")) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG"))

pse_inputs_total <- pse_inputs %>%
  group_by(dataset) %>%
  summarise(n = sum(n)) %>%
  mutate(n = paste0("(n = ", n, ")"))

pse_inputs_text <- crossing(dataset = pse_inputs$dataset,
                        kp = c("FSW", "MSM", "PWID", "TG")) %>%
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
pse_spreadsheet_extract <- pse_spreadsheet_extract %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c"),
         data_checked = tolower(data_checked),
         source_found = tolower(source_found)) %>%
  filter(is.na(duplicate_of),
         !is.na(iso3))

#Remove non-specific area names
pse_remove_label$nonspec <- pse_spreadsheet_extract %>%
  filter(str_detect(area_name, regex("urban|rural|county|counties|districts|state|total", ignore_case = TRUE))) %>%
  nrow()

pse_remove_label$nonspec <- paste0("Unspecific area name (n = ", pse_remove_label$nonspec, ")")

pse_spreadsheet_extract <- pse_spreadsheet_extract %>%
  filter(!str_detect(area_name, regex("urban|rural|county|counties|districts|state|total", ignore_case = TRUE)))

#Remove extrapolated values
pse_remove_label$model <- pse_spreadsheet_extract %>%
  filter(str_detect(method, regex("extrapolat|modelled", ignore_case = TRUE))) %>%
  nrow()

pse_remove_label$model <- paste0("Extrapolated/modelled data (n = ", pse_remove_label$model, ")")

pse_spreadsheet_extract <- pse_spreadsheet_extract %>%
  filter(!(str_detect(method, regex("extrapolat|modelled", ignore_case = TRUE)) & data_checked != "yes") | is.na(method))

#Remove non-empirical methods
pse_remove_label$non_emp <- pse_spreadsheet_extract %>%
  filter(str_detect(method, regex("enumeration|delphi|wisdom|WOTC|WODC", ignore_case = TRUE))) %>%
  nrow()

pse_remove_label$non_emp <- paste0("Non-empirical methods (n = ", pse_remove_label$non_emp, ")")

pse_spreadsheet_extract <- pse_spreadsheet_extract %>%
  filter(!str_detect(method, regex("enumeration|delphi|wisdom|WOTC|WODC", ignore_case = TRUE)) | is.na(method))

# #Remove outliers
# pse_remove_label$outlier <- NA
# 
# #Remove unconfirmed data
# pse_remove_label$unconf <- NA

# pse_spreadsheet_extract %>%
#   filter(!data_checked %in% c("yes", "remove"),
#          (is.na(source_found) | source_found == "no"),
#          year > 2009) %>%
#   count(iso3, kp, year, ref, data_checked) %>%
#   arrange(desc(n)) %>%
#   View()

pse_spreadsheet_extract <- pse_spreadsheet_extract %>%
  ungroup() %>%
  mutate(idx = row_number())

truly_checked <- pse_spreadsheet_extract %>%
  filter(data_checked == "yes",
         year > 2009,
         source_found != "private"
  ) %>%
  mutate(data_checked = "Yes")

mapped_place <- pse_spreadsheet_extract %>%
  filter(str_detect(method, "apping|PLACE"),
         data_checked == "remove",
         source_found %in% c("yes"),
         is.na(duplicate_of),
         !country.name == area_name,
         year > 2009) %>%
  mutate(data_checked = "Yes")

unknown <- pse_spreadsheet_extract %>%
  filter(
    (is.na(data_checked) & source_found == "no")  | (is.na(data_checked) & is.na(source_found)),
    year > 2009
  ) %>%
  mutate(data_checked = "No")

pse_remove_label$unsourced <- paste0("Unsourced (n = ", nrow(unknown), ")")
# 
# View(filter(pse_spreadsheet_extract, !idx %in% c(truly_checked$idx, mapped_place$idx, unknown$idx)) %>% select(data_checked, source_found, everything()) %>% filter(year > 2009))

pse_cleaned_data <- bind_rows(truly_checked, mapped_place) %>%
  mutate(kp = ifelse(kp == "TGW", "TG", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG"))

pse_cleaned_data <- pse_cleaned_data %>%
  mutate(area_name = str_replace(area_name, " and", ","))

write_csv(pse_cleaned_data, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_spreadsheet_cleaned_sourced.csv")

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

pse_id <- c(pse_id[!is.na(pse_id)], orderly::orderly_search(name = "aaa_assign_populations", query = paste0('latest(parameter:iso3 == "COD")'), draft = FALSE))

setwd(rprojroot::find_rstudio_root_file())

pse_final <- lapply(paste0("archive/aaa_assign_populations/", pse_id, "/pse_prevalence.csv"),
       read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  filter(is.na(x),
         !is.na(population_proportion),
         method != "Extrapolation from 2S-CRC"
  ) %>%
  mutate(
    iso3 = countrycode(country.name, "country.name", "iso3c"),
    method = case_when(
      method == "" ~ NA_character_,
      method %in% c("Programmatic mapping", "Hotspot mapping", "PLACE", "Enumeration/mapping", "Mapping", "Mapping and enumeration") ~ "PLACE/Mapping",
      method %in% c("Unique object", "Unique object multiplier", "Unique Object Multiploer") ~ "Object multiplier",
      method %in% c("Service Multiplier", "Service multiplier", "Multiplier") ~ "Service multiplier",
      method %in% c("Unique event multiplier", "Unique event", "Event multiplier") ~ "Event multiplier",
      method %in% c("Capture - recapture", "CRC", "2S-CEC") ~ "2S-CRC",
      method %in% c("Wisdom of Crowds, unique object distribution, social event and successive-sampling methods",
                    "Service multiplier, Unique Object , Literature SS-PSE(RDS-A), Unique event & Consensus approach-Modified Delphi",
                    "Mapping, Enumeration and literature review",
                    "Consensus and mapping",
                    "Unique object, WODM, service multiplier, social multiplier, 2S-CRC",
                    "Wisdom of Crowds, unique object multiplier, social events and SS-PSE",
                    "WODC, unique object, service, social, 2S-CRC",
                    "Wisdom of the masses and capture-recapture",
                    "Wisdom of the masses and social multiplier",
                    "Triangulation of the follow methods unique object multiplier, wisdom of the masses,  capture â€“ recapture  and multiplier",
                    "Unique Object multiplier et wisdom of the masses, NSUM and capture-recapture",
                    "Unique object multiplier, Census, Respondent driven sampling survey and service multiplier method",
                    "Unique object multiplier, social event, wisdom of the masses and NSUM",
                    "WODC, unique object, service, social",
                    "Literature review (meta-analysis model for surveys) and Delphi method",
                    "Literature review (meta-analysis) and Delphi method",
                    "Literature review and individual interviews",
                    "Delphi method and consensus",
                    "Consensus of % adult population and population growth",
                    "Consensus (service, literature, unique object, mapping, WOTC)",
                    "Median of unique object, WOTC",
                    "Median of unique object, WOTC, 2S-CRC",
                    "Programme data",
                    "RDS",
                    "Snowball",
                    "Triangulation",
                    "Social event, wisdom of the crowds, capture-recapture, unique object multiplier and service multiplier",
                    "Wisdom of the masses, unique object multiplier and social event"
                    
      ) ~ "Multiple methods - mixture",
      method %in% c("Service multiplier, unique object multiplier,  literature review, RDSAnalyst  SS-PSE Method",
                    "Unique object multiplier, service multiplier, event multiplier",
                    "Bayesian synthesis - multiplier and SS-PSE",
                    "Unique object and special event multiplier",
                    "Unique object, social, service, 2S-CRC",
                    "Capture-recapture, unique object multiplier and register multiplier",
                    "Capture-recapture and NSUM",
                    "Multiplier, capture-recapture and social event",
                    "Unique object, event and service multipliers, SS-PSE, and a synthesis of the methods using the Anchored Multiplier",
                    "Unique object, service multiplier and NSUM",
                    "Unique object, social, service",
                    "Consensus (SS-PSE, unique object, multiplier, 1% recommendation)",
                    "Consensus (SS-PSE, unique object, multiplier)",
                    "Social event, unique object multiplier and service multiplier",
                    "Bayesian synthesis (SS-PSE, literature, something else)"
                    
                    
      ) ~ "Multiple methods - empirical",
      TRUE ~ method
    ),
    simple_method = case_when(
      method %in% c("2S-CRC", "3S-CRC", "Multiple methods - empirical", "Object/event multiplier", "Service multiplier", "SS-PSE") ~ "empirical",
      method %in% c("Multiple methods - mixture", "PLACE/Mapping") ~ "nonempirical",
      TRUE ~ method
    ))

# bad_match <- lapply(paste0("archive/aaa_assign_populations/", pse_id[!is.na(pse_id)], "/bad_match_error.csv"),
#        read_csv) %>%
#   bind_rows() %>%
#   distinct(iso3, given_name)

pse_final_text <- pse_final %>%
  mutate(kp = ifelse(kp == "TGW", "TG", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG")) %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(pse_final), ")"),
         name = "Final PSE data") %>%
  select(name, n, everything())

pse_remove_label$denom <- nrow(pse_cleaned_data) - nrow(pse_final)
pse_remove_label$denom <- paste0("No area match (n = ", pse_remove_label$denom, ")")

pse_final_labels <- collapse(pse_final_text)

saveRDS(pse_final_text, "R/Report/R objects for report/PSE/pse_final_count_text.rds")
write_csv(pse_final, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final_sourced.csv")
saveRDS(pse_inputs_text, "R/Report/R objects for report/PSE/pse_input_count_text.rds")
# 
pal <- wesanderson::wes_palette("Zissou1", 10, "continuous")

pse_final <- read.csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_final_sourced.csv")
pse_cleaned_data <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/pse_spreadsheet_cleaned_sourced.csv")

method_counts <- pse_spreadsheet_extract %>%
  filter(str_detect(method, regex("enumeration|delphi|wisdom|WOTC|WODC", ignore_case = TRUE))) %>%
  filter(source_found == "yes") %>%
  mutate(method = "Non-empirical") %>%
  bind_rows(pse_final) %>%
  mutate(year = plyr::round_any(year, 3, floor),
         year = paste0(year, "-", year+2),
         # year = ifelse(year == "2019-2021", "2019-2022", year)
  ) %>%
  filter(year != "2007-2009") %>%
  distinct(year, ref, kp) %>%
  count(year, kp)

fig2 <- pse_spreadsheet_extract %>%
  filter(str_detect(method, regex("enumeration|delphi|wisdom|WOTC|WODC", ignore_case = TRUE))) %>%
  filter(source_found == "yes") %>%
  mutate(method = "Non-empirical") %>%
  bind_rows(pse_final) %>%
  filter(year > 2009) %>%
  mutate(year = plyr::round_any(year, 3, floor),
         year = paste0(year, "-", year+2),
         # year = ifelse(year == "2019-2021", "2019-2020", year)
         ) %>%
  filter(year != "2007-2009") %>%
  distinct(kp, method, year, ref) %>%
  group_by(kp, method, year) %>%
  count() %>%
  # mutate(simple_method = ifelse(method %in% c("2S-CRC", "3S-CRC", "SS-PSE", "Object/event multiplier", "Service multiplier"), 1, 0)) %>%
  # group_by(year) %>%
  # count(simple_method)
  mutate(method = factor(method,
                         levels= c("3S-CRC", "SS-PSE", "2S-CRC", "Object multiplier", "Event multiplier", "Multiple methods - empirical", "Service multiplier", "PLACE/Mapping", "Multiple methods - mixture", "Non-empirical"))) %>%
  ggplot(aes(x=year, y=n)) +
  geom_col(aes(fill=method), position = "fill") +
  scale_fill_manual(values = pal)+
  scale_y_continuous(labels = scales::label_percent()) +
  facet_wrap(~kp, nrow=1) +
  moz.utils::standard_theme() +
  labs(x=element_blank(), y=element_blank(), fill=element_blank()) +
  coord_cartesian(clip = "off", ylim = c(0,1)) +
  geom_text(data = method_counts, aes(y=1.1, label = n)) +
  theme(strip.text = element_text(face = "bold", size=13),
        strip.text.x = element_text(margin = margin(b=20)),
        axis.text.x = element_text(angle = 20, hjust = 1))

png("~/OneDrive - Imperial College London/Phd/KP data consolidation/Consolidation paper/Figs/Fig 5 prevalence.png", width = 1100, height = 800)
fig2
dev.off()


pse_flow <- grViz("
digraph a_nice_graph {
  
    subgraph cluster_0 {
    # style=filled;
    # color=lightgrey;
  node [fontname = Helvetica, fontcolor = darkslategray,shape = rectangle, color = darkslategray]
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
    
  node [fontname = Helvetica, fontcolor = darkslategray,shape = rectangle, color = darkslategray]
  inp_total [label = '@@1-6']
  m1_dedup [label = '@@2-1']
  m2_nonspec [label = '@@2-2']
  m3_model [label = '@@2-3']
  m4_non_emp [label = '@@2-4']
  m5_unsourced [label = '@@2-5']
  clean [label = '@@3']
  c1_denom [label = '@@2-7']
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
  rsvg::rsvg_png("R/Report/R objects for report/PSE/pse_flowchart.png")

### Prevalence

# prev_spreadsheet_extract <- lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/HIV prevalence/Edited/", full.names = TRUE, pattern = "csv"), read_csv, na= "", show_col_types = FALSE)
# 
# prev_spreadsheet_extract <- c(prev_spreadsheet_extract, 
#               lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/HIV prevalence/Edited/", full.names = TRUE, pattern = "xlsx"), readxl::read_xlsx)
# ) %>%
#   lapply(type_convert) %>%
#   bind_rows()

prev_spreadsheet_extract <- read_csv("~/Imperial College London/Key population data - WP - General/Combined data/HIV prevalence/Edited/combined_prev3.csv")

prev_spreadsheet_extract <-  prev_spreadsheet_extract %>%
  mutate(prev = ifelse(prev > 1, prev/100, prev),
         prev_upper = ifelse(prev_upper > 1, prev_upper/100, prev_upper),
         prev_lower = ifelse(prev_lower > 1, prev_lower/100, prev_lower),
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
  )

write_csv(prev_spreadsheet_extract, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_spreadsheet_extract.csv")

prev_remove_label <- list()

prev_total_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/2021_11_24_prev_all_data.csv")

prev_inputs <- prev_total_dat %>%
  filter(!dataset %in% c("Goals_Nat", "Optima input")) %>%
  count(dataset, kp) 

prev_simple_deduplication <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/2021_11_24_prev_deduplicated.csv")

prev_remove_label$duplicates <- nrow(prev_total_dat) - nrow(prev_simple_deduplication) + prev_spreadsheet_extract %>%
  filter(!is.na(duplicate_of) | !is.na(is_aggregate)) %>%
  nrow()

prev_remove_label$duplicates <- paste0("Duplicated data (n = ", prev_remove_label$duplicates, ")")

prev_surveillance_review_input <- prev_spreadsheet_extract %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c"),
         kp = ifelse(kp %in% c("TGW", "TGM", "TGW/GQ"), "TG", kp)) %>%
  filter(is.na(duplicate_of),
         !is.na(iso3),
         is.na(uid)) %>%
  count(kp) %>%
  mutate(dataset = "Surveillance review")

prev_inputs <- prev_inputs %>%
  bind_rows(prev_surveillance_review_input)

prev_inputs <- prev_inputs %>%
  bind_rows(prev_inputs %>%
              group_by(kp) %>%
              summarise(n = sum(n)) %>%
              mutate(dataset = "Total")) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG"),
         !dataset %in% c("Goals_Nat", "Optima input"))

prev_inputs_total <- prev_inputs %>%
  group_by(dataset) %>%
  summarise(n = sum(n)) %>%
  mutate(n = paste0("(n = ", n, ")"))

prev_inputs_text <- crossing(dataset = prev_inputs$dataset,
                            kp = c("FSW", "MSM", "PWID", "TG")) %>%
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

prev_spreadsheet_extract <- prev_spreadsheet_extract %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c")) %>%
  filter(is.na(duplicate_of),
         is.na(is_aggregate),
         !is.na(iso3))

# Remove non-specific area names
prev_remove_label$nonspec <- prev_spreadsheet_extract %>%
  filter(str_detect(area_name, regex("urban|rural|county|counties|districts|state|total", ignore_case = TRUE))) %>%
  nrow()

prev_remove_label$nonspec <- paste0("Unspecific area name (n = ", prev_remove_label$nonspec, ")")

prev_spreadsheet_extract <- prev_spreadsheet_extract %>%
  filter(!str_detect(area_name, regex("urban|rural|county|counties|districts|state|total", ignore_case = TRUE)))

# Remove outliers
# prev_remove_label$unconf <- NA

# prev_spreadsheet_extract %>%
#   filter(!data_checked %in% c("yes", "remove"),
#          !(data_checked == "no" & iso3 == "ZWE"),
#          (is.na(source_found) | source_found == "no"),
#          year > 2009) %>%
#   count(iso3, kp, year) %>%
#   arrange(desc(n)) %>% 
#   left_join(
#     prev_spreadsheet_extract %>%
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
# prev_spreadsheet_extract %>%
#   filter(!data_checked %in% c("yes", "remove"),
#          !(data_checked == "no" & iso3 == "ZWE"),
#          (is.na(source_found) | source_found == "no"),
#          year > 2009) %>%
#   count(iso3, year, ref) %>%
#   arrange(desc(n)) %>%
#   mutate(country = countrycode(iso3, "iso3c", "country.name")) %>%
#   write_csv("~/Downloads/missing_prev_iso.csv", na = "")
# 
prev_spreadsheet_extract <- prev_spreadsheet_extract %>%
  ungroup() %>%
  mutate(idx = row_number())

prev_checked <- prev_spreadsheet_extract %>%
  filter(data_checked == "yes",
         source_found != "private",
         year > 2009
  ) %>%
  mutate(data_checked = "Yes")

prev_unknown <- prev_spreadsheet_extract %>%
  filter(
    (is.na(data_checked) & source_found == "no")  | (is.na(data_checked) & is.na(source_found)),
    year > 2009
  ) %>%
  mutate(data_checked = "No")

prev_remove_label$unsourced <- paste0("Unsourced (n = ", nrow(prev_unknown), ")")

prev_cleaned_data <- bind_rows(prev_checked) %>%
  mutate(kp = ifelse(kp == "TGW", "TG", kp),
         method = case_when(
           method %in% c("Lab") ~ "lab",
           method %in%c("Self report", "self-report", "Self-report") ~ "selfreport",
           method == "Unknown" ~ "selfreport",
           TRUE ~ method
         )) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG"))

write_csv(prev_cleaned_data, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/prev_clean_sourced.csv")

prev_cleaned_text <- prev_cleaned_data %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(prev_cleaned_data), ")"),
         name = "Cleaned HIV prevalence data") %>%
  select(name, n, everything())

prev_clean_labels <- collapse(prev_cleaned_text)

setwd(rprojroot::find_rstudio_root_file())

prev_id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_extrapolate_naomi", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022)'), draft = FALSE)
})

prev_id <- c(prev_id[!is.na(prev_id)], orderly::orderly_search(name = "aaa_extrapolate_naomi", query = paste0('latest(parameter:iso3 == "COD")'), draft = FALSE))

prev_final <- lapply(paste0("archive/aaa_extrapolate_naomi/", prev_id, "/prev.csv"),
                    function(x) {read_csv(x) %>% select(-any_of("...1"))}) %>%
  bind_rows()

prev_final_text <- prev_final %>%
  mutate(kp = ifelse(kp == "TGW", "TG", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG")) %>%
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

# subgraph input_cluster {
#   node [fontname = Helvetica, fontcolor = darkslategray, shape = rectangle, color = darkslategray];
#   inp_gam [label = '@@1-1'];
#   inp_atlas [label = '@@1-2'];
#   inp_gf [label = '@@1-3'];
#   inp_cdc [label = '@@1-4'];
#   inp_surv [label = '@@1-5'];
# }

prev_flow <- grViz("
digraph a_nice_graph {
  
    subgraph cluster_0 {
    # style=filled;
    # color=lightgrey;
  node [fontname = Helvetica, fontcolor = darkslategray,shape = rectangle, color = darkslategray]
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
    
  node [fontname = Helvetica, fontcolor = darkslategray,shape = rectangle, color = darkslategray]
  inp_total [label = '@@1-6']
  m1_dedup [label = '@@2-1']
  m2_nonspec [label = '@@2-2']
  m3_unsourced [label = '@@2-3']
  clean [label = '@@3']
  c1_denom [label = '@@2-6']
  final [label = '@@4']
    
  node [shape=none, width=0, height=0, label='']
  p3 -> clean;
  p6 -> final;
  {rank=same; p1 -> m1_dedup};
  {rank=same; p2 -> m2_nonspec};
  {rank=same; p3 -> m3_unsourced};
  {rank=same; p6 -> c1_denom};
  
  inp_cdc -> inp_total;

  edge [dir = none]
    inp_total -> p1;
    p1 -> p2;
    p2 -> p3;
    clean -> p6;
    
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



#### ART coverage

# art_spreadsheet_extract <- lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/ART coverage/Edited/", full.names = TRUE, pattern = "csv"), read_csv, na= "", show_col_types = FALSE)
# 
# art_spreadsheet_extract <- c(art_spreadsheet_extract, 
#                               lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/ART coverage/Edited/", full.names = TRUE, pattern = "xlsx"), readxl::read_xlsx)
# ) %>%
#   lapply(type_convert) %>%
#   bind_rows()

art_spreadsheet_extract <- read_csv("~/Imperial College London/Key population data - WP - General/Combined data/ART coverage/Edited/art_merged.csv")

art_spreadsheet_extract <-  art_spreadsheet_extract %>%
  rename(art = art_coverage) %>%
  mutate(art = ifelse(art > 1, art/100, art),
         art_upper = ifelse(art_upper > 1, art_upper/100, art_upper),
         art_lower = ifelse(art_lower > 1, art_lower/100, art_lower),
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
  )

write_csv(art_spreadsheet_extract, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_spreadsheet_extract.csv")

art_remove_label <- list()

art_total_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_all_data.csv")

art_inputs <- art_total_dat %>%
  filter(!dataset %in% c("Goals_Nat", "Optima input")) %>%
  count(dataset, kp) 

art_simple_deduplication <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_deduplicated.csv")

art_remove_label$duplicates <- nrow(art_total_dat) - nrow(art_simple_deduplication) + art_spreadsheet_extract %>%
  filter(!is.na(duplicate_of) | !is.na(is_aggregate)) %>%
  nrow()

art_remove_label$duplicates <- paste0("Duplicated data (n = ", art_remove_label$duplicates, ")")

art_surveillance_review_input <- art_spreadsheet_extract %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c"),
         kp = ifelse(kp %in% c("TGW", "TGM", "TGW/GQ"), "TG", kp)) %>%
  filter(is.na(duplicate_of),
         !is.na(iso3),
         is.na(uid)) %>%
  count(kp) %>%
  mutate(dataset = "Surveillance review")

art_inputs <- art_inputs %>%
  bind_rows(art_surveillance_review_input)

art_inputs <- art_inputs %>%
  bind_rows(art_inputs %>%
              group_by(kp) %>%
              summarise(n = sum(n)) %>%
              mutate(dataset = "Total")) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG"),
         !dataset %in% c("Goals_Nat", "Optima input"))

art_inputs_total <- art_inputs %>%
  group_by(dataset) %>%
  summarise(n = sum(n)) %>%
  mutate(n = paste0("(n = ", n, ")"))

art_inputs_text <- crossing(dataset = art_inputs$dataset,
                             kp = c("FSW", "MSM", "PWID", "TG")) %>%
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

art_spreadsheet_extract <- art_spreadsheet_extract %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c")) %>%
  filter(is.na(duplicate_of),
         is.na(is_aggregate),
         !is.na(iso3))

# Remove non-specific area names
art_remove_label$nonspec <- art_spreadsheet_extract %>%
  filter(str_detect(area_name, regex("urban|rural|county|counties|districts|state|total", ignore_case = TRUE))) %>%
  nrow()

art_remove_label$nonspec <- paste0("Unspecific area name (n = ", art_remove_label$nonspec, ")")

art_spreadsheet_extract <- art_spreadsheet_extract %>%
  filter(!str_detect(area_name, regex("urban|rural|county|counties|districts|state|total", ignore_case = TRUE)))

# Remove outliers
# art_remove_label$unconf <- NA

# art_spreadsheet_extract %>%
#   filter(!data_checked %in% c("yes", "remove"),
#          (is.na(source_found) | source_found == "no"),
#          year > 2009) %>%
#   count(iso3, kp, year, ref) %>%
#   arrange(desc(n)) %>%
#   View()

art_checked <- art_spreadsheet_extract %>%
  filter(data_checked == "yes",
         source_found != "private",
         year > 2009
  ) %>%
  mutate(data_checked = "Yes")

art_unknown <- art_spreadsheet_extract %>%
  filter(
    (is.na(data_checked) & source_found == "no")  | (is.na(data_checked) & is.na(source_found)),
    year > 2009
  ) %>%
  mutate(data_checked = "No")

art_remove_label$unsourced <- paste0("Unsourced (n = ", nrow(art_unknown), ")")

art_cleaned_data <- bind_rows(art_checked) %>%
  mutate(kp = ifelse(kp == "TGW", "TG", kp),
         art = ifelse(method == "VLS" & !is.na(method), art/.9, art),
         art_lower = ifelse(method == "VLS" & !is.na(method), art_lower/.9, art_lower),
         art_upper = ifelse(method == "VLS" & !is.na(method), art_upper/.9, art_upper),
         method = ifelse(method == "VLS" & !is.na(method), "Lab", method),
         method = case_when(
           method %in% c("Lab") ~ "lab",
           method %in%c("Self report", "self-report", "Self-report") ~ "selfreport",
           method == "Unknown" ~ NA_character_,
           TRUE ~ method
         )
         ) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG"))

write_csv(art_cleaned_data, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_clean_sourced.csv")

art_cleaned_text <- art_cleaned_data %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(art_cleaned_data), ")"),
         name = "Cleaned ART coverage data") %>%
  select(name, n, everything())

art_clean_labels <- collapse(art_cleaned_text)

setwd(rprojroot::find_rstudio_root_file())

art_id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_extrapolate_naomi", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022)'), draft = FALSE)
})

art_id <- c(art_id[!is.na(art_id)], orderly::orderly_search(name = "aaa_extrapolate_naomi", query = paste0('latest(parameter:iso3 == "COD")'), draft = FALSE))

art_final <- lapply(paste0("archive/aaa_extrapolate_naomi/", art_id[!is.na(art_id)], "/art.csv"),
                     function(x) {read_csv(x) %>% select(-any_of("...1"))}) %>%
  bind_rows()

art_final_text <- art_final %>%
  mutate(kp = ifelse(kp == "TGW", "TG", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG")) %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(art_final), ")"),
         name = "Final ART coverage data") %>%
  select(name, n, everything())

art_remove_label$denom <- nrow(art_cleaned_data) - nrow(art_final)
art_remove_label$denom <- paste0("No area match (n = ", art_remove_label$denom, ")")

art_final_labels <- collapse(art_final_text)

# saveRDS(list('art_inputs_labels' = art_inputs_labels,
#              "art_remove_label" = art_remove_label,
#              "art_clean_labels" = art_clean_labels,
#              "art_final_labels" = art_final_labels),
#         "R/Report/R objects for report/Prevalence/art_flow_input.rds")

saveRDS(art_final_text, "R/Report/R objects for report/ART coverage/art_count_text.rds")
write_csv(art_final, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/ART coverage/art_final.csv")
saveRDS(art_inputs_text, "R/Report/R objects for report/ART coverage/art_input_count_text.rds")

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
  clean [label = '@@3']
  c1_denom [label = '@@2-6']
  final [label = '@@4']
    
  node [shape=none, width=0, height=0, label='']
  p3 -> clean;
  p6 -> final;
  {rank=same; p1 -> m1_dedup};
  {rank=same; p2 -> m2_nonspec};
  {rank=same; p3 -> m3_unsourced};
  {rank=same; p6 -> c1_denom};
  
  inp_gf -> inp_total;

  edge [dir = none]
    inp_total -> p1;
    p1 -> p2;
    p2 -> p3;
    clean -> p6;
    
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