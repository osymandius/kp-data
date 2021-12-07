library(tidyverse)
library(DiagrammeR)
library(countrycode)

ssa_names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- countrycode(ssa_names, "country.name", "iso3c")

collapse <- function(...) {paste0(..., collapse = "\n")}

### PSE

pse_remove_label <- list()

pse_total_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/2021_11_28_pse_all_data.csv")

pse_inputs <- pse_total_dat %>%
  filter(!dataset %in% c("Goals_Nat", "Optima")) %>%
  count(dataset, kp) 

pse_simple_deduplication <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/2021_11_28_pse_distinct.csv")
pse_spreadsheet_extract <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/2021_12_6_pse_spreadsheet_extract.csv")

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
  filter(str_detect(method, regex("extrapolat", ignore_case = TRUE))) %>%
  nrow()

pse_remove_label$model <- paste0("Extrapolated/modelled data (n = ", pse_remove_label$model, ")")

pse_spreadsheet_extract <- pse_spreadsheet_extract %>%
  filter(!str_detect(method, regex("extrapolat", ignore_case = TRUE)) | is.na(method))

#Remove non-empirical methods
pse_remove_label$non_emp <- pse_spreadsheet_extract %>%
  filter(str_detect(method, regex("enumeration|delphi", ignore_case = TRUE))) %>%
  nrow()

pse_remove_label$non_emp <- paste0("Non-empirical methods (n = ", pse_remove_label$non_emp, ")")

pse_spreadsheet_extract <- pse_spreadsheet_extract %>%
  filter(!str_detect(method, regex("enumeration|delphi", ignore_case = TRUE)) | is.na(method))

#Remove outliers
pse_remove_label$outlier <- NA

#Remove unconfirmed data
pse_remove_label$unconf <- NA

truly_checked <- pse_spreadsheet_extract %>%
  filter(data_checked == "yes",
         year > 2009
  )

mapped_place <- pse_spreadsheet_extract %>%
  filter(str_detect(method, "apping|PLACE"),
         data_checked == "remove",
         is.na(duplicate_of),
         !country.name == area_name,
         year > 2009)

pse_cleaned_data <- bind_rows(truly_checked, mapped_place) %>%
  mutate(kp = ifelse(kp == "TGW", "TG", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG"))

write_csv(pse_cleaned_data, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/2021_12_06_spreadsheet_cleaned.csv")

pse_cleaned_text <- pse_cleaned_data %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(pse_cleaned_data), ")"),
         name = "Cleaned PSE data") %>%
  select(name, n, everything())

pse_clean_labels <- collapse(pse_cleaned_text)

final_pse_id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_assign_populations", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

final_pse <- lapply(paste0("archive/aaa_assign_populations/", final_pse_id[!is.na(final_pse_id)], "/pse_prevalence.csv"),
       read_csv) %>%
  bind_rows() %>%
  filter(is.na(x),
         !is.na(population_proportion)
  )

pse_final_text <- final_pse %>%
  mutate(kp = ifelse(kp == "TGW", "TG", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG")) %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(final_pse), ")"),
         name = "Final PSE data") %>%
  select(name, n, everything())

pse_remove_label$denom <- nrow(pse_cleaned_data) - nrow(final_pse)
pse_remove_label$denom <- paste0("No population denominator (n = ", pse_remove_label$denom, ")")

pse_final_labels <- collapse(pse_final_text)

grViz("
digraph a_nice_graph {

# node definitions with substituted label text
  node [fontname = Helvetica, fontcolor = darkslategray,
        shape = rectangle, color = darkslategray]
inp_gam [label = '@@1-1']
inp_atlas [label = '@@1-2']
inp_gf [label = '@@1-3']
inp_cdc [label = '@@1-4']
inp_surv [label = '@@1-5']
inp_total [label = '@@1-6']
m1_dedup [label = '@@2-1']
m2_nonspec [label = '@@2-2']
m3_model [label = '@@2-3']
m4_non_emp [label = '@@2-4']
m5_outlier [label = 'Outlier (n=??)']
m6_unconf [label = 'Unconfirmed (n=??)']
clean [label = '@@3']
c1_denom [label = '@@2-6']
final [label = '@@4']

node [shape=none, width=0, height=0, label='']
  p6 -> clean
  p7 -> final
  {rank=same; p1 -> m1_dedup}
  {rank=same; p2 -> m2_nonspec}
  {rank=same; p3 -> m3_model}
  {rank=same; p4 -> m4_non_emp}
  {rank=same; p5 -> m4_outlier}
  {rank=same; p6 -> m5_unconf}
  {rank=same; p7 -> c1_denom}

{inp_gam inp_atlas inp_cdc inp_gf inp_surv} -> inp_total

# edge definitions with the node IDs
edge [dir = none]
  inp_total -> p1;
  p1 -> p2;
  p2 -> p3;
  p3 -> p4;
  p4 -> p5;
  p5 -> p6;
  clean -> p7
  
 

}

[1]: pse_inputs_labels
[2]: unlist(pse_remove_label)
[3]: pse_clean_labels
[4]: pse_final_labels
")

### Prevalence

prev_spreadsheet_extract <- lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/HIV prevalence/Edited/", full.names = TRUE, pattern = "csv"), read_csv, na= "")

prev_spreadsheet_extract <- c(prev_spreadsheet_extract, 
              lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/HIV prevalence/Edited/", full.names = TRUE, pattern = "xlsx"), readxl::read_xlsx)
) %>%
  lapply(type_convert) %>%
  bind_rows()

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
           
         )
  )

write_csv(prev_spreadsheet_extract, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/2021_12_6_prev_spreadsheet_extract.csv")

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
prev_remove_label$unconf <- NA

prev_checked <- prev_spreadsheet_extract %>%
  filter(data_checked == "yes",
         year > 2009
  )

prev_unknown <- prev_spreadsheet_extract %>%
  filter(
    (is.na(data_checked) & source_found == "no")  | (is.na(data_checked) & is.na(source_found)),
    year > 2009
  )

prev_cleaned_data <- bind_rows(prev_checked, prev_unknown) %>%
  mutate(kp = ifelse(kp == "TGW", "TG", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG"))

write_csv(prev_cleaned_data, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/2021_12_06_prev_clean.csv")

prev_cleaned_text <- prev_cleaned_data %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(prev_cleaned_data), ")"),
         name = "Cleaned HIV prevalence data") %>%
  select(name, n, everything())

prev_clean_labels <- collapse(prev_cleaned_text)

final_prev_id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_extrapolate_naomi", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

final_prev <- lapply(paste0("archive/aaa_extrapolate_naomi/", final_prev_id[!is.na(final_prev_id)], "/prev.csv"),
                    function(x) {read_csv(x) %>% select(-`...1`)}) %>%
  bind_rows()

prev_final_text <- final_prev %>%
  mutate(kp = ifelse(kp == "TGW", "TG", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG")) %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(final_prev), ")"),
         name = "Final PSE data") %>%
  select(name, n, everything())

prev_remove_label$denom <- nrow(prev_cleaned_data) - nrow(final_prev)
prev_remove_label$denom <- paste0("No population denominator (n = ", prev_remove_label$denom, ")")

prev_final_labels <- collapse(prev_final_text)

grViz("
digraph a_nice_graph {

# node definitions with substituted label text
  node [fontname = Helvetica, fontcolor = darkslategray,
        shape = rectangle, color = darkslategray]
inp_gam [label = '@@1-1']
inp_atlas [label = '@@1-2']
inp_gf [label = '@@1-3']
inp_cdc [label = '@@1-4']
inp_surv [label = '@@1-5']
inp_total [label = '@@1-6']
m1_dedup [label = '@@2-1']
m2_nonspec [label = '@@2-2']
m3_model [label = '@@2-3']
m4_outlier [label = 'Outlier (n=??)']
m5_unconf [label = 'Unconfirmed (n=??)']
clean [label = '@@3']
c1_denom [label = '@@2-6']
final [label = '@@4']

node [shape=none, width=0, height=0, label='']
  p5 -> clean
  p6 -> final
  {rank=same; p1 -> m1_dedup}
  {rank=same; p2 -> m2_nonspec}
  {rank=same; p3 -> m3_model}
  {rank=same; p4 -> m4_outlier}
  {rank=same; p5 -> m5_unconf}
  {rank=same; p6 -> c1_denom}

{inp_gam inp_atlas inp_cdc inp_gf inp_surv} -> inp_total

# edge definitions with the node IDs
edge [dir = none]
  inp_total -> p1;
  p1 -> p2;
  p2 -> p3;
  p3 -> p4;
  p4 -> p5;
  clean -> p6
  
 

}

[1]: prev_inputs_labels
[2]: unlist(prev_remove_label)
[3]: prev_clean_labels
[4]: prev_final_labels
")
