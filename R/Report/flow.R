library(tidyverse)
library(DiagrammeR)


### PSE

remove_label <- list()

pse_total_dat <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/2021_11_28_pse_all_data.csv")

inputs <- pse_total_dat %>%
  filter(!dataset %in% c("Goals_Nat", "Optima")) %>%
  count(dataset, kp) 

pse_simple_deduplication <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/2021_11_28_pse_distinct.csv")
pse_spreadsheet_extract <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/2021_12_6_pse_spreadsheet_extract.csv")

remove_label$duplicates <- nrow(pse_total_dat) - nrow(pse_simple_deduplication) + pse_spreadsheet_extract %>%
                                                          filter(!is.na(duplicate_of)) %>%
                                                          nrow()

remove_label$duplicates <- paste0("Duplicated data (n = ", remove_label$duplicates, ")")

surveillance_review_input <- pse_spreadsheet_extract %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c"),
         kp = ifelse(kp == "TGW", "TG", kp)) %>%
  filter(is.na(duplicate_of),
         !is.na(iso3),
         is.na(uid)) %>%
  count(kp) %>%
  mutate(dataset = "Surveillance review")

inputs <- inputs %>%
  bind_rows(surveillance_review_input)

inputs <- inputs %>%
  bind_rows(inputs %>%
              group_by(kp) %>%
              summarise(n = sum(n)) %>%
              mutate(dataset = "Total")) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG"))

inputs_total <- inputs %>%
  group_by(dataset) %>%
  summarise(n = sum(n)) %>%
  mutate(n = paste0("(n = ", n, ")"))

inputs_text <- crossing(dataset = inputs$dataset,
                        kp = c("FSW", "MSM", "PWID", "TG")) %>%
  left_join(inputs) %>%
  mutate(n = replace_na(n, 0),
         n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(-dataset, ~paste0(cur_column(), " ", .x))) %>%
  left_join(inputs_total) %>%
  select(dataset, n, everything()) %>%
  mutate(dataset = factor(dataset,
                          levels = c("GAM", "KP_Atlas", "GF", "CDC", "Surveillance review",  "Total"),
                          labels = c("UNAIDS GAM", "UNAIDS KP Atlas", "Global Fund", "CDC", "Surveillance review", "Total"))) %>%
  arrange(dataset)

collapse <- function(...) {paste0(..., collapse = "\n")}
inputs_labels <- apply(inputs_text, 1, collapse)

remove_label$nonspec <- pse_spreadsheet_extract %>%
  filter(str_detect(area_name, regex("urban|rural|county|counties|districts|state|total", ignore_case = TRUE))) %>%
  nrow()

remove_label$nonspec <- paste0("Unspecific area name (n = ", remove_label$nonspec, ")")

remove_label$model <- pse_spreadsheet_extract %>%
  filter(str_detect(method, regex("extrapolat", ignore_case = TRUE))) %>%
  nrow()

remove_label$model <- paste0("Extrapolated/modelled data (n = ", remove_label$model, ")")

remove_label$outlier <- NA
remove_label$unconf <- NA

cleaned_data <- read_csv("src/aaa_assign_populations/2021_12_06_deduplicated_pse_data.csv") %>%
  mutate(kp = ifelse(kp == "TGW", "TG", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG"))
  
cleaned_text <- cleaned_data %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(cleaned_data), ")"),
         name = "Cleaned PSE data") %>%
  select(name, n, everything())

clean_labels <- collapse(cleaned_text)

final_pse_id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_assign_populations", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

final_pse <- lapply(paste0("archive/aaa_assign_populations/", final_pse_id[!is.na(final_pse_id)], "/pse_prevalence.csv"),
       read_csv) %>%
  bind_rows() %>%
  filter(is.na(x),
         !is.na(population_proportion)
  )

final_text <- final_pse %>%
  mutate(kp = ifelse(kp == "TGW", "TG", kp)) %>%
  filter(kp %in% c("FSW", "MSM", "PWID", "TG")) %>%
  count(kp) %>%
  mutate(n = paste0("n = ", n)) %>%
  pivot_wider(names_from = kp, values_from = n) %>%
  mutate(across(everything(), ~paste0(cur_column(), " ", .x)),
         n = paste0("(n = ", nrow(final_pse), ")"),
         name = "Final PSE data") %>%
  select(name, n, everything())

remove_label$denom <- nrow(cleaned_data) - nrow(final_pse)
remove_label$denom <- paste0("No population denominator (n = ", remove_label$denom, ")")

final_labels <- collapse(final_text)

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

[1]: inputs_labels
[2]: unlist(remove_label)
[3]: clean_labels
[4]: final_labels
")
