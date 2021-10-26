library(tidyverse)
library(orderly)

iso3_vec <- c("BDI", "BWA", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")

id <- orderly_batch("aaa_assign_province", parameters = data.frame(iso3 = iso3_vec))

lapply(id_list[c(8:33)] %>% compact(), orderly_commit)

df <- lapply(file.path("draft/aaa_assign_province", id, "prev_assigned_province.csv"), read.csv) %>%
  lapply(function(x) {
    if(nrow(x))
      x
    else
      NULL
  }) %>%
  purrr::compact()

orderly_develop_start("aaa_assign_province", parameters = data.frame(iso3 = "SLE"))
setwd("src/aaa_assign_province")

df %>%
  bind_rows() %>%
  filter(str_detect("CMR", area_id))

df %>%
  bind_rows()
  filter(is.na(x)) %>%
  select(-x) %>% View()

lapply("GNB", function(x){
  orderly::orderly_pull_archive("aaa_scale_pop", id = paste0('latest(parameter:iso3 == "', x, '")'), remote = "fertility")
})

areas <- read_sf("~/Documents/GitHub/fertility_orderly/archive/tza_data_areas/20201130-150758-409ee8ac/tza_areas.geojson")

possibly_pull <- purrr::possibly(.f = orderly_pull_archive, otherwise = NA)
map("BWA", ~possibly_pull("aaa_scale_pop", id = paste0('latest(parameter:iso3 == "', .x, '")'), remote = "fertility"))
map(iso3_vec, ~possibly_pull("aaa_extrapolate_naomi", id = paste0('latest(parameter:iso3 == "', .x, '")'), remote = "fertility"))

names(suc) <- iso3_vec

possibly_run <- purrr::possibly(.f = orderly_run, otherwise = NULL)
id_list <- map(iso3_vec, ~possibly_run("aaa_assign_province", parameters = data.frame(iso3 = .x)))
id_list <- map(iso3_vec, ~possibly_run("aaa_extrapolate_naomi", parameters = data.frame(iso3 = .x)))
id_list <- map("BWA", ~possibly_run("aaa_assign_province", parameters = data.frame(iso3 = .x)))
id_list <- map(c("CMR", "COD", 
                 "LSO", "MWI", 
                 "SLE", "TGO", 
                 "ETH", "GAB", 
                 "NER", "SEN"), ~possibly_run("aaa_extrapolate_naomi", parameters = data.frame(iso3 = .x)))

names(id_list) <- iso3_vec

orderly_run("aaa_assign_populations", parameters = data.frame(iso3 = "BWA"), tags = "surveillance_only")

orderly_develop_start("aaa_outputs_adr_pull", parameters = data.frame(iso3 = "SEN"))
setwd("src/aaa_extrapolate_naomi/")

lapply(id_list %>%
  compact(),
  orderly_commit)

lapply(id %>%
         compact() %>%
         unlist,
       function(x) {orderly_push_archive("aaa_download_worldpop", id=x)})

dat %>%
  bind_rows() %>%
  select(-X) %>%
  write_csv("~/Dropbox/Work Streams/2021/Key populations/Guidance/anonymised_prev_matched.csv")

id <- lapply(iso3_vec, function(x){
  orderly::orderly_search(name = "aaa_download_worldpop", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

names(id) <- iso3_vec
id <- id_list
id <- id %>%unlist()
id <- id[!is.na(id)]

pse_dat <- lapply(file.path("archive/aaa_assign_populations", id, "pse_prevalence.csv"),
                   read.csv)

pse_dat %>%
  bind_rows() %>%
  filter(kp == "MSM") %>%
  ggplot(aes(x=year, y=population_proportion)) +
    geom_jitter() +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0,.1)) +
    geom_hline(aes(yintercept = 0.01), color="red", linetype = 2, size=1) +
    moz.utils::standard_theme() +
    labs(x=element_blank(), y="MSM population proportion")

pse_dat %>%
  bind_rows() %>%
  filter(kp == "MSM") %>%
  ggplot(aes(x=year, y=population_proportion, group=year)) +
    geom_boxplot() +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0,.1)) +
    geom_hline(aes(yintercept = 0.01), color="red", linetype = 2, size=1) +
    moz.utils::standard_theme() +
    labs(x=element_blank(), y="MSM population proportion")

pse_dat %>%
  bind_rows() %>%
  filter(kp == "MSM") %>%
  group_by(kp, country.name, year) %>%
  mutate(idx = cur_group_id()) %>%
  View()

max(int$idx)


int <- pse_dat %>%
  bind_rows() %>%
  group_by(kp, country.name, year) %>%
  mutate(idx = cur_group_id()) %>%
  add_count(ref)

max(int$idx)
max(int$n)

prev_dat <- lapply(file.path("archive/aaa_extrapolate_naomi", id, "anonymised_prev.csv"),
       read.csv)

prev_dat %>%
  bind_rows() %>%
  mutate(jitter_year = year + sample(-50:50, n(), replace = TRUE)/100) %>%
  select(kp, year, jitter_year, value, provincial_value, ratio) %>%
  filter(!is.na(provincial_value), provincial_value != 0, value < 1) %>%
  write_csv("~/Dropbox/Work Streams/2021/Key populations/Guidance/anonymised_prev_matched.csv")

art_dat <- lapply(file.path("archive/aaa_extrapolate_naomi", id, "anonymised_art.csv"),
                   read.csv)

art_dat %>%
  bind_rows() %>%
  mutate(jitter_year = year + sample(-50:50, n(), replace = TRUE)/100) %>%
  select(kp, year, jitter_year, value, provincial_value, ratio) %>%
  filter(!is.na(provincial_value), provincial_value != 0, value < 1) %>%
  write_csv("~/Dropbox/Work Streams/2021/Key populations/Guidance/anonymised_art_matched.csv")

dat <- lapply(file.path("archive/aaa_extrapolate_naomi", id, "extrapolated_naomi.csv"),
              read.csv) %>%
  bind_rows()

lapply(iso3_vec, function(x){
  orderly::orderly_search(name = "aaa_extrapolate_naomi", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

lapply("MOZ", function(x){
  orderly::orderly_search(name = "aaa_assign_province", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

lapply("MOZ", function(x){
  orderly::orderly_search(name = "aaa_assign_populations", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

orderly::orderly_run("aaa_assign_populations", data.frame(iso3 = "TCD"))

id_list %>%
  keep(~is.null(.x))

orderly_develop_start("aaa_assign_province", parameters = data.frame(iso3 = "BDI"))
setwd("src/aaa_assign_province")

