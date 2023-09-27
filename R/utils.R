library(tidyverse)
library(orderly)
library(countrycode)
library(purrr)
library(moz.utils)

ssa_iso3 <- moz.utils::ssa_iso3()

possibly_run <- purrr::possibly(.f = orderly_run, otherwise = NULL)
possibly_pull <- purrr::possibly(.f = orderly_pull_archive, otherwise = NA)

map(ssa_iso3, ~possibly_pull(paste0(tolower(.x), "_data_areas"), remote = "naomi_2021"))
orderly_pull_archive("ssd_data_areas", remote = "fertility")

#######
id <- map(ssa_iso3, ~possibly_pull("aaa_download_constrained_worldpop", id = paste0('latest(parameter:iso3 == "', .x, '" && parameter:version == 2022)'), recursive = TRUE, remote = "inference"))
id <- map(area_tasks, ~possibly_pull(.x, id = paste0('latest(parameter:version == "', 2021, '")'), recursive = FALSE, remote = "naomi_2021"))

### Assign populations

lapply(ssa_iso3, function(x) orderly_pull_dependencies("aaa_assign_populations", remote = "inference-web", parameters = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2022)')))
orderly_pull_dependencies("aaa_assign_populations", remote = "inference-web", parameters = paste0('latest(parameter:iso3 == "MWI" && parameter:version == 2022)'))

id <- orderly::orderly_batch("aaa_assign_populations", data.frame(iso3 = ssa_iso3, version = 2022))
# names(id) <- ssa_iso3
lapply(id$id[id$success == TRUE], orderly_commit)

id$iso3[id$success == FALSE]

orderly_dev_start_oli("aaa_assign_populations", data.frame(iso3 = "NAM"))
# setwd("src/aaa_assign_populations")

### Assign province
id <- map(c("ZAF", "TZA", "RWA", "GHA", "ETH", "COD", "CIV", "BDI", "UGA", "BEN", "TGO"), ~possibly_run("aaa_assign_province", parameters = data.frame(iso3 = .x)))
# names(id) <- ssa_iso3
lapply(id2 %>% compact(), orderly_commit)


orderly_dev_start_oli("aaa_extrapolate_naomi", data.frame(iso3 = "MOZ"))
# setwd("src/aaa_assign_province")

### Extrapolate Naomi
id <- map(ssa_iso3, ~possibly_run("aaa_extrapolate_naomi", parameters = data.frame(iso3 = .x)))
id <- orderly_batch("aaa_assign_populations", parameters = data.frame(iso3 = ssa_iso3[ssa_iso3 != "COD"], version = 2022))
# names(id) <- ssa_iso3
lapply(id %>% compact(), orderly_commit)

to_do <- names(id %>% keep(~is.null(.x)))

orderly_dev_start_oli("aaa_extrapolate_naomi", data.frame(iso3 = "TZA"))

### Get Spectrum
id <- map(ssa_iso3, ~possibly_run("aaa_download_worldpop", parameters = data.frame(iso3 = .x)))
names(id) <- ssa_iso3
lapply(id %>% compact(), orderly_commit)

### SEARCH
orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', "COD", '")'), draft = FALSE)


id <- orderly_batch("aaa_inputs_orderly_pull", parameters = data.frame(iso3 = ssa_iso3))
names(id) <- ssa_iso3
lapply(id, orderly_commit)
orderly_run("aaa_assign_populations", parameters = data.frame(iso3 = "ZAF"))

foo <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2021)'), draft = FALSE)
})

lapply(c("TZA"), function(x){
  orderly::orderly_search(name = "aaa_extrapolate_naomi", query = paste0('parameter:iso3 == "', x, '"'), draft = FALSE)
})


id <- map(ssa_iso3, ~possibly_run("aaa_assign_populations", parameters = data.frame(iso3 = .x)))
names(id) <- ssa_iso3
lapply(id %>% compact(), orderly_commit)
orderly_run("aaa_extrapolate_naomi", parameters = data.frame(iso3 = "ZAF"))

orderly_develop_start("aaa_extrapolate_naomi", parameters = data.frame(iso3 = "ZAF"))
setwd("src/aaa_extrapolate_naomi")

df %>%
  bind_rows() %>%
  filter(str_detect("CMR", area_id))

df %>%
  bind_rows()
  filter(is.na(x)) %>%
  select(-x) %>% View()

lapply("SSD", function(x){
  orderly::orderly_pull_archive("aaa_areas_pull", id = paste0('latest(parameter:iso3 == "', x, '")'), remote = "naomi_2021")
})


areas <- read_sf("~/Documents/GitHub/fertility_orderly/archive/tza_data_areas/20201130-150758-409ee8ac/tza_areas.geojson")


map("CMR", ~possibly_pull("aaa_outputs_adr_pull", id = paste0('latest(parameter:iso3 == "', .x, '")')))
map(c("KEN", "UGA"), ~possibly_pull("aaa_scale_pop", id = paste0('latest(parameter:iso3 == "', .x, '")'), remote = "fertility"))
id <- map(ssa_iso3, ~possibly_pull(paste0(.x, "_data_areas"), remote = "naomi_2021"))

names(suc) <- ssa_iso3

possibly_run <- purrr::possibly(.f = orderly_run, otherwise = NULL)
id_list <- map(ssa_iso3, ~possibly_run("aaa_extrapolate_naomi", parameters = data.frame(iso3 = .x)))
names(id_list) <- ssa_iso3

lapply(id_list %>%
         compact(),
       orderly_commit)


id_list <- map(ssa_iso3, ~possibly_run("aaa_extrapolate_naomi", parameters = data.frame(iso3 = .x)))

lapply(id_list %>%
         compact(),
       orderly_commit)

lapply(id_list %>% compact(), function(x) {orderly_push_archive("aaa_extrapolate_naomi", id=x)})

id_list <- map("BWA", ~possibly_run("aaa_assign_province", parameters = data.frame(iso3 = .x)))
id_list <- map(c("CMR", "COD", 
                 "LSO", "MWI", 
                 "SLE", "TGO", 
                 "ETH", "GAB", 
                 "NER", "SEN"), ~possibly_run("aaa_extrapolate_naomi", parameters = data.frame(iso3 = .x)))

names(id_list) <- ssa_iso3

orderly_run("aaa_extrapolate_naomi", parameters = data.frame(iso3 = "ZWE"))

orderly_develop_start("aaa_assign_province", parameters = data.frame(iso3 = "SEN"))
setwd("src/aaa_assign_province/")

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

lapply(c("aaa_download_worldpop"), function(y) {
  
  id <- lapply(ssa_iso3, function(x){
    orderly::orderly_search(name = y, query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
  })
  
  lapply(id[!is.na(id)], function(x) {orderly_push_archive(y, id=x)})
  
})



names(id) <- ssa_iso3
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

prev_dat <- lapply(file.path("archive/aaa_extrapolate_naomi", id_list %>% compact(), "anonymised_prev.csv"),
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

lapply("MOZ", function(x){
  orderly::orderly_search(name = "aaa_extrapolate_naomi", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

lapply("MOZ", function(x){
  orderly::orderly_search(name = "aaa_assign_province", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_assign_populations", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

pse_dat <- lapply(file.path("archive/aaa_assign_populations", id, "pse_prevalence.csv"),
                   read.csv)

pse_dat %>%
  bind_rows() %>%
  mutate(jitter_year = year + sample(-50:50, n(), replace = TRUE)/100,
         population_proportion = pse/population,
         color = "SSA") %>%
  select(kp, year, jitter_year, population_proportion, color) %>%
  filter(!is.na(population_proportion), population_proportion != 0) %>%
  write_csv("~/Dropbox/Work Streams/2021/Key populations/Guidance/anonymised_pse.csv")
