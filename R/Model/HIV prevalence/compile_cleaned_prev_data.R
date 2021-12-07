edit_dat <- lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/HIV prevalence/Edited/", full.names = TRUE, pattern = "csv"), read_csv, na= "")

edit_dat <- c(edit_dat, 
              lapply(list.files("~/Imperial College London/Key population data - WP - General/Combined data/HIV prevalence/Edited/", full.names = TRUE, pattern = "xlsx"), readxl::read_xlsx)
              ) %>%
  lapply(type_convert) %>%
  bind_rows()

write_csv(edit_dat, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/2021_12_6_prev_spreadsheet_extract.csv")

lapply(edit_dat, "[", "prev") %>%
  lapply(str)

# ## Duplicates at this stage
edit_dat %>%
  bind_rows() %>%
  filter(!is.na(duplicate_of)) %>%
  nrow()

# 
# ## Non-specific area names
# edit_dat %>%
#   bind_rows() %>%
#   filter(str_detect(area_name, "urban|Urban|county|counties|districts|states|States|cities|Cities|rural|Rural")) %>%
#            count()
# 
# ## Extrapolated
# edit_dat %>%
#   bind_rows() %>%
#   filter(str_detect(method, "xtrapol"),
#          is.na(duplicate_of)) %>%
#   count(kp)


edit_dat <- edit_dat %>%
  bind_rows() %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c")) %>%
  filter(is.na(duplicate_of),
         !is.na(iso3))

edit_dat <- edit_dat %>%
  filter(!str_detect(area_name, regex("urban|rural|county|counties|districts|state|total", ignore_case = TRUE)))

edit_dat <- edit_dat %>%
  filter(!str_detect(method, regex("extrapolat", ignore_case = TRUE)) | is.na(method))

unknown <- edit_dat %>%
  filter(
    (is.na(data_checked) & source_found == "no")  | (is.na(data_checked) & is.na(source_found)),
    year > 2009
  )

truly_checked <- edit_dat %>%
  filter(data_checked == "yes",
         year > 2009
  )

deduplicated_data <- truly_checked %>%
  bind_rows(unknown)

deduplicated_data <- deduplicated_data %>%
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

write_csv(deduplicated_data, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/HIV prevalence/2021_12_06_prev_clean.csv")

####

deduplicated_data <- read_csv("src/aaa_assign_populations/2021_11_28_deduplicated_pse_data.csv")

deduplicated_data %>%
  distinct(iso3, method, kp, year)

recode_data %>%
  count(method) %>%
  arrange(desc(n))

recode_data <- deduplicated_data %>%
  distinct(iso3, method, kp, year) %>%
  mutate(method = case_when(
    method %in% c("Programmatic mapping", "Hotspot mapping", "PLACE", "Enumeration/mapping", "Mapping", "Mapping and enumeration") ~ "PLACE/Mapping",
    method %in% c("Unique object", "Unique object multiplier", "Unique Object Multiploer") ~ "Multiplier",
    method %in% c("Service Multiplier", "Service multiplier") ~ "Multiplier",
    method %in% c("Unique event multiplier", "Unique event", "Event multiplier") ~ "Multiplier",
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
                  "Consensus of % adult population and population growth"
                  
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
                  "Unique object, social, service"
                  
                  ) ~ "Multiple methods - empirical",
    TRUE ~ method
  ))

recode_data %>%
  count(method) %>%
  View()

other_methods <- recode_data %>%
  count(method) %>%
  filter(n<5) %>%
  .$method

recode_data <- recode_data %>%
  mutate(method = case_when(
    method %in% other_methods ~ "Other",
    TRUE ~ method
  ))

recode_data %>%
  filter(!is.na(method),
         kp %in% c("FSW", "MSM", "PWID")) %>%
  mutate(year = plyr::round_any(year, 3),
         year = paste0(year, "-", year+2)) %>%
  count(method, kp, year) %>%
  ggplot(aes(x=year, y=n, fill=method)) +
    geom_col() +
    facet_wrap(~kp)

recode_data %>%
  filter(!is.na(method),
         kp %in% c("FSW", "MSM", "PWID")) %>%
  mutate(year = plyr::round_any(year, 3),
         year = paste0(year, "-", year+2),
         method = case_when(
           method %in% c("2S-CRC", "3S-CRC", "Multiplier", "Multiple methods - empirical", "SS-PSE", "RDS") ~ "empirical",
           TRUE ~ "non-empirical"
         )) %>%
  count(method, year) %>%
  arrange(year) %>%
  group_by(year) %>%
  mutate(prop = n/sum(n))

### RUN AAA ASSIGN POPULATIONS

id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_assign_populations", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

dat <- lapply(paste0("archive/aaa_assign_populations/", id[!is.na(id)], "/pse_prevalence.csv"),
              read_csv)

dat <- lapply(list.files("draft/aaa_assign_populations", full.names = TRUE) %>% lapply(list.files, pattern = "pse_prevalence", full.names = TRUE),
              read_csv)

dat <- dat %>%
  bind_rows() %>%
  filter(is.na(x),
         !is.na(population_proportion)) 

write_csv(dat, "~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/Method comparison/deduplicated_pse_data.csv")

bad_match <- lapply(list.files("draft/aaa_assign_populations", full.names = TRUE) %>% lapply(list.files, pattern = "bad_match", full.names = TRUE),
                    read_csv) %>%
  bind_rows()

bad_match %>%
  filter(is.na(x)) %>%
  View()

######## Descriptiive table..

dat <- dat %>%
  mutate(iso3 = countrycode(country.name, "country.name", "iso3c"),
         kp = case_when(
           kp == "SW" ~ "FSW",
           kp == "TGW" ~ "TG",
           TRUE ~ kp
         )) %>%
  left_join(region)

## Data counts
dat %>%  
  count(kp, region) %>%
  bind_rows(
    dat %>%
      count(kp) %>%
      mutate(region = "SSA")
  ) %>%
  mutate(region = factor(region, levels=c("SSA", "ESA", "WCA")),
         kp = case_when(
           kp == "SW" ~ "FSW",
           kp == "TGW" ~ "TG",
           TRUE ~ kp
         ),
         kp = factor(kp, levels = c("FSW", "MSM", "PWID", "TG"))
         ) %>%
  arrange(kp, region)

## Countries with data
countries_with_data <- crossing(iso3 = ssa_iso3) %>%
  left_join(region) %>%
  left_join(
    dat %>%
      distinct(kp, iso3) %>%
      mutate(has = 1) %>%
      pivot_wider(names_from = kp, values_from = has),
  )

countries_with_data[is.na(countries_with_data)] <- 0

countries_with_data %>%
  summarise(across(c("MSM", "FSW", "TG", "PWID"), sum))

countries_with_data %>%
  group_by(region) %>%
  summarise(across(c("MSM", "FSW", "TG", "PWID"), sum))

########
data_input <- read_csv("src/aaa_assign_populations/2021_11_28_deduplicated_pse_data.csv")
processed_uid <- filter(dat, !is.na(uid))$uid

data_input %>%
  filter(!is.na(uid)) %>%
  filter(!uid %in% processed_uid) %>%
  View()


checked_and_unknown_proportions <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/Method comparison/checked_and_unknown_proportions.csv")
 
truly_checked_proportions <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/Method comparison/truly_checked_proportions.csv")

mapped_place_proportions <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/key-populations/PSE/Method comparison/mapped_place_proportions.csv")

dat1 <- truly_checked_proportions %>%
  select(country.name, year, kp, population_proportion) %>%
  mutate(source = "Checked") %>%
  bind_rows(
    # checked_and_unknown_proportions %>%
    #   select(country.name, year, kp, population_proportion) %>%
    #   mutate(source = "Checked and unknown"),
    mapped_place_proportions %>%
      filter(method != "PLACE") %>%
      select(country.name, year, kp, population_proportion) %>%
      mutate(source = "Mapped"),
    mapped_place_proportions %>%
      filter(method == "PLACE") %>%
      select(country.name, year, kp, population_proportion) %>%
      mutate(source = "PLACE")
      
  )

dat1 %>%
  filter(kp %in% c("FSW", "MSM", "PWID")) %>%
  ggplot(aes(x=year, y=population_proportion, color=source)) +
    geom_jitter() +
    lims(y=c(0,0.25)) +
    facet_wrap(~kp)
    
dat1 %>%
  filter(kp %in% c("FSW", "MSM", "PWID")) %>%
  ggplot(aes(x=kp, y=population_proportion, color=source)) +
    geom_boxplot() +
    lims(y=c(0,0.1))

truly_checked_proportions %>%
  count(method) %>%
  arrange(desc(n)) %>%
  View()
