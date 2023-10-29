iso3_c <- iso3

areas <- read_sf("depends/naomi_areas.geojson") %>%
  mutate(iso3 = iso3_c) %>%
  st_make_valid()

admin1_lvl <-
  filter(read_csv("resources/iso_mapping_fit.csv", show_col_types = FALSE),
         iso3 == iso3_c)$admin1_level

# areas <- read_sf("archive/bdi_data_areas/20201024-115335-68b89939/bdi_areas.geojson") %>%
#   mutate(iso3 = iso3_c)
# merge_cities <- read_sf("src/aaa_assign_populations/merge_cities.geojson") %>%
#   filter(iso3 == iso3_c)

# merge_cities <- read_sf("merge_cities.geojson") %>%
#   filter(iso3 == iso3_c)
# 
# merge_cities <- merge_cities %>%
#   filter(!tolower(area_name) %in% tolower(areas$area_name)) %>%
#   st_make_valid()
# 
# cities_areas <- merge_cities %>%
#   st_join(
#     areas %>% filter(area_level == admin1_lvl) %>% select(matched_province_area_id = area_id) %>% st_make_valid(),
#     largest = TRUE
#   ) %>%
#   st_drop_geometry() %>%
#   bind_rows(
#     areas %>%
#       select(area_id, area_name, area_level, geometry) %>%
#       st_make_valid() %>%
#       st_join(
#         areas %>% filter(area_level == admin1_lvl) %>% select(matched_province_area_id = area_id) %>% st_make_valid(),
#         largest = TRUE
#       ) %>%
#       st_drop_geometry() %>%
#       mutate(
#         matched_province_area_id = ifelse(area_level == 0, area_id, matched_province_area_id),
#         iso3 = iso3_c
#       )
#   )

# sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))

# prev_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Analytical datasets/key-populations/HIV prevalence", "prev_clean_sourced.csv")
# prev <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = prev_path)
prev <-
  read_csv("prev_clean_sourced.csv", show_col_types = FALSE) %>%
  rename(value = prop_estimate) %>%
  mutate(iso3 = countrycode(country.name, "country.name", "iso3c")) %>%
  filter(iso3 == iso3_c)
#
# prev <- read_csv("msm_tg.csv") %>%
#   filter(iso3 == iso3_c) %>%
#   mutate(kp = "MSM") %>%
#   rename(value = estimate_MSM) %>%
#   select(-idx) %>%
#   ungroup()

# art_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Analytical datasets/key-populations/ART coverage", "art_clean_sourced.csv")
# art <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = art_path)
art <- read_csv("art_clean_sourced.csv", show_col_types = FALSE) %>%
  rename(value = prop_estimate) %>%
  mutate(iso3 = countrycode(country.name, "country.name", "iso3c")) %>%
  left_join(naomi::get_age_groups() %>% select(age_group_label, age_group)) %>%
  select(-age_group_label) %>%
  filter(iso3 == iso3_c)

dat <- list("prev" = prev, "art" = art)

## ERI has odd age group aggregates
if(iso3 == "ERI")
  dat$prev <- dat$prev %>%
  filter(is.na(age_group) | age_group == "Y015_049")

# dat <- list("prev" = prev)
# #
x <- prev

matched_province_dat <- lapply(dat, function(x) {
  indicator <- unique(x$indicator)
  
  x <- x %>%
    mutate(area_name = case_when(
             iso3 == "BEN" & area_name == "Bourgou" ~ "Borgou",
             
             iso3 == "BFA" & area_name == "Sanmentenga" ~  "Sanmatenga",
             
             iso3 == "COD" & area_name == "Bas Congo" ~ "Kongo Central",
             iso3 == "COD" & area_name == "Katanga" ~ "Tanganyika; Haut-Lomami; Lualaba; Haut-Katanga",
             iso3 == "COD" & area_name %in% c("Oriental", "Orientale") ~ "Ituri; Haut-Uele; Tshopo; Bas-Uele",
             iso3 == "COD" & area_name == "Centre Hospitalaire Saint Hilaire" ~ "Kinshasa",
             
             iso3 == "ETH" & area_name == "Adama" ~ "Adama Town",
             
             iso3 == "GHA" & area_name == "Nothern" ~ "Northern",
             
             iso3 == "GNB" & area_name == "Guinea-Bissau" ~ "Guinea Bissau",
             
             iso3 == "KEN" & area_name == "Dagoretti" ~ "Dagoretti South; Dagoretti North",
             iso3 == "KEN" & area_name == "Embakasi" ~ "Embakasi South; Embakasi North; Embakasi West; Embakasi East; Embakasi Central",
             iso3 == "KEN" & area_name == "Mavoko" ~ "Athiriver",
             iso3 == "KEN" & area_name == "Nairobi" & study_idx == 124 ~ "Nairobi (County)",
             iso3 == "KEN" & area_name == "Tharaka" & study_idx == 124 ~ "Tharaka-Nithi",
             
             iso3 == "NAM" & area_name == "Khomas region" ~ "Khomas",
             
             iso3 == "NGA" & area_name == "Nassarawa" ~ "Nasarawa",
             
             iso3 == "UGA" & area_name == "Nkono" & study_idx == 234 & value == 0.186 ~ "Nkono (Kisoro)",
             iso3 == "UGA" & area_name == "Nkono" & study_idx == 234 & value == 0.133 ~ "Nkono (Iganga)",

             TRUE ~ area_name
           )) %>%
    mutate(
      area_name = str_replace_all(area_name, "\\,|\\/| and ", "\\;"),
      area_name = str_replace_all(area_name, "\\;\\;", "\\;"),
      area_name = str_remove_all(area_name, "\n")
    ) %>%
    distinct() %>%
    select(indicator, iso3, area_name, year, kp, row_id, study_idx) %>%
    separate(
      area_name,
      sep = ";",
      into = paste0("area_split", 1:20),
      remove = FALSE
    ) %>%
    mutate(across(starts_with("area_split"), ~ str_trim(.x))) %>%
    pivot_longer(-c(indicator, iso3, area_name, year, row_id, study_idx, kp)) %>%
    filter(!is.na(value)) %>%
    mutate(idx = row_number(),
           value = tolower(value))
  
  if (nrow(x)) {
    
    # bounding_box <- data.frame(idx = integer(),
    #                            area_id = character(),
    #                            geometry = st_sfc()) %>%
    #   st_as_sf(crs = 4326)
    
    naomi_matched <- x %>%
      distinct(indicator, iso3, value, year, kp, row_id, idx, study_idx) %>%
      left_join(areas %>% 
                  select(area_name, area_id, area_level) %>% 
                  mutate(area_name = tolower(area_name)),
                by = c("value" = "area_name")) %>%
      mutate(matched_area_name = value) %>%
      rename(matched_area_id = area_id) %>%
      group_by(idx) %>%
      filter(area_level == max(area_level)) %>%
      ungroup() %>%
      select(-area_level)
    
    nat_val <- naomi_matched %>%
      filter(matched_area_id == iso3_c) %>%
      mutate(matched_provincial_area_id = iso3_c)
    
    naomi_matched <- naomi_matched %>%
      filter(matched_area_id != iso3_c) %>%
      st_as_sf() %>%
      st_join(areas %>% filter(area_level == admin1_lvl) %>% select(matched_provincial_area_id = area_id), largest = T) %>%
      bind_rows(nat_val)
      
    google_df <- x %>%
      filter(!idx %in% naomi_matched$idx) %>%
      mutate(google_search = paste0(
        value,
        " ",
        countrycode(
          iso3,
          "iso3c",
          "country.name",
          custom_match = c("COD" = "DRC",
                           "COG" = "Republic of the Congo")
        )
      ))
    
    print_and_capture <- function(x)
    {
      class(x) <- "data.frame"
      paste(capture.output(print(x)), collapse = "\n")
    }
    
    if (nrow(google_df)) {
      tst <- Map(function(x, idx) {
        output <- geocode(x, output = "all")
        
        name <- output$results %>%
          lapply("[[", "address_components") %>%
          lapply(function(x)
            lapply(x, "[[", "long_name")) %>%
          lapply(unlist) %>%
          lapply(paste, collapse = ", ") %>%
          unlist()
        
        
        geometry <- lapply(output$results, function(x) {
          df <-
            data.frame(x$geometry$location) %>% sf::st_as_sf(coords = c("lng", "lat"), crs = 4326)
        }) %>%
          bind_rows()
        
        data.frame(google_search = x,
                   value = name,
                   idx = idx) %>%
          bind_cols(geometry)
        
      }, google_df$google_search, google_df$idx)
      
      
      first_cut <- tst %>%
        bind_rows() %>%
        left_join(google_df %>% select(study_idx, row_id, idx)) %>%
        mutate(value = str_replace(value, "'", "’")) %>%
        filter(
          str_detect(
            value,
            countrycode(
              iso3_c,
              "iso3c",
              "country.name",
              custom_match = c("COD" = "Democratic Republic of the Congo",
                               "COG" = "Republic of the Congo")
            )
          ),
          value != countrycode(
            iso3_c,
            "iso3c",
            "country.name",
            custom_match = c("COD" = "Democratic Republic of the Congo",
                             "COG" = "Republic of the Congo")
          )
        ) %>%
        st_as_sf() %>%
        st_join(areas %>% filter(area_level == admin1_lvl) %>% select(area_id))
      
      ## Google geocoded point is in water outside Naomi boundaries. Handcode them.
      first_cut$area_id[first_cut$google_search == "kitobo Uganda"] <- "UGA_1_09"
      
      
      ## Miscoded
      first_cut$area_id[first_cut$google_search == "central division Uganda"] <- "UGA_1_06"
      
      if (nrow(filter(first_cut, is.na(area_id)))) {
        nearest_join <- first_cut %>%
          filter(is.na(area_id)) %>%
          select(-area_id) %>%
          st_join(areas %>% filter(area_level == admin1_lvl) %>% select(area_id),
                  join = st_nearest_feature)
        
        warning(print_and_capture(nearest_join %>% rename(nearest_area_id = area_id)))
        
        ## Check plot for NA areas
        na_area_plot <- filter(areas, area_level == admin1_lvl) %>%
          ggplot() +
          geom_sf(aes(fill = area_id)) +
          geom_sf_text(aes(label = area_id)) +
          geom_sf(data = nearest_join) +
          ggrepel::geom_label_repel(
            data = filter(first_cut, is.na(area_id)),
            aes(label = google_search, geometry = geometry),
            stat = "sf_coordinates",
            min.segment.length = 0
          )
        
        dir.create("check")
        pdf(
          paste0("check/na_area_plot_", indicator, ".pdf"),
          width = 12,
          height = 12
        )
        print(na_area_plot)
        dev.off()
        
        first_cut <- first_cut %>%
          filter(!is.na(area_id)) %>%
          bind_rows(nearest_join)
        
        if (nrow(filter(first_cut, is.na(area_id))))
          stop(print_and_capture(filter(first_cut, is.na(area_id))))
        
      }
      
      multiple_province_matches <- first_cut %>%
        distinct(idx, area_id) %>%
        count(idx) %>%
        filter(n > 1) %>%
        pull(idx)
      
      # Multiple matches resolved using the study reports
      first_cut <- first_cut %>%
        filter(case_when(
          google_search == "kitebere Uganda" & study_idx == 234 ~ area_id == "UGA_1_04",
          google_search == "rwentuha Uganda" & study_idx == 234 ~ area_id == "UGA_1_02",
          google_search == "ehlanzeni South Africa" & study_idx == 242 ~ area_id == "ZAF_1_MP",
          google_search == "ebibeyin Equatorial Guinea" & study_idx == 99 ~ area_id == "GNQ_2_02lh",
          TRUE ~ T)
        )
      
      multiple_province_matches <- first_cut %>%
        distinct(idx, area_id) %>%
        count(idx) %>%
        filter(n > 1) %>%
        pull(idx)
      
      err_df <- first_cut %>%
        filter(idx %in% multiple_province_matches)
      
      
      if (nrow(err_df))
        stop(print_and_capture(err_df))
      
      first_cut <- first_cut %>%
        group_by(idx) %>%
        mutate(n = row_number()) %>%
        filter(n == 1) %>%
        select(-n) %>%
        ungroup()
      
      no_google_hit <- tst %>%
        bind_rows() %>%
        filter(!idx %in% first_cut$idx) %>%
        distinct(idx, google_search)
      
      if (nrow(no_google_hit)) {
        
        opq <- lapply(no_google_hit$google_search, function(x) try(opq(x)))
        names(opq) <- no_google_hit$idx
        
        fail_search <- lapply(opq, function(x) {
          any(class(x) == "try-error")
        }) %>% 
          unlist() %>%
          grep(T, .)
        
        opq[fail_search] <- NULL
        
        coords <- opq %>%
          lapply(function(x) {
            coords <- as.numeric(unlist(str_split(x$bbox, ",")))
            geometry <- data.frame(lon = c(coords[2], coords[4]),
                                   lat = c(coords[1], coords[3])) %>%
              st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
              st_bbox() %>%
              st_as_sfc()
            
            data.frame(geometry = geometry)
          })
        
        if(length(coords)) {
          bounding_box <- coords %>%
            bind_rows(.id = "idx") %>%
            st_as_sf() %>%
            st_join(areas %>% filter(area_level == admin1_lvl) %>% select(area_id)) %>%
            filter(!is.na(area_id)) %>%
            rename(matched_provincial_area_id = area_id,
                   matched_point = geometry) %>%
            mutate(idx = as.numeric(idx))
          
          bounding_box <- x %>%
            filter(idx %in% bounding_box$idx) %>%
            left_join(bounding_box)
          
          multiple_province_matches <- bounding_box %>%
            distinct(idx, matched_provincial_area_id) %>%
            count(idx) %>%
            filter(n > 1) %>%
            pull(idx)
          
          # Multiple matches resolved using the study reports
          bounding_box <- bounding_box %>%
            filter(case_when(
              value == "malaba" & study_idx == 234 ~ matched_provincial_area_id == "UGA_1_03",
              TRUE ~ T)
            )
          
          multiple_province_matches <- bounding_box %>%
            distinct(idx, matched_provincial_area_id) %>%
            count(idx) %>%
            filter(n > 1) %>%
            pull(idx)
          
          err_df <- bounding_box %>%
            filter(idx %in% multiple_province_matches)
          
          
          if (nrow(err_df))
            stop(print_and_capture(err_df))
          
        } else {
          message("Wiped bounding box 1")
          bounding_box <- data.frame(idx = integer(),
                                     matched_provincial_area_id = character(),
                                     matched_point = character())
        }
        
        
      } else {
        message("Wiped bounding box 2")
        bounding_box <- data.frame(idx = integer(),
                                   matched_provincial_area_id = character(),
                                   matched_point = character())
      }
      
      first_cut$geometry <- as.character(first_cut$geometry)
      class(first_cut) <- "data.frame"
      
      first_cut <- x %>%
        filter(idx %in% first_cut$idx) %>%
        left_join(first_cut %>% 
                    select(idx, study_idx, matched_area_name = value, matched_provincial_area_id = area_id,
                           matched_point = geometry)
        ) %>%
        select(any_of(c(colnames(naomi_matched), "matched_point")))
        
    } else {
      
      message("Wiped bounding box 3")
      bounding_box <- first_cut <- data.frame(idx = integer(),
                                 matched_provincial_area_id = character(),
                                 matched_point = character())
        
      }
      
    # } else {
    #   bounding_box <- data.frame(idx = integer(),
    #                              area_id = character(),
    #                              geometry = character())
    # 
    #   # first_cut <- bounding_box <- data.frame(idx = integer(),
    #   #                                         area_id = character(),
    #   #                                         value = character(),
    #   #                                         geometry = character())
    # }
    
    
    # nat_vals <-  x %>%
    #   distinct(iso3, value, year, kp, row_id, idx) %>%
    #   filter(value == tolower(countrycode(iso3_c, "iso3c", "country.name"))) %>%
    #   mutate(area_id = iso3_c,
    #          google_search = tolower(countrycode(iso3_c, "iso3c", "country.name")))
    # # select(any_of(colnames(first_cut)))
    # 
    # if (nrow(nat_vals))
    #   first_cut <- bind_rows(nat_vals, first_cut)
    
    bounding_box$matched_point <- as.character(bounding_box$matched_point)
    class(bounding_box) <- "data.frame"
    
    total_match <- naomi_matched %>%
      mutate(source = "Naomi") %>%
      st_drop_geometry() %>%
      bind_rows(first_cut %>% mutate(source = "Google"),
                bounding_box %>% mutate(source = "OSM")) %>%
      arrange(idx)
    
    if(iso3 == "UGA" & indicator == "prevalence")
      total_match <- total_match %>%
        bind_rows(
          x %>%
            filter(!idx %in% total_match$idx) %>%
            mutate(matched_provincial_area_id = case_when(
              value == "akwang" & study_idx == 234 ~ "UGA_1_04",
              value == "idudi" & study_idx == 234 ~ "UGA_1_02",
              value == "kamuwunga" & study_idx == 234 ~ "UGA_1_09",
              value == "katuna" & study_idx == 234 ~ "UGA_1_02",
              value == "lambu" & study_idx == 234 ~ "UGA_1_09",
              value == "malaba" & study_idx == 234 ~ "UGA_1_03",
              value == "migeera" & study_idx == 234 ~ "UGA_1_08",
              value == "nyeihanga" & study_idx == 234 ~ "UGA_1_02",
              value == "ssenyondo" & study_idx == 234 ~ "UGA_1_09",
              value == "kakonde-bakijulula tea estate" & study_idx == 234 ~ "UGA_1_08",
              value == "kya-muhunga tea estate" & study_idx == 234 ~ "UGA_1_02",
              TRUE ~ NA
              ),
              source = "Handmatched using study report"
            ) %>%
            filter(!is.na(matched_provincial_area_id))
        )
    
    no_match <- x %>%
      filter(!idx %in% total_match$idx) %>%
      select(row_id, idx, value)
    
    ## In cases where 1 line has multiple areas, and only some of them are matched, set the provincial area ID for that row ID to NA.
    x <- x %>%
      left_join(total_match %>% 
                  select(row_id, idx, starts_with("match"), source)) %>%
      group_by(row_id) %>%
      mutate(matched_provincial_area_id = ifelse(any(is.na(matched_provincial_area_id)), NA, matched_provincial_area_id))
    
    total_match <- x %>% 
      filter(!is.na(matched_provincial_area_id)) %>%
      select(any_of(colnames(total_match)))
    
    
    if (nrow(no_match)) {
      warning("\nString match is bad:\n",
              paste0(utils::capture.output(no_match), collapse = "\n"))
      
      
    } else {
      no_match <- data.frame()
    }
    
    
  } else {
    total_match <- data.frame()
    no_match = data.frame()
  }
  
  message(indicator)
  
  out <- list("assigned_province" = total_match, "no_match" = no_match)
  
})

write_csv(matched_province_dat$prev$no_match, "prev_bad_match_error.csv", na = "")
write_csv(matched_province_dat$prev$assigned_province,
          "prev_assigned_province.csv",
          na = "")

write_csv(matched_province_dat$art$no_match, "art_bad_match_error.csv", na = "")
write_csv(matched_province_dat$art$assigned_province, "art_assigned_province.csv", na = "")


if(nrow(dat$prev)) {
  if(nrow(matched_province_dat$prev$assigned_province))
    dat$prev <- dat$prev %>% 
      left_join(matched_province_dat$prev$assigned_province %>% select(study_idx, row_id, matched_provincial_area_id)) %>%
      mutate(matched_provincial_area_id = ifelse(is.na(matched_provincial_area_id), iso3_c, matched_provincial_area_id),
             indicator = "HIV prevalence")
  else
    dat$prev <- dat$prev %>% 
      mutate(matched_provincial_area_id = iso3_c,
             indicator = "HIV prevalence")
} else {
  dat$prev <- data.frame()
}


if(nrow(dat$art)) {
  if(nrow(matched_province_dat$art$assigned_province))
    dat$art <- dat$art %>% 
      left_join(matched_province_dat$art$assigned_province %>% select(study_idx, row_id, matched_provincial_area_id)) %>%
      mutate(matched_provincial_area_id = ifelse(is.na(matched_provincial_area_id), iso3_c, matched_provincial_area_id),
             indicator = "ART coverage")
  else
    dat$art <- dat$art %>% 
      mutate(matched_provincial_area_id = iso3_c,
             indicator = "ART coverage")
} else {
  dat$art <- data.frame()
}

spectrum <- extract_pjnz_naomi("depends/naomi_pjnz.zip")

spectrum <- spectrum %>%
  filter(age %in% 15:49, year > 1999)

spectrum_ratio <- spectrum %>%
  filter(age %in% 15:49, year > 1999) %>%
  select(iso3, year, age, sex, hivpop, totpop, artpop) %>%
  sex_aggregation(c("hivpop", "totpop", "artpop")) %>%
  # bind_rows(
  #   spectrum %>%
  #     group_by(year) %>%
  #     summarise(hivpop = sum(hivpop),
  #               totpop = sum(totpop),
  #               artpop = sum(artpop)) %>%
  #     mutate(sex = "both")
  # ) %>%
  group_by(sex, year) %>%
  summarise(prevalence = sum(hivpop)/sum(totpop),
            art_coverage = sum(artpop)/sum(hivpop),
            population = sum(totpop)) %>%
  pivot_longer(-c(sex, year), names_to = "indicator") %>%
  group_by(sex, indicator) %>%
  mutate(ratio = value/value[year == 2022],
         age_group = "Y015_049")

if(!iso3 %in% c("SSD", "ERI")) {
  
  indicators <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2023 naomi final", pattern = "zip", full.names = T) %>%
    grep(iso3, ., value = T)
  
  if(iso3 == "NGA")
    indicators <- list.files("~/Imperial College London/HIV Inference Group - WP - Documents/Data/Spectrum files/2022 naomi preliminary", pattern = "zip", full.names = T) %>%
      grep(iso3, ., value = T)
  
  tmpd <- tempfile()
  on.exit(unlink(tmpd))
  utils::unzip(indicators, exdir = tmpd)
  out <- list()
  out$indicators <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "indicators.csv"))
  out$meta_age_group <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "meta_age_group.csv"))
  out$meta_period <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "meta_period.csv"))
  out$meta_area <- read_sf(list.files(tmpd, full.names = TRUE, pattern = "boundaries.geojson"))
  out$meta_indicator <- read.csv(list.files(tmpd, full.names = TRUE, pattern = "meta_indicator.csv"))
  class(out) <- "naomi_output"
  
  indicators <- add_output_labels(out)
  
  time_point <- unique(indicators$calendar_quarter)[2]
  
  filtered_indicators <- indicators %>%
    filter(area_level <= admin1_lvl,
           indicator %in% c("population", "plhiv", "art_current_residents"),
           # age_group_label %in% c("15-19", "15+", "15-49", "15-24", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "25-49", "15-64"),
           calendar_quarter == time_point) %>%
    select(area_level:age_group_label, indicator, mean) %>%
    pivot_wider(names_from = indicator, values_from = mean) %>%
    ungroup()
  
  filtered_indicators <- bind_rows(filtered_indicators, 
                                   filtered_indicators %>%
                                     filter(age_group_label %in% c("15-24", "25-29")) %>%
                                     group_by(across(-c(age_group, age_group_label, plhiv, population, art_current_residents))) %>%
                                     summarise(plhiv = sum(plhiv),
                                               population = sum(population),
                                               art_current_residents = sum(art_current_residents)) %>%
                                     mutate(age_group_label = "15-29",
                                            age_group = "Y015_029"),
                                   filtered_indicators %>%
                                     filter(age_group_label %in% c("35-39", "40-44", "45-49")) %>%
                                     group_by(across(-c(age_group, age_group_label, plhiv, population, art_current_residents))) %>%
                                     summarise(plhiv = sum(plhiv),
                                               population = sum(population),
                                               art_current_residents = sum(art_current_residents)) %>%
                                     mutate(age_group_label = "35-49",
                                            age_group = "Y035_049"),
                                   filtered_indicators %>%
                                     filter(age_group_label %in% c("35-39", "40-44")) %>%
                                     group_by(across(-c(age_group, age_group_label, plhiv, population, art_current_residents))) %>%
                                     summarise(plhiv = sum(plhiv),
                                               population = sum(population),
                                               art_current_residents = sum(art_current_residents)) %>%
                                     mutate(age_group_label = "35-44",
                                            age_group = "Y035_044"),
                                   filtered_indicators %>%
                                     filter(age_group_label %in% c("30-34", "35-39", "40-44", "45-49")) %>%
                                     group_by(across(-c(age_group, age_group_label, plhiv, population, art_current_residents))) %>%
                                     summarise(plhiv = sum(plhiv),
                                               population = sum(population),
                                               art_current_residents = sum(art_current_residents)) %>%
                                     mutate(age_group_label = "30-49",
                                            age_group = "Y030_049")
  ) %>%
    ungroup() %>%
    mutate(prevalence = plhiv/population,
           art_coverage = art_current_residents/plhiv) %>%
    select(-c(plhiv, art_current_residents))
  
  dat <- lapply(dat, function(x) {
    if(nrow(x))
      x <- x %>%
        mutate(matched_provincial_area_id = case_when(
          matched_provincial_area_id == "CMR_1_3" ~ "CMR_1_5", # Douala merged into Littoral
          matched_provincial_area_id == "CMR_1_12" ~ "CMR_1_2", # Yaounde merged into Centre
          matched_provincial_area_id == "CMR_1_4" ~ "CMR_1_3", 
          matched_provincial_area_id == "CMR_1_5" ~ "CMR_1_4", 
          matched_provincial_area_id == "CMR_1_6" ~ "CMR_1_5", 
          matched_provincial_area_id == "CMR_1_7" ~ "CMR_1_6", 
          matched_provincial_area_id == "CMR_1_8" ~ "CMR_1_7", 
          matched_provincial_area_id == "CMR_1_9" ~ "CMR_1_8", 
          matched_provincial_area_id == "CMR_1_11" ~ "CMR_1_9",
          
          matched_provincial_area_id == "ZMB_1_10" ~ "ZMB_1_10gh",
          matched_provincial_area_id == "ZMB_1_17" ~ "ZMB_1_17xt",
          matched_provincial_area_id == "ZMB_1_12" ~ "ZMB_1_12sg",
          matched_provincial_area_id == "ZMB_1_14" ~ "ZMB_1_14fz",
          matched_provincial_area_id == "ZMB_1_15" ~ "ZMB_1_15pi",
          
          TRUE ~ matched_provincial_area_id
        ))
    else
      data.frame()
  })
  
  data_age_groups <- c(unique(dat$prev$age_group)[!is.na(unique(dat$prev$age_group))],
                       unique(dat$art$age_group)[!is.na(unique(dat$art$age_group))])
  naomi_age_groups <- unique(filtered_indicators$age_group)
  
  data_area_id <- c(unique(dat$prev$matched_provincial_area_id),
                    unique(dat$art$matched_provincial_area_id))
  naomi_area_id <- unique(filtered_indicators$area_id)
  
  if(length(data_age_groups[!data_age_groups %in% naomi_age_groups])>0)
    stop("Age groups in data not in Naomi")
  
  if(length(data_area_id[!data_area_id %in% naomi_area_id])>0)
    stop("Area IDs in data not in Naomi")
  
  filtered_indicators <- filtered_indicators %>%
    pivot_longer(cols = c(prevalence, population, art_coverage), names_to = "indicator", values_to = "mean")
  
  df <- spectrum_ratio %>%
    select(-age_group) %>%
    left_join(filtered_indicators, by=c("sex", "indicator")) %>%
    mutate(mean = ratio * mean,
           # lower = ratio * lower,
           # upper = ratio * upper,
           indicator = case_when(
             indicator == "prevalence" ~ "HIV prevalence",
             indicator == "art_coverage" ~ "ART coverage",
             indicator == "population" ~ "Population",
             TRUE ~ indicator)) %>%
    select(area_level:area_name,
           sex:indicator,
           age_group_label,
           age_group,
           # lower, 
           mean
           # upper
    )
  
  
  naomi_matched_dat <- lapply(dat, function(x) {
    
    # x <- dat$prev
    
    if(nrow(x)) {
      anonymised_dat <- x %>%
        select(!contains(c("Column", "..."))) %>%
        left_join(kp_to_sex() %>% mutate(sex = ifelse(kp == "PWID", "male", sex))) %>%
        # mutate(sex = case_when(
        #   kp %in% c("FSW", "SW", "TGW", "TG") ~ "female",
        #   kp %in% c("MSM", "TGM") ~ "male",
        #   kp == "PWID" ~ "both"
        # ),
        mutate(
          has_age = ifelse(!is.na(age_group), 1, 0),
          age_group = case_when(
            # (is.na(age_group) | !age_group %in% unique(filtered_indicators$age_group)) & !kp %in% c("TG", "TGW", "MSM") ~ "Y015_049",
            # (is.na(age_group) | !age_group %in% unique(filtered_indicators$age_group)) & kp %in% c("TG", "TGW", "MSM") ~ "Y015_029",
            is.na(age_group) | !age_group %in% unique(filtered_indicators$age_group) ~ "Y015_049",
            TRUE ~ as.character(age_group)
          )) %>%
        left_join(df %>% select(indicator, matched_provincial_area_id = area_id, sex, year, age_group, mean)) %>%
        group_by(row_id) %>%
        mutate(mean = median(mean)) %>%
        mutate(
          model_matched_provincial_area_id = ifelse(length(unique(matched_provincial_area_id)) > 1, iso3_c, matched_provincial_area_id),
          model_matched_provincial_area_id = ifelse(length(unique(matched_provincial_area_id)) > 1, countrycode::countrycode(iso3_c, "iso3c", "country.name"), matched_provincial_area_id),
          rn = paste0("split_", row_number())) %>%
        # group_by(row_id, matched_provincial_area_id) %>%
        # mutate(mean = median(mean)) %>%
        pivot_wider(names_from = rn, values_from = matched_provincial_area_id) %>%
        unite("all_matched_provincial_area_id", starts_with("split"), sep = "; ") %>%
        mutate(all_matched_provincial_area_id = str_remove_all(all_matched_provincial_area_id, "; NA|NA; ")) %>%
        rename(provincial_value = mean) %>%
        mutate(ratio = value/provincial_value) %>%
        ungroup() %>%
        select(-province) %>%
        left_join(areas %>% select(area_id, province = area_name) %>% st_drop_geometry(), by = c("model_matched_provincial_area_id" = "area_id"))
      # pivot_wider(names_from = rn, values_from = c(area_id, matched_area_name)) %>%
      #   unite("area_id", starts_with("area_id_split"), sep = "; ") %>%
      #   unite("matched_area_name", starts_with("matched_area_name_split"), sep = "; ") %>%
      #   mutate(area_id = str_remove_all(area_id, "; NA|NA; "),
      #          matched_area_name = str_remove_all(matched_area_name, "; NA|NA; ")) %>%
      #   rename(provincial_value = mean) %>%
      #   mutate(ratio = value/provincial_value) %>%
      #   ungroup()
    } else {
      data.frame()
    }
    
  })
  
  area_indicators <- indicators %>%
    filter(age_group == "Y015_049",
           indicator %in% c("prevalence", "art_coverage"),
           area_level %in% c(0, admin1_lvl),
           calendar_quarter == unique(indicators$calendar_quarter)[2]) %>%
    select(area_id, sex, indicator, mean)
  
} else {
  
  dat$prev$matched_provincial_area_id <- iso3_c
  dat$art$matched_provincial_area_id <- iso3_c
  
  df <- spectrum_ratio %>%
    mutate(indicator = case_when(
      indicator == "prevalence" ~ "HIV prevalence",
      indicator == "art_coverage" ~ "ART coverage",
      indicator == "population" ~ "Population",
      TRUE ~ indicator)) %>%
    rename(provincial_value = value) %>%
    select(-age_group)
  
  naomi_matched_dat <- lapply(dat, function(x) {
    
    if(nrow(x)) {
      anonymised_dat <- x %>%
        select(!contains(c("Column", "..."))) %>%
        mutate(sex = case_when(
          kp %in% c("FSW", "SW", "TGW", "TG") ~ "female",
          kp %in% c("MSM", "TGM", "PWID") ~ "male",
          # kp == "PWID" ~ "both"
        ),
        has_age = ifelse(!is.na(age_group), 1, 0),
        age_group = case_when(
          # is.na(age_group) | !kp %in% c("TG", "TGW", "MSM") ~ "Y015_049",
          # is.na(age_group) | kp %in% c("TG", "TGW", "MSM") ~ "Y015_029",
          is.na(age_group) ~ "Y015_049",
          TRUE ~ as.character(age_group))
        ) %>%
        left_join(df %>% select(indicator, sex, year, mean = provincial_value)) %>%
        group_by(row_id) %>%
        mutate(mean = median(mean)) %>%
        mutate(
          model_matched_provincial_area_id = ifelse(length(unique(matched_provincial_area_id)) > 1, iso3_c, matched_provincial_area_id),
          model_matched_provincial_area_id = ifelse(length(unique(matched_provincial_area_id)) > 1, countrycode::countrycode(iso3_c, "iso3c", "country.name"), matched_provincial_area_id),
          rn = paste0("split_", row_number())) %>%
        # group_by(row_id, matched_provincial_area_id) %>%
        # mutate(mean = median(mean)) %>%
        pivot_wider(names_from = rn, values_from = matched_provincial_area_id) %>%
        unite("matched_provincial_area_id", starts_with("split"), sep = "; ") %>%
        mutate(matched_provincial_area_id = str_remove_all(matched_provincial_area_id, "; NA|NA; ")) %>%
        rename(provincial_value = mean) %>%
        mutate(ratio = value/provincial_value) %>%
        ungroup() %>%
        select(-province) %>%
        left_join(areas %>% select(area_id, province = area_name) %>% st_drop_geometry(), by = c("matched_provincial_area_id" = "area_id"))
    } else {
      data.frame()
    }
    
  })
  
  area_indicators <- data.frame()
}


if(nrow(naomi_matched_dat$prev)) {
  out_prev_model <- naomi_matched_dat$prev %>%
    filter(across(any_of("is_aggregate"), is.na)) %>%
    select(iso3, area_id = model_matched_provincial_area_id, year, kp, age_group, method, denominator, has_age, value, provincial_value, ratio, study_idx)
} else {
  out_prev_model <- data.frame() 
}

if(nrow(naomi_matched_dat$art)) {
  out_art_model <- naomi_matched_dat$art %>%
    filter(across(any_of("is_aggregate"), is.na)) %>%
    select(iso3, area_id = model_matched_provincial_area_id, year, kp, method, age_group, has_age, value, denominator, provincial_value, ratio, study_idx)
} else {
  out_art_model <- data.frame() 
}

write_csv(df, "extrapolated_naomi.csv", na = "")
write_csv(out_prev_model, "prev.csv", na = "")
write_csv(out_art_model, "art.csv", na = "")
write_csv(area_indicators, "area_indicators.csv", na = "")

if(nrow(dat$prev)) {
  workbook_export_prev <- dat$prev %>%
    select(-any_of("province")) %>%
    left_join(areas %>% select(area_id, province = area_name) %>% st_drop_geometry(), by = c("matched_provincial_area_id" = "area_id")) %>%
    distinct(iso3, data_checked, country.name, method, kp, area_name, province, year, prop_lower, value, prop_upper, denominator, ref, link) %>%
    mutate(indicator = "HIV prevalence") %>%
    rename(prop_estimate = value) %>%
    mutate(
      surveillance_type = NA,
      count_lower = NA,
      count_estimate = NA,
      count_upper = NA,
      population = NA,
      sex= case_when(
        kp %in%  c("FSW", "TG", "TGW") ~ "female",
        kp %in% c("MSM", "PWID") ~ "male",
        # kp == "PWID" ~ "both"
      ),
      age_group = NA,
      notes = NA,
      link = NA,
      prop_lower = round(prop_lower, 3),
      prop_estimate = round(prop_estimate, 3),
      prop_upper = round(prop_upper, 3)
    ) %>%
    select(all_of(c("country.name", "data_checked", "surveillance_type", "indicator", "method", "kp", "sex", "age_group", "area_name", "province", "year", "count_lower", "count_estimate", "count_upper", "population", "prop_lower", "prop_estimate", "prop_upper", "denominator", "notes", "ref", "link")))
} else {
  workbook_export_prev <- data.frame()
}

if(nrow(dat$art)) {
  workbook_export_art <- dat$art %>%
    select(-any_of("province")) %>%
    left_join(areas %>% select(area_id, province = area_name) %>% st_drop_geometry(), by = c("matched_provincial_area_id" = "area_id")) %>%
    ungroup() %>%
    distinct(iso3, data_checked, country.name, method, kp, area_name, province, year, prop_lower, value, prop_upper, denominator, ref, link) %>%
    mutate(indicator = "ART coverage") %>%
    rename(prop_estimate = value) %>%
    mutate(
      surveillance_type = NA,
      count_lower = NA,
      count_estimate = NA,
      count_upper = NA,
      population = NA,
      sex= case_when(
        kp %in%  c("FSW", "TG", "TGW") ~ "female",
        kp %in% c("MSM", "PWID") ~ "male",
        # kp == "PWID" ~ "both"
      ),
      age_group = NA,
      notes = NA,
      link = NA,
      prop_lower = round(prop_lower, 3),
      prop_estimate = round(prop_estimate, 3),
      prop_upper = round(prop_upper, 3)
    ) %>%
    select(all_of(c("country.name", "data_checked", "surveillance_type", "indicator", "method", "kp", "sex", "age_group", "area_name", "province", "year", "count_lower", "count_estimate", "count_upper", "population", "prop_lower", "prop_estimate", "prop_upper", "denominator", "notes", "ref", "link")))
} else {
  workbook_export_art <- data.frame()
}


write_csv(workbook_export_prev, "workbook_export_prev.csv", na = "")
write_csv(workbook_export_art, "workbook_export_art.csv", na = "")
# 
# naomi_matched_dat$prev %>%
#   select(iso3:year, starts_with("prop"), prop_estimate = value, study_idx, row_id) %>%
#   left_join(matched_province_dat$prev$assigned_province %>% select(-indicator)) %>% View
# 
# if(nrow(naomi_matched_dat$prev)) {
#   out_prev_data_sharing <- naomi_matched_dat$prev %>%
#     mutate(age_group_analysis = age_group,
#            age_group = ifelse(has_age == 1, age_group, NA)) %>%
#     select(study_idx, 
#            country = country.name,
#            kp,
#            year,
#            indicator,
#            method,
#            area_name,
#            matched_area_name,
#            area_id,
#            matched_province = province,
#            matched_province_area_id, 
#            age_group,
#            age_group_analysis,
#            proportion_estimate = value,
#            proportion_lower = prop_lower,
#            proportion_upper = prop_upper,
#            sample_size = denominator,
#            provincial_value,
#            ratio)
#   
# } else {
#   out_prev_data_sharing <- data.frame() 
# }
# 
# if(nrow(naomi_matched_dat$art)) {
#   out_art_data_sharing <- naomi_matched_dat$art %>%
#     mutate(age_group_analysis = age_group,
#            age_group = ifelse(has_age == 1, age_group, NA)) %>%
#     select(study_idx, 
#            country = country.name,
#            kp,
#            year,
#            indicator,
#            method,
#            area_name,
#            matched_area_name,
#            area_id,
#            matched_province = province,
#            matched_province_area_id, 
#            age_group,
#            age_group_analysis,
#            proportion_estimate = value,
#            proportion_lower = prop_lower,
#            proportion_upper = prop_upper,
#            sample_size = denominator,
#            provincial_value,
#            ratio)
# } else {
#   out_art_data_sharing <- data.frame() 
# }
# 
# write_csv(out_prev_data_sharing, "data_sharing_prev.csv", na = "")
# write_csv(out_art_data_sharing, "data_sharing_art.csv", na = "")
