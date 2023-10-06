iso3_c <- iso3

areas <- read_sf("depends/naomi_areas.geojson") %>%
  mutate(iso3 = iso3_c)

admin1_lvl <-
  filter(read_csv("resources/iso_mapping_fit.csv", show_col_types = FALSE),
         iso3 == iso3_c)$admin1_level

# areas <- read_sf("archive/bdi_data_areas/20201024-115335-68b89939/bdi_areas.geojson") %>%
#   mutate(iso3 = iso3_c)
# merge_cities <- read_sf("src/aaa_assign_populations/merge_cities.geojson") %>%
#   filter(iso3 == iso3_c)

merge_cities <- read_sf("merge_cities.geojson") %>%
  filter(iso3 == iso3_c)

merge_cities <- merge_cities %>%
  filter(!tolower(area_name) %in% tolower(areas$area_name)) %>%
  st_make_valid()

cities_areas <- merge_cities %>%
  st_join(
    areas %>% filter(area_level == admin1_lvl) %>% select(matched_province_area_id = area_id) %>% st_make_valid(),
    largest = TRUE
  ) %>%
  st_drop_geometry() %>%
  bind_rows(
    areas %>%
      select(area_id, area_name, area_level, geometry) %>%
      st_make_valid() %>%
      st_join(
        areas %>% filter(area_level == admin1_lvl) %>% select(matched_province_area_id = area_id) %>% st_make_valid(),
        largest = TRUE
      ) %>%
      st_drop_geometry() %>%
      mutate(
        matched_province_area_id = ifelse(area_level == 0, area_id, matched_province_area_id),
        iso3 = iso3_c
      )
  )

# sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))

# prev_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Analytical datasets/key-populations/HIV prevalence", "prev_clean_sourced.csv")
# prev <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = prev_path)
prev <-
  read_csv("prev_clean_sourced.csv", show_col_types = FALSE) %>%
  rename(value = prop_estimate) %>%
  mutate(iso3 = countrycode(country.name, "country.name", "iso3c"))
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
  select(-age_group_label)

dat <- list("prev" = prev, "art" = art)

# dat <- list("prev" = prev)
# #
x <- art

out <- lapply(dat, function(x) {
  indicator <- unique(x$indicator)
  
  x <- x %>%
    mutate(row_id = row_number(),
           area_name = case_when(
             iso3 == "BFA" & area_name == "Sanmentenga" ~  "Sanmatenga",
             
             iso3 == "COD" & area_name == "Bas Congo" ~ "Kongo Central",
             iso3 == "COD" & area_name == "Katanga" ~ "Tanganyika; Haut-Lomami; Lualaba; Haut-Katanga",
             iso3 == "COD" & area_name %in% c("Oriental", "Orientale") ~ "Ituri; Haut-Uele; Tshopo; Bas-Uele",
             iso3 == "COD" & area_name == "Centre Hospitalaire Saint Hilaire" ~ "Kinshasa",
             
             iso3 == "ETH" & area_name == "Adama" ~ "Adama Town",
             
             iso3 == "KEN" & area_name == "Dagoretti" ~ "Dagoretti South; Dagoretti North",
             iso3 == "KEN" & area_name == "Embakasi" ~ "Embakasi South; Embakasi North; Embakasi West; Embakasi East; Embakasi Central",
             iso3 == "KEN" & area_name == "Mavoko" ~ "Athiriver",
             iso3 == "KEN" & area_name == "Nairobi" & study_idx == 124 ~ "Nairobi (County)",
             iso3 == "KEN" & area_name == "Tharaka" & study_idx == 124 ~ "Tharaka-Nithi",
             
             iso3 == "NAM" & area_name == "Khomas region" ~ "Khomas",
             
             
             TRUE ~ area_name
           )) %>%
    filter(iso3 == iso3_c) %>%
    select(iso3, area_name, year, kp, row_id, study_idx) %>%
    mutate(
      area_name = str_replace_all(area_name, "\\,|\\/| and ", "\\;"),
      area_name = str_replace_all(area_name, "\\;\\;", "\\;"),
      area_name = str_remove_all(area_name, "\n")
    ) %>%
    distinct() %>%
    separate(
      area_name,
      sep = ";",
      into = paste0("area_split", 1:20),
      remove = FALSE
    ) %>%
    mutate(across(starts_with("area_split"), ~ str_trim(.x))) %>%
    pivot_longer(-c(iso3, area_name, year, row_id, study_idx, kp)) %>%
    filter(!is.na(value)) %>%
    mutate(idx = row_number(),
           value = tolower(value))
  
  if (nrow(x)) {
    google_df <- x %>%
      distinct(iso3, value, year, kp, row_id, idx, study_idx) %>%
      filter(value != tolower(countrycode(iso3_c, "iso3c", "country.name"))) %>%
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
        left_join(google_df %>% select(study_idx, idx)) %>%
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
      
      # Several possible locations for Kitebere, Uganda
      first_cut <- first_cut %>%
        filter(google_search != "kitebere Uganda")
      
      # Study 242 sampled the district in MP province
      first_cut <- first_cut %>%
        filter(case_when(
          google_search == "ehlanzeni South Africa" & study_idx == 242 ~ area_id == "ZAF_1_MP",
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
        possibly_opq <- purrr::possibly(opq)
        
        opq <- map(no_google_hit$google_search, ~ possibly_opq(.x))
        
        names(opq) <- no_google_hit$idx
        
        coords <- opq %>%
          compact() %>%
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
            st_join(areas %>% filter(area_level == 1)) %>%
            filter(!is.na(area_id)) %>%
            mutate(idx = as.numeric(idx))
        } else {
          bounding_box <- data.frame(idx = integer(),
                                     area_id = character(),
                                     geometry = character())
        }
        
        
      }
        
      } else {
        bounding_box <- data.frame(idx = integer(),
                                   area_id = character(),
                                   geometry = character())
        
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
    
    
    nat_vals <-  x %>%
      distinct(iso3, value, year, kp, row_id, idx) %>%
      filter(value == tolower(countrycode(iso3_c, "iso3c", "country.name"))) %>%
      mutate(area_id = iso3_c,
             google_search = tolower(countrycode(iso3_c, "iso3c", "country.name")))
    # select(any_of(colnames(first_cut)))
    
    if (nrow(nat_vals))
      first_cut <- bind_rows(nat_vals, first_cut)
    
    
    ## Google geocoded points are in water outside Naomi boundaries. Handcode them.
    # first_cut$area_id[first_cut$google_search == "kitobo Uganda"] <- "UGA_1_09"
    # first_cut$area_id[first_cut$google_search == "pakwach Uganda"] <- "UGA_1_10"
    
    
    no_match <- x %>%
      filter(!idx %in% first_cut$idx,!idx %in% bounding_box$idx) %>%
      select(row_id, idx, area_name)
    
    if (nrow(no_match)) {
      warning("\nString match is bad:\n",
              paste0(utils::capture.output(no_match), collapse = "\n"))
      
      
    } else {
      no_match <- data.frame()
    }
    
    bounding_box$geometry <- as.character(bounding_box$geometry)
    class(bounding_box) <- "data.frame"
    first_cut$geometry <- as.character(first_cut$geometry)
    class(first_cut) <- "data.frame"
    
    
    x <- x %>%
      left_join(
        bind_rows(
          bounding_box %>% select(idx, area_id, geometry) %>% mutate(source = "OSM"),
          first_cut %>% select(idx, value, area_id, any_of("geometry")) %>% mutate(source = "Google")
          
        ) %>%
          arrange(idx) %>%
          rename(
            matched_area_name = value,
            matched_point = geometry,
            matched_provincial_area_id = area_id
          )
      ) %>%
      mutate(matched_area_name = ifelse(
        is.na(matched_area_name) &
          source == "OSM",
        value,
        matched_area_name
      ))
    
  } else {
    x <- data.frame()
    no_match = data.frame()
  }
  
  out <- list("assigned_province" = x, "no_match" = no_match)
  
  # min_dist <- x %>%
  #   select(iso3, area_name, year, kp, row_id) %>%
  #   mutate(area_name = str_replace_all(area_name, "\\,|\\/| and ", "\\;"),
  #          area_name = str_replace_all(area_name, "\\;\\;", "\\;")
  #          ) %>%
  #   distinct() %>%
  #   separate(area_name, sep = ";", into = paste0("area_split", 1:20), remove=FALSE) %>%
  #   mutate(across(starts_with("area_split"), ~str_trim(.x))) %>%
  #   pivot_longer(-c(iso3, area_name, year, row_id, kp)) %>%
  #   filter(!is.na(value)) %>%
  #   mutate(idx = row_number(),
  #          value = tolower(value)) %>%
  #   rename(given_area = area_name) %>%
  #   left_join(cities_areas, by="iso3") %>%
  #   mutate(dist = stringdist(value, tolower(area_name))) %>%
  #   group_by(idx) %>%
  #   filter(dist == min(dist))
  #
  # # Single hits, good matches
  # best_matches <- min_dist %>%
  #   filter(n() == 1, dist<3) %>%
  #   ungroup
  #
  # # Multiple hits, good matches (caused when city df and area df have the same string distance match. See "Dar-es-salaam" and "Dar es salaam")
  # best_matches <- best_matches %>%
  #   bind_rows(
  #     min_dist %>%
  #       filter(dist<3, n()>1, !is.na(area_id), !is.na(area_level)) %>%
  #       filter(area_level == max(area_level))
  #   )
  #
  # level_check <- min_dist %>%
  #   filter(dist==0, n()>1, !is.na(area_id)) %>%
  #   count(row_id, area_id, area_level) %>%
  #   filter(n() == 1)
  #
  # if(nrow(level_check)) {
  #   best_matches <- best_matches %>%
  #     bind_rows(
  #       min_dist %>%
  #         filter(dist==0, n()>1, !is.na(area_id)) %>%
  #         filter(row_id %in% level_check$row_id),
  #       min_dist %>%
  #         filter(dist==0, n()>1, !is.na(area_id)) %>%
  #         filter(!row_id %in% level_check$row_id) %>%
  #         filter(area_level == max(area_level))
  #     ) %>%
  #     ungroup
  #
  #   warning("\nArea name matched several Naomi area IDs. The finest area level has been chosen\nThis is likely a district sharing the same name as its province. Check.\n")
  #
  # }
  #
  #
  # bad_match <- min_dist %>%
  #   filter(dist>=3)
  #
  #
  # if (nrow(bad_match)) {
  #   bad_match_error <- bad_match %>%
  #     ungroup %>%
  #     mutate(iso3 = iso3_c) %>%
  #     select(iso3, given_area = value, attempted_match = area_name, attempted_area_id = area_id, string_distance = dist)
  #
  #   warning("\nString match is bad:\n",
  #           paste0(utils::capture.output(bad_match_error), collapse = "\n"))
  #
  #
  # } else {
  #   bad_match_error <- data.frame(iso3 = iso3_c, x = "No bad matches")
  # }
  #
  # x <- x %>%
  #   left_join(
  #     best_matches %>%
  #               select(row_id, area_id, matched_area_name = area_name, matched_province_area_id)
  #     )
  # filter(!is.na(area_id))
  
  # best_matches <- best_matches %>%
  #   select(row_id, idx, area_id, area_level, geometry) %>%
  #   mutate(area_level = case_when(
  #     str_detect(area_id, "_1_") ~ as.integer(1),
  #     TRUE ~ as.integer(area_level)
  #   ))
  #
  # naomi_to_admin1 <- best_matches %>%
  #   filter(area_level > admin1_lvl) %>%
  #   st_as_sf() %>%
  #   st_make_valid() %>%
  #   select(-c(area_id, area_level)) %>%
  #   st_join(areas %>% filter(area_level == admin1_lvl) %>% select(area_id) %>% st_make_valid(), largest=TRUE)
  #
  # assigned_province <- best_matches %>%
  #   # filter(!row_id %in% naomi_to_admin1$row_id) %>%
  #   filter(!idx %in% naomi_to_admin1$idx) %>%
  #   bind_rows(naomi_to_admin1) %>%
  #   select(row_id, idx, area_id) %>%
  #   arrange(row_id, idx)
  #
  # x <- x %>%
  #   select(-any_of("idx")) %>%
  #   left_join(assigned_province) %>%
  #   # select(row_id, kp, year, age_group, area_id, value, denominator, ref) %>%
  #   filter(!is.na(area_id))
  
  
  
  
  
})

write_csv(out$prev$no_match, "prev_bad_match_error.csv", na = "")
write_csv(out$prev$assigned_province,
          "prev_assigned_province.csv",
          na = "")

write_csv(out$art$no_match, "art_bad_match_error.csv", na = "")
write_csv(out$art$assigned_province, "art_assigned_province.csv", na = "")