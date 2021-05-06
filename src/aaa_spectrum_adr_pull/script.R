# Pull naomi data sets from ADR

# Get API key
## To set up API key in .Renviron: open your REnviron using
## file.edit("~/.Renviron") and save your key as "ADR_KEY"
api_key <- Sys.getenv("ADR_API_KEY")

# Map iso3 parameter to ADR geo-location + organistaion
map <- read_csv("resources/org_iso_mapping.csv")
org <- map$organisation[map$iso3== iso3]

# Request access to ADR API
build_query <- function(data) {
  paste(vapply(names(data), function(x) {
    sprintf("%s:%s", x, data[[x]])
  }, character(1)), collapse = " AND ")
}

terms <- list(
  organization = org,
  type = "inputs-unaids-estimates"
)

url <- sprintf("https://adr.fjelltopp.org/api/3/action/package_search?q=%s&hide_inaccessible_resources=true&rows=100", build_query(terms))

res <- GET(url,
           add_headers(Authorization = api_key),
           httr::verbose()
)

res_unaids <- fromJSON(content(res, "text"))$result


# Filter resource list
df <- res_unaids$results$resources[[1]]

df <- df %>%
  select(resource_type, url) %>%
        filter(resource_type  %in% c(
          # "inputs-unaids-art", "inputs-unaids-anc",
                                     "inputs-unaids-spectrum-file"))


# Filter urls for country specific organisation
paths <- list(url = list(pjnz = df$url[df$resource_type == "inputs-unaids-spectrum-file"]
                              # anc_testing = df$url[df$resource_type == "inputs-unaids-anc"],
                              # art_number = df$url[df$resource_type == "inputs-unaids-art"]
                         ),
              file_name = list("depends/spectrum_file.zip"
                               # "depends/anc_testing_2021.csv",
                               # "depends/art_number_2021.csv")
                               )
)

# Extract and export spectrum file
if(length(paths$url$pjnz)) {
  pjnz_file <- ckan_fetch(paths$url$pjnz, store = "disk", key = api_key,
                          path = tempfile(fileext = ".pjnz"))


  # Create temp directory and file paths based on number of spectrum files
  region_code <- read_pjnz_region_code(pjnz_file$path)
  outdir <- tempfile()
  outfile <- file.path(outdir, sprintf("%s%02d.pjnz", tolower(iso3), region_code))

  dir.create(outdir)

  Map(copy_pjnz_extract, pjnz_file$path, outfile)
  files2zip <- dir(outdir, full.names = TRUE)
  zip(zipfile = 'spectrum_file', files = files2zip, flags = '-r9Xj')

} else {

  stop(paste0("No spectrum file in ", org, " 2021 estimates shell"))

}


# # Extract and import ANC file
# if(length(paths$url$anc_testing)){
#   anc_tmp <- ckan_fetch(paths$url$anc_testing, store = "disk", key = api_key,
#                         path = tempfile(fileext = ".csv"))
#   anc <- read_csv(anc_tmp$path)
#   write_csv(anc, "anc_testing_2021.csv")
# 
# } else {
#   message(paste0("No ANC data in ", org, " 2021 estimates shell"))
# 
# 
#   empty_anc_df <- data.frame(area_id = character(0),
#                              area_name = character(0),
#                              age_group = character(0),
#                              year = integer(0),
#                              anc_clients = numeric(0),
#                              ancrt_hiv_status = numeric(0),
#                              ancrt_known_pos = numeric(0),
#                              ancrt_already_art = numeric(0),
#                              ancrt_tested = numeric(0),
#                              ancrt_test_pos = numeric(0))
# 
#   write_csv(empty_anc_df,"anc_testing_2021.csv", na = "")
# 
# 
# }
# 
# # Extract and import ART file
# if(length(paths$url$art_number)){
#   art_tmp <- ckan_fetch(paths$url$art_number, store = "disk", key = api_key,
#                         path = tempfile(fileext = ".csv"))
#   art <- read_csv(art_tmp$path)
#   write_csv(art, "art_number_2021.csv")
# 
# } else {
#   message(paste0("No ART data in ", org, " 2021 estimates shell"))
# 
#   empty_art_df <- data.frame(area_id = character(0),
#                              area_name = character(0),
#                              sex = character(0),
#                              age_group = character(0),
#                              calendar_year = character(0),
#                              art_current = numeric(0))
# 
#   write_csv(empty_art_df,"art_number_2021.csv", na = "")
# }

# Save data pull summary
paths$url[lengths(paths$url) == 0] <- NA_real_

log <- data.frame(lapply(paths, unlist))
log$file_type <- row.names(log)

log <- log %>%
  mutate(file_type = rownames(log),
       date_pulled = Sys.Date(),
       iso3 = iso3,
       organisation = org)

write_csv(log, "adr_model_inputs_log.csv")











