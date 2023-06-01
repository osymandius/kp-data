#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

raw_path <- "sites/HIVInferenceGroup-WP/"

df <- read_csv("depends/meta-pjnz-files.csv")

df <- filter(df, iso3 == !!iso3)

df$url_pjnz <- paste0(raw_path, df$path, df$file_name)
df$url_pjnz <- lapply(df$url_pjnz, URLencode)
pjnz_files <- lapply(df$url_pjnz, sharepoint$download)

# df$url_shiny90 <- file.path(spectrum_raw_path, df$shiny90_file)
# df$url_shiny90 <- lapply(df$url_shiny90, URLencode)
# shiny90_files <- lapply(df$url_shiny90, sharepoint$download)

#' Name to save out PJNZ
outdir <- tempfile()
df$out_pjnz <- file.path(outdir, sprintf("%s%02d.pjnz", tolower(iso3), df$region_code))

#' Extract and export spectrum file
dir.create(outdir)
Map(copy_pjnz_extract, pjnz_files, df$out_pjnz)

#' Confirm all files include .shiny90
# stopifnot(vapply(df$out_pjnz, check_pjnz_shiny90, logical(1)))

#' Zip multiple PJNZ into an archive

files2zip <- dir(outdir, full.names = TRUE)
utils::zip(zipfile = "naomi_pjnz", files = files2zip, flags = '-r9Xj')

# For NER, SEN - copy pjnz off ADR

if(iso3 %in% c("NER", "SEN")){

  # Pull naomi data sets from ADR

  # Get API key
  ## To set up API key in .Renviron: open your REnviron using
  ## file.edit("~/.Renviron") and save your key as "ADR_KEY"
  api_key <- Sys.getenv("ADR_KEY")

  # Map iso3 parameter to ADR geo-location + organistaion
  map <- read_csv("resources/org_iso_mapping.csv")
  org <- map$organisation[map$iso3== iso3]

  # Request access to ADR API
  build_query <- function(data) {
    paste(vapply(names(data), function(x) {
      sprintf("%s:%s", x, data[[x]])
    }, character(1)), collapse = "&")
  }

  terms <- list(
    organization = org
  )

  url <- sprintf("https://adr.fjelltopp.org/api/3/action/package_search?q=%s&hide_inaccessible_resources=true&rows=100", build_query(terms))

  res <- GET(url,
             add_headers(Authorization = api_key),
             httr::verbose()
  )

  res_unaids <- fromJSON(content(res, "text"))$result

  # Filter resource list
  results <- res_unaids$results %>% dplyr::filter(type == resource_type)
  df <- results$resources[[1]]

  df <- df %>%
    select(resource_type, url) %>%
    filter(resource_type  == "inputs-unaids-spectrum-file")


  # Filter urls for country specific organisation
  path <- df$url[df$resource_type == "inputs-unaids-spectrum-file"]


  # Extract and export spectrum file
  if(!length(path)) {
    paste0("No spectrum file in ", org, " 2022 estimates shell")
  }

  pjnz_file <- ckan_fetch(path, store = "disk", key = api_key,
                          path = tempfile(fileext = ".zip"))

  pjnz_path <- get_pjnz_paths(pjnz_file$path)

  region_codes <- vapply(pjnz_path, read_pjnz_region_code, integer(1))

  outdir <- tempfile()
  outfiles <- file.path(outdir,
                        sprintf("%s%02d.pjnz", tolower(iso3), region_codes))

  dir.create(outdir)
  Map(copy_pjnz_extract, pjnz_path, outfiles)
  files2zip <- dir(outdir, full.names = TRUE)
  utils::zip(zipfile = "naomi_pjnz", files = files2zip, flags = '-r9Xj')


}


