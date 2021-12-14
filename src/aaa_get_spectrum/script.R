# Pull naomi data sets from ADR

# Get API key
## To set up API key in .Renviron: open your REnviron using
## file.edit("~/.Renviron") and save your key as "ADR_KEY"
api_key <- Sys.getenv("ADR_KEY")

# Map iso3 parameter to ADR geo-location + organistaion
map <- read_csv("resources/org_iso_mapping.csv")
org <- map$organisation[map$iso3== iso3]

if(!iso3 %in% c("MWI", "ETH", "ZAF", "CAF", "GNQ", "GNB", "SSD")) {

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
          filter(resource_type  %in% c("inputs-unaids-art", "inputs-unaids-anc",
                                       "inputs-unaids-spectrum-file"))
  
  
  # Filter urls for country specific organisation
  paths <- list(url = list(pjnz = df$url[df$resource_type == "inputs-unaids-spectrum-file"],
                                anc_testing = df$url[df$resource_type == "inputs-unaids-anc"],
                                art_number = df$url[df$resource_type == "inputs-unaids-art"]),
                file_name = list("depends/spectrum_file.zip",
                                 "depends/anc_testing_2021.csv",
                                 "depends/art_number_2021.csv"))
  
  # Extract and export spectrum file
  
  if(!length(paths$url$pjnz)) {
    stop(paste0("No spectrum file in ", org, " 2021 estimates shell"))
  }
  
  pjnz_file <- ckan_fetch(paths$url$pjnz, store = "disk", key = api_key,
                          path = tempfile(fileext = ".zip"))
  
  pjnz_paths <- get_pjnz_paths(pjnz_file$path)


} else {
  
  sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))
  folder_2020 <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = "Shared Documents/Data/Spectrum files/2020 final shared/SSA")
  folder_2021 <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = "Shared Documents/Data/Spectrum files/2021 final shared/ESA")
  
  if(iso3 %in% c("MWI", "ETH", "ZAF", "SSD")) {
    
    str_search <- paste0(c(iso3, countrycode(iso3, "iso3c", "country.name")), collapse = "|")
    path <- grep(str_search, folder_2021$list()$name, value=TRUE, ignore.case = TRUE)
    path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/Spectrum files/2021 final shared/ESA", path)
    
  } else if (iso3 == "CAF") {
    
    path <- grep("CAR", folder_2020$list()$name, value=TRUE) %>%
      grep("PJNZ", ., value = TRUE)
    
    path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/Spectrum files/2020 final shared/SSA", path)
    
  } else if (iso3 == "GNQ") {
    
    path <- grep("Equatorial_Guinea", folder_2020$list()$name, value=TRUE, ignore.case = TRUE) %>%
      grep("PJNZ", ., value = TRUE)
    path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/Spectrum files/2020 final shared/SSA", path)
    
  } else if (iso3 == "GNB") {
    
    path <- grep("Guine_Bissau", folder_2020$list()$name, value=TRUE, ignore.case = TRUE) %>%
      grep("PJNZ", ., value = TRUE)
    path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/Spectrum files/2020 final shared/SSA", path)
  }
  
  pjnz_paths <- lapply(path, function(x) {sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = x)})

}

region_codes <- vapply(pjnz_paths, read_pjnz_region_code, integer(1))

outdir <- tempfile()
outfiles <- file.path(outdir,
                      sprintf("%s%02d.pjnz", tolower(iso3), region_codes))

dir.create(outdir)
Map(copy_pjnz_extract, pjnz_paths, outfiles)
files2zip <- dir(outdir, full.names = TRUE)
zip(zipfile = "spectrum_file", files = files2zip, flags = '-r9Xj')






