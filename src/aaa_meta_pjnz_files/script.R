#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

read_region_na <- function(x) {
  v <- eppasm::read_region(x)
  if(is.null(v))
    v <- NA_character_
  v
}

pjnz_folder_path <- "Shared Documents/Data/Spectrum files/2022 final restricted/"

folder_names <- c("EPP-Gen",
                  "EPP-Gen/Nigeria 2022  03 09")

pjnz_file_paths <- paste0(pjnz_folder_path, folder_names, "/")

pjnz_file_names <- pjnz_file_paths %>%
  lapply(function(x) sharepoint$folder("HIVInferenceGroup-WP", URLencode(x))) %>%
  lapply(function(x) x$files()$name)

names(pjnz_file_names) <- pjnz_file_paths

df <- stack(pjnz_file_names, recursive = TRUE, use.names = TRUE) %>%
  select(file_name = values, path = ind) %>%
  filter(grepl("*.pjnz", ignore.case = TRUE, file_name))

pjnz_files <- paste0("sites/HIVInferenceGroup-WP/", df$path, df$file_name) %>%
  lapply(URLencode) %>%
  lapply(sharepoint$download)

df$iso3 <- vapply(pjnz_files, eppasm::read_iso3, character(1))
df$country <- vapply(pjnz_files, eppasm::read_country, character(1))
df$region <- vapply(pjnz_files, read_region_na, character(1))
df$region_code <- vapply(pjnz_files, read_pjnz_region_code, integer(1))
df$has_shiny90 <- vapply(pjnz_files, check_pjnz_shiny90, logical(1))

#' Merge shiny90 files

#' df$shiny90_country <- if_else(is.na(df$region), df$country, paste(df$country, "-", df$region))
#'
#' #' Check and not found in Spectrum list
#' anti_join(shiny90, df, by = "shiny90_country")
#'
#' df <- left_join(df, shiny90, by = "shiny90_country")
#'
#' df$pjnz <- sub("/Users/jeff/Data/Spectrum files/2020 final shared/", "", df$pjnz)
#' df$shiny90 <- sub("/Users/jeff/Data/Spectrum files/2020 final shared/", "", df$shiny90)
#'
#' df <- select(df, iso3, country, region, region_code, has_shiny90, shiny90_country,
#'              pjnz_file = pjnz, shiny90_file = shiny90)

write_csv(df, "meta-pjnz-files.csv", na = "")
