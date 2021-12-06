
# This script will pull the following artefacts from the orderly archive
# for the downstream use in a parameterised orderly task:
# * Areas: latest from orderly archive
# * Population: latest from orderly, worldpop or gpw (pull from org-iso-mapping in global resources)
# * Survey: latest from orderly archive


# Prepare model inputs
files <- list.files("depends")

## Boundary file
area <- files[grepl(paste0(tolower(iso3), "_areas"), files)]
area_file <- paste0("depends/", area)

if (!basename(area_file) %in% files) {
  stop("Areas file not found: ", area_file)
}
areas <- read_area_merged(area_file)

# Save boundaries
st_write(areas, "naomi_areas.geojson", delete_dsn = TRUE)


## Population files
# Map iso3 parameter to population source
# map <- read_csv("resources/org_iso_mapping.csv")
# pop_source <- map$pop_data_source[map$iso3 == iso3]
# 
# if(pop_source == "orderly") {
#   
#   pop_file <- files[grepl(paste0(tolower(iso3), "_population"), files)]
#   pop <- read_csv(paste0("depends/", pop_file))
#   
# } else if (pop_source == "gpw") {
#   
#   pop <- read_csv("depends/population_gpw_naomi.csv")
#   
# } else if (pop_source == "worldpop") {
#   
#   pop <- read_csv("depends/population_worldpop_naomi.csv")
#   
# }
# 
# # Save population file
# write_csv(pop, "naomi_population.csv")

## Survey files
# survey_file <- files[grepl(paste0(tolower(iso3), "_survey"), files)]
# 
# if (!basename(survey_file) %in% files) {
#   stop("Survey file not found: ", area_file)
# }
# 
# survey <- read_csv(paste0("depends/", survey_file))
# 
# # Save survey file
# write_csv(survey, "naomi_survey.csv")






