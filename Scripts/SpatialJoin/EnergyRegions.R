# Load EMM Shapefile for EIA electricity forecast
# PBH Feb 2025

library(tidyverse)
library(terra)


# Energy Regions -------
url_file <- "Inputs/Spatial"
shp_emm <- vect(paste0(
  url_file,
  "/EMM_GIS_shapefile/25_EMM_Regions_Final_20200115.shp"
))
# terra::plot(shp_emm)
as.data.frame(shp_emm) %>% head()
unique(shp_emm$eGrid_Reg) # Note that name is misleading
# get names of EMM Regions
dict_emm <- read.csv("Inputs/Join_EMM.csv")

shp_emm <- merge(shp_emm, dict_emm, by = c("eGrid_Reg"), all.x = TRUE)
nrow(shp_emm) # no duplicates


# ecoinvent or egrid regions ----

library(geojsonsf)

sf_obj <- geojson_sf(paste0(url_file, "/eGrid/subregion.json"))

shp_ecoinvent <- vect(sf_obj)
# terra::plot(shp_ecoinvent)
# as.data.frame(shp_ecoinvent) %>% head()
unique(shp_ecoinvent$fullName)

dict_ecoinvent <- tibble(fullName = unique(shp_ecoinvent$fullName)) %>%
  mutate(
    Region = paste0(
      "US-",
      str_extract(fullName, "WECC|NPCC|ASCC|SERC|RFC|HICC|MRO|ERCOT|FRCC|SPP")
    ) %>%
      str_replace("ERCOT", "TRE") %>% # Texas case
      str_replace("FRCC", "SERC") %>% # Florida does not have ecoinvent - use SERC (old maps)
      str_replace("SPP", "MRO")
  ) %>% # Region with no ecoinvent, use MRO (old maps)
  filter(Region != "US-NA")
dict_ecoinvent

# add to shp
shp_ecoinvent <- merge(
  shp_ecoinvent,
  dict_ecoinvent,
  by = c("fullName"),
  all.x = TRUE
)
nrow(shp_ecoinvent) # no duplicates

# remove NA - states and grid for Puerto Rico
shp_ecoinvent <- subset(shp_ecoinvent, !is.na(shp_ecoinvent$Region))

# remove unnecesary info
as.data.frame(shp_ecoinvent) %>% names()
shp_ecoinvent$STATE <- shp_ecoinvent$fuelMixCategories <- shp_ecoinvent$GEO_ID <- shp_ecoinvent$fuelMix <- NULL
shp_ecoinvent$NAME <- shp_ecoinvent$dataYear <- shp_ecoinvent$emissionFactor <- NULL
shp_ecoinvent$type <- shp_ecoinvent$CENSUSAREA <- NULL
shp_ecoinvent$gridLoss <- shp_ecoinvent$LSAD <- NULL
shp_ecoinvent$Shape__Len <- shp_ecoinvent$Shape__Are <- NULL
# terra::plot(shp_ecoinvent)

# EoF
