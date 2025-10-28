# Download EIA Electricity Data
# GHG Estimation module
# Source: EIA Annual Energy Outlook
# https://www.eia.gov/outlooks/aeo/tables_ref.php
# PBH January 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source(
  "Scripts/Prepare-DataInputs/Electricity/00-EIA_API.R",
  encoding = "UTF-8"
) # private API

# Download data -----

# Problem: Query only returns 5000 results
# Solution: Loop through regions and rbind into df

library(jsonlite)
# 2023 old
# url <- "https://api.eia.gov/v2/aeo/2023/data/?api_key=%s&frequency=annual&data[0]=value&facets[tableId][]=%s&facets[scenario][]=%s&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"
url <- "https://api.eia.gov/v2/aeo/2025/data/?api_key=%s&frequency=annual&data[0]=value&facets[tableId][]=%s&facets[scenario][]=%s&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000"

## get unique regions -----
url_regions <- paste0(
  sprintf(url, eia_api, "62", "ref2025"),
  "&start=2026&end=2026"
)
data <- fromJSON(url_regions)$response$data
df <- as.data.frame(data)
(region_id <- unique(df$regionId))


# get unique scenarios
url_scens <- paste0(sprintf(url, eia_api, "62", "ref2025"))
url_scens <- str_replace(
  url_scens,
  "&facets\\[scenario]\\[]=ref2025",
  "&facets[seriesId][]=gen_NA_elep_NA_teg_NA_flrc_blnkwh"
)
data <- fromJSON(url_scens)$response$data
df <- as.data.frame(data)
(scenarios <- unique(df$scenario))
unique(df$scenarioDescription)

# Electricity Data -------
## Loop through regions to get electricity data ------

# if any row is 5000, then we have a problem
df <- c()
for (scen in scenarios) {
  for (reg in region_id) {
    # Filters
    # Table 54 - Electric Power Projections by Electricity Market Module Region
    # Scenario: Reference 2023
    url_reg <- paste0(
      sprintf(url, eia_api, "62", scen),
      "&facets[regionId][]=",
      reg
    )
    data <- fromJSON(url_reg)
    df_reg <- as.data.frame(data$response$data)
    cat(scen, " , ", reg, ", rows: ", nrow(df_reg), "\n")
    df <- rbind(df, df_reg)
    rm(df_reg, data)
    Sys.sleep(4) # wait seconds before sending a new API request, too avoid too many request error
  }
}
df %>% group_by(scenario) %>% tally()
nrow(df) # Should be 87464 per scenario, according to webpage results

## Save data  -----
write.csv(df, "Inputs/EIA_Table54.csv", row.names = F)

# Renewable Electricity Data -------
## Loop through regions to get electricity data ------

# if any row is 5000, then we have a problem
df2 <- c()
for (scen in scenarios) {
  for (reg in region_id) {
    # Filters
    # Table 56 - Renewable Energy Generation by Fuel
    # Scenario: Reference 2023
    url_reg <- paste0(
      sprintf(url, eia_api, "67", scen),
      "&facets[regionId][]=",
      reg
    )
    data <- fromJSON(url_reg)
    df_reg <- as.data.frame(data$response$data)
    cat(scen, " , ", reg, ", rows: ", nrow(df_reg), "\n")
    df2 <- rbind(df2, df_reg)
    rm(df_reg, data)
    Sys.sleep(4) # wait seconds before sending a new API request, too avoid too many request error
  }
}

df2 %>% group_by(scenario) %>% tally()
nrow(df2) # Should be 64090 per scenario, according to webpage results

## Save data  -----
write.csv(df2, "Inputs/EIA_Table56.csv", row.names = F)

# EoF
