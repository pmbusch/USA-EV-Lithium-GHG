# Load US Census data at County level, total population
# Joins Census data to other regions aggregators
# PBH Feb 2025
# Population spatial layers are useful to conduct spatial joins

library(tidyverse)

url_file <- "Inputs/Spatial"

# State codes----
state <- read_csv(paste0(url_file, "/Census/State_Codes.csv"), col_types = "cc")
state <- state %>%
  mutate(State = str_to_title(State) %>% str_replace("Of Colum", "of Colum"))
state_list <- paste(state$State, collapse = "|")

# Census Pop data -------
census <- readxl::read_excel(
  paste0(url_file, "/Census/co-est2023-pop.xlsx"),
  sheet = "CO-EST2023-POP",
  range = "A5:F3149"
)
names(census) <- c("County", "20201April", "2020", "2021", "2022", "pop")
head(census)
census <- census[, c(1, 6)] # 2023 data
sum(census$pop) / 1e6 # 334.9M


# Note: to join Census to shapefiles data we need to use full name and get state
census <- census %>%
  mutate(State = map_chr(str_split(County, ","), ~ .x[2])) %>%
  mutate(State = str_extract(State, state_list)) %>%
  mutate(County = map_chr(str_split(County, ","), ~ .x[1])) %>%
  mutate(County = substr(County, 2, str_length(County)))
head(census)
census <- census %>%
  rename(NAMELSAD = County) %>%
  dplyr::select(State, NAMELSAD, pop)


# Shapefile ----
library(terra)

shp <- vect(paste0(url_file, "/tl_2024_us_county/tl_2024_us_county.shp"))
# terra::plot(shp)

# join names
nrow(shp) # 3235
# add state name
shp <- merge(shp, state, by = "STATEFP", all.x = TRUE)

# check that every population count has a shapefile
b <- as.data.frame(shp)
a <- as.data.frame(shp) %>% dplyr::select(State, NAME, NAMELSAD)
aux <- left_join(census, a)
filter(aux, is.na(NAME))


# add pop
shp <- merge(shp, census, by = c("State", "NAMELSAD"), all.x = TRUE)
nrow(shp) # no duplicates
sum(shp$pop, na.rm = T) / 1e6 # perfect match

# remove NA pop
shp <- subset(shp, !is.na(shp$pop))
nrow(shp)
unique(shp$State)

# Plot using ggplot
shp_sf <- sf::st_as_sf(shp)
ggplot(shp_sf) +
  geom_sf(aes(fill = pop), color = "black") +
  scale_fill_gradientn(
    colors = c("white", "purple", "red"),
    values = scales::rescale(c(min(shp_sf$pop), max(shp_sf$pop)))
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) + # Continental US bounds
  theme_minimal()


# Transport regions (by state) ----------

# load transport region EIA - State equivalency
dict_EIA <- read.csv("Inputs/Join_TransportEIA_State.csv") %>%
  rename(State = NAME)

# add Region transport to each county, based on state
shp <- merge(shp, dict_EIA, by = c("State"), all.x = TRUE)
nrow(shp) # no duplicates - 3144 after removing NAs
as.data.frame(shp) %>% group_by(Region_Transport, State) %>% tally()

# %%%%%%%%%%%%%%%%%%%%%%%%%
# Join to Energy --------
# %%%%%%%%%%%%%%%%%%%%%%%%%

source("Scripts/SpatialJoin/EnergyRegions.R")

## Join to EMM ----
crs(shp)
crs(shp_emm)

terra::plot(shp_emm, border = "blue")
terra::plot(shp, border = "darkred", add = TRUE)

# correct different projection
shp <- project(shp, crs(shp_emm))

# id to keep track of each observation
shp$id <- 1:nrow(shp)

# Compute intersection areas
intersections <- intersect(shp, shp_emm)
intersections$area <- expanse(intersections) # area

# Compute area of county polygons
areaCounties <- data.frame(id = shp$id, areaCounty = expanse(shp))

# Merge to get the original area counties
intersections <- as.data.frame(intersections)
intersections <- merge(intersections, areaCounties, by = "id", all.x = TRUE)
names(intersections)

# Compute the proportion of area overlap
intersections$propArea <- intersections$area / intersections$areaCounty

# exploration - not many borderline counties with close to 50% area overlap
ggplot(intersections, aes(propArea)) + geom_histogram()

# get repeated elements
intersections <- intersections %>%
  group_by(State, NAMELSAD) %>%
  mutate(n = n()) %>%
  ungroup()

# less than 50% of area but with a unique intersection - keep as they are on border
intersections %>% filter(n == 1, propArea < 0.5) %>% nrow()

# no counties have more than one intersection at 0.5
intersections %>%
  filter(propArea > 0.5) %>%
  group_by(State, NAMELSAD) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  nrow()

# Keep matches with above maximum area overlap
result <- intersections %>%
  group_by(State, NAMELSAD) %>%
  mutate(maxOverlap = max(propArea)) %>%
  ungroup() %>%
  filter(propArea == maxOverlap)

names(result)
result <- result %>%
  dplyr::select(State, NAMELSAD, eGrid_Reg, Region_Electricity)

# join to shp
shp <- merge(shp, result, by = c("State", "NAMELSAD"), all.x = TRUE)
nrow(shp)

# check failed joins - Alaska and Hawai are not part of EIA-EMM data
as.data.frame(shp) %>%
  filter(is.na(eGrid_Reg)) %>%
  filter(!State %in% c("Alaska", "Hawaii"))
# both in Massachusetts, due to different map resolution, check region
as.data.frame(shp) %>%
  filter(State == "Massachusetts") %>%
  group_by(eGrid_Reg, Region_Electricity) %>%
  tally()
# simply use this region, as both counties are on the coast
aux <- as.data.frame(shp) %>%
  mutate(
    eGrid_Reg = if_else(
      State == "Massachusetts" & is.na(eGrid_Reg),
      "ISNE",
      eGrid_Reg
    ),
    Region_Electricity = if_else(
      State == "Massachusetts" & is.na(Region_Electricity),
      "Northeast Power Coordinating Council / New England",
      Region_Electricity
    )
  )
values(shp) <- aux
rm(aux)


## Join to ecoinvent or egrid (same map) -----

# same projection
crs(shp)
crs(shp_ecoinvent)

terra::plot(shp_ecoinvent, border = "blue") # includes HI and AK
terra::plot(shp, border = "darkred", add = TRUE)

shp$id <- 1:nrow(shp)

# Compute intersection areas
intersections <- intersect(shp, shp_ecoinvent)
intersections$area <- expanse(intersections)

# Compute area of county polygons
areaCounties <- data.frame(id = shp$id, areaCounty = expanse(shp))

# Merge to get the original area counties
intersections <- as.data.frame(intersections)
intersections <- merge(intersections, areaCounties, by = "id", all.x = TRUE)
names(intersections)

# Compute the proportion of area overlap
intersections$propArea <- intersections$area / intersections$areaCounty

# exploration - not many borderline counties with close to 50% area overlap
ggplot(intersections, aes(propArea)) + geom_histogram()

# get repeated elements
intersections <- intersections %>%
  group_by(State, NAMELSAD) %>%
  mutate(n = n()) %>%
  ungroup()

# less than 50% of area but with a unique intersection - keep
intersections %>% filter(n == 1, propArea < 0.5) %>% nrow()

# no counties have more than one intersection at 0.5
intersections %>%
  filter(propArea > 0.5) %>%
  group_by(State, NAMELSAD) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  nrow()

# Keep matches with above maximum area overlap
result <- intersections %>%
  group_by(State, NAMELSAD) %>%
  mutate(maxOverlap = max(propArea)) %>%
  ungroup() %>%
  filter(propArea == maxOverlap)

names(result)
result <- result %>% dplyr::select(State, NAMELSAD, fullName, Region)

# join to shp
shp <- merge(shp, result, by = c("State", "NAMELSAD"), all.x = TRUE)
nrow(shp)

# check failed joins
as.data.frame(shp) %>% filter(is.na(fullName)) # 4 in total
as.data.frame(shp) %>%
  filter(State %in% c("Massachusetts", "Hawaii", "Washington")) %>%
  group_by(State, fullName, Region) %>%
  tally()
# trivial regions to assign

# simply use this region, as both counties are on the coast
aux <- as.data.frame(shp) %>%
  mutate(
    fullName = if_else(
      State == "Massachusetts" & is.na(fullName),
      "NPCC New England",
      fullName
    ),
    fullName = if_else(
      State == "Hawaii" & is.na(fullName),
      "HICC Miscellaneous",
      fullName
    ),
    fullName = if_else(
      State == "Washington" & is.na(fullName),
      "WECC Northwest",
      fullName
    ),
    Region = if_else(
      State == "Massachusetts" & is.na(Region),
      "US-NPCC",
      Region
    ),
    Region = if_else(State == "Hawaii" & is.na(Region), "US-HICC", Region),
    Region = if_else(State == "Washington" & is.na(Region), "US-WECC", Region)
  )
values(shp) <- aux
rm(aux)

# Save data frame with spatial joins -----

df <- as.data.frame(shp)
df <- df %>%
  dplyr::select(
    State,
    NAMELSAD,
    STATEFP,
    COUNTYFP,
    NAME,
    pop,
    Region_Transport,
    eGrid_Reg,
    Region_Electricity,
    fullName,
    Region
  ) %>%
  rename(Region_ecoinvent = Region) %>%
  rename(Region_ecoinvent_detail = fullName) %>%
  rename(Region_Electricity_EIA = Region_Electricity) %>%
  rename(Region_EMM = eGrid_Reg)
names(df)

# df at county level contains the assigned region for EIA electricity data, EIA transport data and ecoinvent region
write.csv(df, "Parameters/census_joins.csv", row.names = F)

# EoF
