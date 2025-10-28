# Upstream Emissions
# YC April 2025

# Location of excel
url_file <- "~/Library/CloudStorage/GoogleDrive-yuzchen@ucdavis.edu/My Drive/GHG Model/%s"


# LIB BOM g/ kg LIB BOM---------

# prepare LIB BOM from GREET2024 table
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)

LIB_BOM <- read_excel(
  sprintf(url_file, "GREET2024_Data.xlsx"),
  sheet = "LIB_BoM",
  col_names = FALSE
)
# Rows that contain the emission parameters(e.g. CO2, CH4)
parameter_rows <- 8:25

# Store each battery type’s data
battery_data_list <- list()

# Each battery type is spread over 3 columns: name (i), value (i+1), unit (i+2)
# Start from column 2 and process every 3 columns
for (i in seq(2, 190, by = 3)) {
  type <- as.character(LIB_BOM[1, i, drop = TRUE])
  range <- as.character(LIB_BOM[2, i, drop = TRUE])
  chem <- as.character(LIB_BOM[3, i, drop = TRUE])
  param_names <- as.character(LIB_BOM[parameter_rows, i, drop = TRUE])
  values <- suppressWarnings(as.numeric(LIB_BOM[
    parameter_rows,
    i + 1,
    drop = TRUE
  ]))
  units <- tolower(as.character(LIB_BOM[parameter_rows, i + 2, drop = TRUE]))

  if (all(is.na(values))) {
    next
  }

  # Convert to grams
  values_g <- case_when(
    units == "kg" ~ values * 1000,
    units == "mg" ~ values * 0.001,
    units == "ug" ~ values * 0.000001,
    TRUE ~ values
  )

  # Convert each battery group into a tidy row and compile all rows into a final dataframe
  param_values <- setNames(
    as.list(values_g),
    make.names(param_names, unique = TRUE)
  )
  battery_row <- tibble(
    Type = type,
    Range = range,
    LIB_Chem = chem,
    !!!param_values
  )

  battery_data_list[[length(battery_data_list) + 1]] <- battery_row
}

LIB_BOM_tidy <- bind_rows(battery_data_list)
LIB_BOM_tidy <- LIB_BOM_tidy %>%
  mutate(Type = if_else(Type == "Car", "CAR", Type))


# Calculate the Global Warming Potential (GWP) for each battery chemistries and ranges
# using AR6 values: CO2 = 1, CH4 = 29.8, N2O = 273
LIB_BOM_tidy <- LIB_BOM_tidy %>%
  mutate(GWP = CO2.Total * 1 + CH4.Total * 29.8 + N2O * 273) %>%
  relocate(GWP, .after = LIB_Chem)


# LIB_Specific Energy Wh/kg---------

raw <- read_excel(
  sprintf(url_file, "GREET2024_Data.xlsx"),
  sheet = "LIB_Specific Energy",
  col_names = FALSE
)

output <- list()
range_labels <- c("150mile", "200mile", "300mile")
col_blocks <- list(1:8, 9:16, 17:24)

for (i in seq(1, nrow(raw), by = 4)) {
  model <- as.character(raw[i, 1][[1]])

  for (j in seq_along(col_blocks)) {
    range <- range_labels[j]
    cols <- col_blocks[[j]]

    chemistries <- as.character(unlist(raw[i + 2, cols]))
    values <- as.numeric(unlist(raw[i + 3, cols]))

    df <- tibble(
      Type = model,
      Range = range,
      LIB_Chem = chemistries,
      Spec_Energy_WhPerKg = values
    ) %>%
      filter(!is.na(Spec_Energy_WhPerKg))

    output[[length(output) + 1]] <- df
  }
}

LIB_SpecEnergy <- bind_rows(output)

# LIB_upstream kgCO2e/kWh---------

LIB_upstream <- LIB_BOM_tidy %>%
  left_join(
    LIB_SpecEnergy %>%
      select(Type, Range, LIB_Chem, Spec_Energy_WhPerKg),
    by = c("Type", "Range", "LIB_Chem")
  ) %>%
  relocate(Spec_Energy_WhPerKg, .before = GWP)

LIB_upstream_GHG <- LIB_upstream %>%
  mutate(kgco2e_kwh = GWP / Spec_Energy_WhPerKg) %>%
  relocate(kgco2e_kwh, .before = GWP)


upstream_libmaterial <- LIB_upstream_GHG %>%
  filter(
    Type %in% c("CAR", "PUT"),
    Range == "300mile",
    LIB_Chem != "LMO"
  ) %>%
  select(Type, Range, LIB_Chem, kgco2e_kwh)
write.csv(
  upstream_libmaterial,
  "Inputs/upstream_libmaterial.csv",
  row.names = FALSE
)

# LIB_Assembly kgCO2e/kWh---------

LIB_Assembly <- read_excel(
  sprintf(url_file, "GREET2024_Data.xlsx"),
  sheet = "LIB_Assembly",
  col_names = FALSE
)
LIB_Assembly <- data.frame(
  emission = as.character(LIB_Assembly[10:22, ][[1]]),
  g_per_kWh = as.numeric(LIB_Assembly[10:22, ][[2]])
)
GWP_factors <- c(CO2 = 1, CH4 = 29.8, N2O = 273)

LIB_Assembly <- LIB_Assembly %>%
  mutate(
    GWP_factor = GWP_factors[emission],
    GWP_component = g_per_kWh * GWP_factor
  )
write.csv(LIB_Assembly, "Inputs/LIB_Assembly.csv", row.names = FALSE)


# Gasoline kg CO2e/gal---------

Gasoline <- read_excel(
  sprintf(url_file, "GREET2024_Data.xlsx"),
  sheet = "Gasoline",
  col_names = FALSE
)

# The lower heating value (LHV) of gasoline from GREET
# Btu/gal
gasoline_LHV <- 112194

# Extract emission names and values
Gas_upstream <- data.frame(
  emission = as.character(Gasoline[11:30, ][[1]]),
  g_per_mmbtu = as.numeric(Gasoline[11:30, ][[9]])
)

# Convert from g/mmBtu to g/gal
Gas_upstream <- Gas_upstream %>%
  mutate(g_per_gal = g_per_mmbtu * gasoline_LHV / 1e6)

GWP_factors <- c(
  "CO2 (w/ C in VOC & CO)" = 1,
  "CH4" = 29.8,
  "N2O" = 273
)

Gas_upstream <- Gas_upstream %>%
  mutate(
    g_per_gal = as.numeric(g_per_gal),
    GWP_factor = GWP_factors[emission],
    GWP_component = g_per_gal * GWP_factor
  )
write_csv(Gas_upstream, "Inputs/Gas_upstream.csv")


# Vehicle Production(Exclude EV Battery)---------

Vehi_Prod <- read_excel(
  sprintf(url_file, "GREET2024_Data.xlsx"),
  sheet = "Vehi_Prod",
  col_names = FALSE
)

GWP_factors <- c(CO2 = 1, CH4 = 29.8, N2O = 273)

# ICE vehicle data
get_ICE <- function(rows, category) {
  data.frame(
    vehicle_type = "ICE",
    vehicle_class = category,
    Emissions = as.character(Vehi_Prod[rows, 1][[1]]),
    g_per_vehlife = as.numeric(Vehi_Prod[rows, 6][[1]])
  )
}

# BEV vehicle data (exclude battery-related emissions)
get_BEV <- function(rows, category) {
  data.frame(
    vehicle_type = "EV",
    vehicle_class = category,
    Emissions = as.character(Vehi_Prod[rows, 1][[1]]),
    g_per_vehlife = as.numeric(Vehi_Prod[rows, 6][[1]]) -
      as.numeric(Vehi_Prod[rows, 4][[1]])
  )
}

Prod_CAR_ICE <- get_ICE(11:23, "CAR")
Prod_CAR_BEV <- get_BEV(43:55, "CAR")

Prod_SUV_ICE <- get_ICE(75:87, "SUV")
Prod_SUV_BEV <- get_BEV(107:119, "SUV")

Prod_PUT_ICE <- get_ICE(140:152, "PUT")
Prod_PUT_BEV <- get_BEV(172:184, "PUT")

Prod_Veh <- bind_rows(
  Prod_CAR_ICE,
  Prod_CAR_BEV,
  Prod_SUV_ICE,
  Prod_SUV_BEV,
  Prod_PUT_ICE,
  Prod_PUT_BEV
) %>%
  mutate(
    GWP_factor = GWP_factors[Emissions],
    GWP_component = g_per_vehlife * GWP_factor
  )
write_csv(Prod_Veh, "Inputs/Prod_Veh.csv")


# LIB Recycling ---------

LIB_Rec <- read_excel(
  sprintf(url_file, "GREET2024_Data.xlsx"),
  sheet = "LIB_Recycling",
  col_names = FALSE
)


GWP_factors <- c(CO2 = 1, CH4 = 29.8, N2O = 273)

chemistries <- as.character(unlist(LIB_Rec[2, 2:8]))
emission_names <- as.character(unlist(LIB_Rec[10:20, 1]))
emission_values <- LIB_Rec[10:20, 2:8] %>%
  mutate_all(as.numeric)

emission_matrix <- as.matrix(emission_values)
rownames(emission_matrix) <- emission_names
colnames(emission_matrix) <- chemistries

# GHG rows
GHG_rows <- intersect(names(GWP_factors), rownames(emission_matrix))
GHG_data <- emission_matrix[GHG_rows, , drop = FALSE]

# Apply GWP multipliers
for (gas in GHG_rows) {
  GHG_data[gas, ] <- GHG_data[gas, ] * GWP_factors[gas]
}

LIBRec_GWP <- as.data.frame(t(GHG_data))
LIBRec_GWP$Total_GWP_g_per_ton <- rowSums(LIBRec_GWP)
LIBRec_GWP <- tibble::rownames_to_column(LIBRec_GWP, var = "Chemistry")

write_csv(LIBRec_GWP, "Inputs/LIBRec_GWP.csv")

# LIB Extraction ---------

Li_Extraction <- read_excel(
  sprintf(url_file, "GREET2024_Data.xlsx"),
  sheet = "Lithium_Extraction",
  col_names = FALSE
)


GWP_factors <- c(CO2 = 1, CH4 = 29.8, N2O = 273)

# Average Li2CO3 in column v, row 13-25
Li_exact <- Li_Extraction[13:25, ] %>%
  select(name = ...1, value = ...22) %>%
  mutate(
    gwp = GWP_factors[name],
    co2e_g = ifelse(!is.na(gwp), as.numeric(value) * gwp, NA)
  )

write.csv(Li_exact, "Inputs/Li_extract.csv", row.names = FALSE)
