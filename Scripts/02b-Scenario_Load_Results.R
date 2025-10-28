# 02b - Scenario_Load_Results
# Load results in repeated format for scenario analysis and comparison

source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")

# LOAD Results -------

veh_prod <- read.csv("Results/veh_prod.csv") # by Sales Scenario
veh_maintenance <- read.csv("Results/veh_maintenance.csv") # by Sales and Lifetime Scenario
lib_prod <- read.csv("Results/lib_prod.csv") # by Sales and Capacity Scenario
LIB_replacement <- read.csv("Results/LIB_replacement.csv") # by Sales, Capacity and Lifetime Scenario
ev_usage <- read.csv("Results/ev_usage.csv") # by Sales, Lifetime and Grid Scenario
ice_usage <- read.csv("Results/ice_usage.csv") # by Sales and Lifetime Scenario
LIB_recycling <- read.csv("Results/LIB_recycling.csv") # by Sales, Capacity and Lifetime Scenario
veh_recycling <- read.csv("Results/veh_recycling.csv") # by Sales and Lifetime Scenario

amort_total <- read.csv("Results/amort_veh.csv")
amort_lib_total <- read.csv("Results/amort_LIB.csv")

# kWh
kwh_total <- read.csv("Results/kwh.csv")
kwh_recycling_total <- read.csv("Results/kwh_recyc.csv")


# Change names
(names(veh_prod) <- names(veh_prod) %>% str_remove("vehProd_"))
(names(veh_maintenance) <- names(veh_maintenance) %>% str_remove("vehMain_"))
(names(lib_prod) <- names(lib_prod) %>% str_remove("LIB_"))
(names(LIB_replacement) <- names(LIB_replacement) %>% str_remove("LIB_"))
# names(ev_usage) <- names(ev_usage) %>% str_remove("")
# names(ice_usage) <- names(ice_usage) %>% str_remove("")
(names(LIB_recycling) <- names(LIB_recycling) %>% str_remove("LIBRecyc_"))
(names(veh_recycling) <- names(veh_recycling) %>% str_remove("VehRecyc_"))
(names(amort_total) <- names(amort_total) %>% str_remove("vehProd_"))
(names(amort_lib_total) <- names(amort_lib_total) %>% str_remove("LIB_"))


# scenario of grid to include
scens_grid <- c("ref2025", "Cambium", "highogs", "lowogs")

# Flat them and join them - expanding missing scenarios
f.flat.exp <- function(df) {
  names_scen <- paste0(names(df), collapse = ",")

  # EXPAND DF to include all other scenario, no grid...
  if (!str_detect(names_scen, "Scenario_Lifetime")) {
    df_aux <- c()
    for (i in unique(LIB_replacement$Scenario_Lifetime)) {
      df$Scenario_Lifetime <- i
      df_aux <- rbind(df_aux, df)
    }
    df <- df_aux
  }

  if (!str_detect(names_scen, "Scenario_Capacity")) {
    df_aux <- c()
    for (i in unique(LIB_replacement$Scenario_Capacity)) {
      df$Scenario_Capacity <- i
      df_aux <- rbind(df_aux, df)
    }
    df <- df_aux
  }

  if (!str_detect(names_scen, "Scenario_Grid")) {
    df_aux <- c()
    for (i in scens_grid) {
      df$Scenario_Grid <- i
      df_aux <- rbind(df_aux, df)
    }
    df <- df_aux
  }

  if (!str_detect(names_scen, "Scenario_mpg")) {
    df_aux <- c()
    for (i in unique(ice_usage$Scenario_mpg)) {
      df$Scenario_mpg <- i
      df_aux <- rbind(df_aux, df)
    }
    df <- df_aux
  }

  if (!str_detect(names_scen, "Scenario_Recycling")) {
    df_aux <- c()
    for (i in unique(LIB_recycling$Scenario_Recycling)) {
      df$Scenario_Recycling <- i
      df_aux <- rbind(df_aux, df)
    }
    df <- df_aux
  }

  df <- df %>%
    pivot_longer(
      -c(
        Scenario_Sales,
        Scenario_Lifetime,
        Scenario_Capacity,
        Scenario_Recycling,
        Scenario_Grid,
        Scenario_mpg,
        Year,
        vehSize,
        vehicle_type,
        Stage
      ),
      names_to = "impact",
      values_to = "value"
    )
  return(df)
}

df_all_scen <- rbind(
  f.flat.exp(veh_prod),
  f.flat.exp(veh_maintenance),
  f.flat.exp(lib_prod),
  f.flat.exp(LIB_replacement),
  f.flat.exp(ev_usage),
  f.flat.exp(ice_usage),
  f.flat.exp(amort_total),
  f.flat.exp(amort_lib_total),
  f.flat.exp(LIB_recycling),
  f.flat.exp(veh_recycling)
)
# fmt: skip
rm(veh_prod,veh_maintenance,lib_prod,LIB_replacement,ev_usage,ice_usage,amort_total,amort_lib_total,LIB_recycling)

nrow(df_all_scen) / 1e6 # 7m

stage_lvl <- c(
  "LIB Recycling",
  "Driving",
  "LIB Replacement",
  "LIB production",
  "Vehicle Maintenance",
  "Vehicle production"
)
df_all_scen <- df_all_scen %>%
  mutate(Stage = factor(Stage, levels = stage_lvl)) |>
  filter(Scenario_Grid %in% scens_grid)

# EoF
