## GREET Comparison Figure for Supplementary Information
# GHG only
# PBH Nov 2025

# Baseline results ------
source("Scripts/02-Load_Results.R")

## baseline scenario
df <- df_all %>%
  filter(
    Scenario_Sales == "Ambitious",
    Scenario_Lifetime == "Reference",
    Scenario_Capacity == "Reference",
    Scenario_Recycling == "Recycling 0%",
    Scenario_mpg == "Reference",
    Scenario_Grid == "ref2025"
  )

# only CO2e
unique(df$impact)
df <- df %>%
  filter(impact == "kgCO2eq") %>%
  group_by(Year, vehicle_type, vehSize, Stage) %>%
  reframe(tonsCO2e = sum(value) / 1000) %>%
  ungroup()

## VMT  ------
vmt <- read.csv("Results/total_VMT.csv")
vmt <- vmt %>%
  mutate(
    Scenario_Lifetime = str_extract(Scenario, "Reference|Short|Long"),
    Scenario_Sales = str_extract(Scenario, "Momentum|Ambitious"),
    Scenario = NULL
  ) %>%
  filter(Scenario_Sales != "Baseline")
vmt_size <- vmt %>% group_by(Scenario_Sales, Scenario_Lifetime, vehSize) %>% reframe(vmt = sum(total_vmt))

vmt <- vmt %>% group_by(Scenario_Sales, Scenario_Lifetime, Year) %>% reframe(vmt = sum(total_vmt) / 1e6) %>% ungroup()

# 2023, 3.19 trillion miles in USA
# https://afdc.energy.gov/data/10315
vmt_total <- vmt %>% group_by(Scenario_Sales, Scenario_Lifetime) %>% reframe(vmt = sum(vmt) * 1e6) # total miles
vmt_total <- vmt_total %>% filter(Scenario_Sales == "Ambitious", Scenario_Lifetime == "Reference")


# GREET ------
veh_prod <- read.csv("Results/GREET/veh_prod.csv")
lib_prod <- read.csv("Results/GREET/lib_prod.csv")
LIB_replacement <- read.csv("Results/GREET/LIB_replacement.csv")
ice_usage <- read.csv("Results/GREET/ice_usage.csv")
amort_total <- read.csv("Results/GREET/amort_veh.csv")
amort_lib_total <- read.csv("Results/GREET/amort_LIB.csv")

# join, use EV usage, maintenance, vehicle recycling from ecoinvent
ev_usage <- read.csv("Results/ev_usage.csv") |>
  filter(Scenario_Sales == "Ambitious", Scenario_Lifetime == "Reference", Scenario_Grid == "ref2025") |>
  rename(Scenario = Scenario_Sales) |>
  group_by(Scenario, Year, vehSize, Stage) |>
  reframe(tonsCO2e = sum(kgCO2eq) / 1000) |>
  ungroup() |>
  mutate(vehicle_type = "EV")

veh_maintenance <- read.csv("Results/veh_maintenance.csv") |>
  filter(Scenario_Sales == "Ambitious", Scenario_Lifetime == "Reference") |>
  rename(Scenario = Scenario_Sales) |>
  group_by(Scenario, Year, vehicle_type, vehSize, Stage) |>
  reframe(tonsCO2e = sum(vehMain_kgCO2eq) / 1000) |>
  ungroup()

veh_recycling <- read.csv("Results/veh_recycling.csv") |>
  filter(Scenario_Sales == "Ambitious", Scenario_Lifetime == "Reference") |>
  rename(Scenario = Scenario_Sales) |>
  group_by(Scenario, Year, vehicle_type, vehSize, Stage) |>
  reframe(tonsCO2e = sum(VehRecyc_kgCO2eq) / 1000) |>
  ungroup()


greet <- rbind(
  veh_prod,
  lib_prod,
  LIB_replacement,
  ice_usage,
  amort_total,
  amort_lib_total,
  ev_usage,
  veh_maintenance,
  veh_recycling
)

# Comparison Results -----------------
data_fig <- rbind(mutate(greet, Model = "GREET") |> dplyr::select(-Scenario), mutate(df, Model = "ecoinvent"))

data_fig <- data_fig |>
  mutate(
    Stage = case_when(
      str_detect(Stage, "Recycling|Replacement") ~ "LIB production",
      str_detect(Stage, "Maintenance") ~ "Vehicle production",
      T ~ Stage
    ) |>
      factor(levels = stage_lvl)
  )

data_fig <- data_fig |>
  group_by(Model, vehicle_type, Stage) |>
  reframe(tonsCO2e = sum(tonsCO2e) / 1e6) |> #million
  ungroup()

stage_colors <- viridis::viridis(6, option = "E", direction = -1)
names(stage_colors) <- stage_lvl

data_fig |>
  # filter(Stage != "Driving") |>
  mutate(vehicle_type = if_else(vehicle_type == "ICE", "ICEV", vehicle_type)) |>
  ggplot(aes(Model, tonsCO2e, fill = Stage)) +
  geom_col(col="black",linewidth = 0.1) +
  facet_wrap(~vehicle_type, ncol = 1) +
  coord_flip() +
  scale_fill_manual(values = stage_colors) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = " ", scientific = FALSE),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * 1e6 * 1e6 / vmt_total$vmt / 1.61, name = expression("grams " ~ CO[2] * e ~ " per km"))
  ) +
  labs(x = "", y = expression("" ~ million ~ tons ~ CO[2] * eq * "")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw(8) +
  theme(panel.grid = element_blank(), legend.position = "bottom")

ggsave("Figures/FigGREET.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 14, height = 8.7)

# Totals
data_fig |>
  group_by(Model, vehicle_type) |>
  reframe(value = sum(tonsCO2e)) |>
  pivot_wider(names_from = vehicle_type, values_from = value) |>
  mutate(diff = (EV - ICE) / ICE) |>
  mutate(EV_km = EV * 1e12 / vmt_total$vmt / 1.61, ICE_km = ICE * 1e12 / vmt_total$vmt / 1.61)

# Stage accounting
data_fig |>
  filter(Model == "GREET") |>
  group_by(Stage, vehicle_type) |>
  reframe(value = sum(tonsCO2e, na.rm = T)) |>
  pivot_wider(names_from = vehicle_type, values_from = value) |>
  mutate(perc_EV = EV / sum(EV), perc_ICE = ICE / sum(ICE, na.rm = T))

# EoF
