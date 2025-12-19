## Cambium Long Term Marginal Electricity Emissions
# PBH Nov 2025

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
  group_by(Year, vehicle_type, Stage) %>%
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


# Cambium long run driving ------
ev_usage <- read.csv("Results/Cambium/ev_usage.csv")

cam <- df |>
  filter(!(Stage == "Driving" & vehicle_type == "EV")) |>
  filter(vehicle_type == "EV") |>
  rbind(mutate(ev_usage, Stage = "Driving"))

# Cambium average grid mix
cam_avg <- df_all |>
  filter(
    Scenario_Sales == "Ambitious",
    Scenario_Lifetime == "Reference",
    Scenario_Capacity == "Reference",
    Scenario_Recycling == "Recycling 0%",
    Scenario_mpg == "Reference",
    Scenario_Grid == "Cambium",
    vehicle_type == "EV",
    Stage == "Driving",
    impact == "kgCO2eq"
  ) |>
  group_by(Year, vehicle_type, Stage) %>%
  reframe(tonsCO2e = sum(value) / 1000) %>%
  ungroup()

cam_avg <- df |>
  filter(!(Stage == "Driving" & vehicle_type == "EV")) |>
  filter(vehicle_type == "EV") |>
  rbind(mutate(cam_avg, Stage = "Driving"))

# Comparison Results -----------------
data_fig <- rbind(
  mutate(cam, Model = "Long-Run Marginal Cambium"),
  mutate(df, Model = "Avg. Grid Mix EIA"),
  mutate(cam_avg, Model = "Avg. Grid Mix Cambium")
)

data_fig <- data_fig |>
  mutate(Model = if_else(vehicle_type == "ICE", "ICEV", Model)) |>
  mutate(
    Model = factor(
      Model,
      levels = rev(c("ICEV", "Avg. Grid Mix EIA", "Avg. Grid Mix Cambium", "Long-Run Marginal Cambium"))
    )
  )

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
  ggh4x::facet_grid2(vehicle_type ~ ., scales = "free_y", axes = "all", space = "free_y") +
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

ggsave("Figures/FigCambium.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 14, height = 8.7)

# Totals
data_fig |>
  group_by(Model) |>
  reframe(value = sum(tonsCO2e)) |>
  pivot_wider(names_from = Model, values_from = value) |>
  rename(EV = `Long-Run Marginal Cambium`) |>
  mutate(diff = (EV - ICEV) / ICEV) |>
  mutate(EV_km = EV * 1e12 / vmt_total$vmt / 1.61, ICE_km = ICEV * 1e12 / vmt_total$vmt / 1.61)

# Stage accounting
data_fig |>
  filter(Model == "Long-Run Marginal Cambium") |>
  group_by(Stage, vehicle_type) |>
  reframe(value = sum(tonsCO2e, na.rm = T)) |>
  pivot_wider(names_from = vehicle_type, values_from = value) |>
  mutate(perc_EV = EV / sum(EV))

# EoF
