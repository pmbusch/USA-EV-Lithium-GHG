## Analysis of baseline results
# Barplots for primary energy and co2
## PBH Sept 2025

# LOAD Results -------

source("Scripts/02-Load_Results.R")

# baseline scenario
df <- df_all %>%
  filter(
    Scenario_Sales == "Ambitious",
    Scenario_Lifetime == "Reference",
    Scenario_Capacity == "Reference",
    Scenario_Recycling == "Recycling 0%",
    Scenario_mpg == "Reference",
    Scenario_Grid == "ref2025"
  )

# fmt: skip
table(df$Scenario_Sales,df$Scenario_Lifetime,df$Scenario_Capacity,df$Scenario_Grid,df$Scenario_mpg,df$Scenario_Recycling)


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

# Figure -----

unique(df$impact)
dict <- read_excel("Inputs/Dict_Impacts.xlsx")
start_year <- 2025
range(df$Year)

# simplified stages
stage_colors <- viridis::viridis(6, option = "E", direction = -1)
names(stage_colors) <- stage_lvl
df <- df %>%
  mutate(
    Stage = case_when(
      str_detect(Stage, "Recycling|Replacement") ~ "LIB production",
      str_detect(Stage, "Maintenance") ~ "Vehicle production",
      T ~ Stage
    )
  )

total_df <- df %>%
  left_join(dict) %>%
  filter(!is.na(Category)) %>%
  group_by(Category, Impact_Name, Abbr, vehicle_type, Stage) %>%
  reframe(value = sum(value)) %>%
  ungroup() %>% #   mutate(Stage=factor(Stage,levels=stage_lvl)) %>%
  mutate(
    Impact_Name = Impact_Name %>%
      str_replace("Crudeoil", "Crude oil") %>%
      str_replace("Naturalgas", "Natural gas") %>%
      str_replace("Non metal", "Other Materials")
  ) %>%
  mutate(labX = paste0(Impact_Name, "-", vehicle_type))


## Energy -----

# order
energy_levels <- rev(c(
  "Coal",
  "Crude oil",
  "Natural gas",
  "Uranium",
  "Biomass",
  "Hydro",
  "Geothermal",
  "Solar",
  "Wind",
  "Other"
))
comb_lvl <- expand.grid(energy_levels, stage_lvl) %>% mutate(x = paste0(Var1, Var2)) %>% pull(x)

# fuel colors
energy_colors <- fuel_colors
names(energy_colors) <- names(energy_colors) %>%
  str_remove(" Conventional|Wood and Other |power| Photovoltaic") %>%
  str_replace("Petroleum", "Crude oil") %>%
  str_replace("Nuclear", "Uranium") %>%
  str_replace("Gas", "gas")

data_fig2 <- total_df %>%
  filter(Category == "Primary energy detail") %>%
  mutate(value = value / 3.6 / 1e9) %>% # to TWh
  mutate(lvl = factor(paste0(Impact_Name, Stage), levels = comb_lvl)) %>%
  mutate(Impact_Name = factor(Impact_Name, levels = energy_levels)) %>%
  mutate(vehicle_type = factor(vehicle_type)) %>%
  arrange(lvl) %>%
  mutate(abb_lab = if_else(value > 2e3, Abbr, ""))

data_fig2a <- data_fig2 %>%
  group_by(vehicle_type, Stage) %>%
  reframe(value = sum(value)) %>%
  ungroup() %>%
  mutate(Stage = factor(Stage, stage_lvl))

total_lab2 <- total_df %>%
  filter(Category == "Primary energy detail") %>%
  mutate(value = value / 3.6 / 1e9) %>% # to TWh
  group_by(Category, vehicle_type) %>%
  reframe(value = sum(value)) %>%
  ungroup() %>%
  mutate(lab_total = paste0(round(value), " MJ"))

# Same but with non-renewable

non_renew <- c("Coal", "Crude oil", "Natural gas", "Uranium")

data_fig3 <- data_fig2 %>% filter(Impact_Name %in% non_renew)
data_fig3a <- data_fig3 %>%
  group_by(vehicle_type, Stage) %>%
  reframe(value = sum(value)) %>%
  ungroup() %>%
  mutate(Stage = factor(Stage, levels = stage_lvl))

# combine to use same plot: common legend
lvl_facet <- c("Primary Energy", "Non-Renewable Primary Energy")
data_fig3 <- rbind(mutate(data_fig2, x = lvl_facet[1]), mutate(data_fig3, x = lvl_facet[2])) %>%
  mutate(x = factor(x, levels = lvl_facet))
data_fig3a <- rbind(mutate(data_fig2a, x = lvl_facet[1]), mutate(data_fig3a, x = lvl_facet[2])) %>%
  mutate(x = factor(x, levels = lvl_facet)) |>
  mutate(abb_lab = if_else(value > 5e3, Stage, ""), color_text = if_else(str_detect(Stage, "Vehicle"), "whit", "blac"))

# just one panel works best
# data_fig3 <- data_fig3 |> filter(x == "Primary Energy")
# data_fig3a <- data_fig3a |> filter(x == "Primary Energy")

p3 <- ggplot(data_fig3, aes(as.numeric(vehicle_type) + 0.15, value)) +
  geom_col(data = data_fig3a,aes(fill = Stage),position = position_stack(),
    col = "black", linewidth = 0.1,width = 0.3) +
  geom_text(data = data_fig3a,position = position_stack(vjust = 0.5),
    size = 8.5 * 5 / 14 * 0.8,aes(label = abb_lab, group = Stage, col = color_text)) +
  # geom_text(data=total_lab2,aes(label=lab_total),size=8*5/14 * 0.8,
  #           nudge_y = 0)+
  scale_color_manual(values = c("whit" = "white", "blac" = "black")) +
  scale_fill_manual(values = stage_colors) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1), color = "none") +
  ggnewscale::new_scale_fill() + # trick to avoid a Stage fill legend
  ggnewscale::new_scale_color() +
  # by energy
  geom_col(aes(x = as.numeric(vehicle_type) - 0.15, fill = Impact_Name, group = lvl),
    position = position_stack(),col = "black",linewidth = 0.1,width = 0.3) +
  geom_text(position = position_stack(vjust = 0.5),size = 8.5 * 5 / 14 * 0.8,
    aes(x = as.numeric(vehicle_type) - 0.15, label = abb_lab, group = lvl)) +
  facet_wrap(~x, ncol = 1) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = " ", scientific = FALSE),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * 1e9 / vmt_total$vmt / 1.61, name = "kWh per km")
  ) +
  scale_fill_manual(values = energy_colors) +
  scale_x_continuous(breaks = 1:2, labels = c("EV", "ICEV")) +
  labs(x = "", y = "TWh Non-renewable Primary Energy 2025-2050 (whole fleet)", fill = "Energy source", tag = "") +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  theme_bw(9) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = NA, color = NA),
    strip.placement = "outside",
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.y.right = element_text(face = "italic"),
    plot.tag = element_text(face = "bold"),
    legend.text = element_text(size = 8.5),
    legend.key.size = unit(0.2, "cm"),
    axis.title.x.top = element_text(margin = margin(b = -20)),
    legend.box = "vertical",
    legend.position = "bottom"
  )

p3

ggsave("Figures/Fig2.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 8.7 * 1.5)

pdf("Figures/PDF/Figure2.pdf", width = 18 / 2.54, height = 12.4 / 2.54)
ggplot2::last_plot()
dev.off()

# total energy comparison
data_fig3 |>
  group_by(x, vehicle_type) |>
  reframe(value = sum(value)) |>
  pivot_wider(names_from = vehicle_type, values_from = value) |>
  mutate(diff = (EV - ICE) / ICE) |>
  mutate(EV_km = EV * 1e9 / vmt_total$vmt / 1.61, ICE_km = ICE * 1e9 / vmt_total$vmt / 1.61)


# table with numbers
data_fig3 |>
  filter(x == "Primary Energy") |>
  group_by(Impact_Name, vehicle_type) |>
  reframe(value = sum(value)) |>
  pivot_wider(names_from = vehicle_type, values_from = value)

# 1 oil barrel: 1700 kWh
70000 * 1e9 / 1700 / 1e6

# Stage accounting
data_fig3 |>
  filter(x == "Primary Energy") |>
  group_by(Stage, vehicle_type) |>
  reframe(value = sum(value)) |>
  pivot_wider(names_from = vehicle_type, values_from = value) |>
  mutate(perc_EV = EV / sum(EV), perc_ICE = ICE / sum(ICE, na.rm = T))


# OLD -----

ggplot(total_df, aes(labX, value)) +
  geom_col(data = filter(total_df, vehicle_type == "EV"),
    aes(fill = Stage),
    position = position_stack(),
    col = "black",
    linewidth = 0.1,
    width = 0.3
  ) +
  geom_col(
    data = filter(total_df, vehicle_type == "ICE"),
    aes(fill = Stage),
    position = position_stack(),
    col = "black",
    linewidth = 0.1,
    width = 0.3
  ) +
  # geom_text(data=total_type_p,aes(label=lab_total),size=8*5/14 * 0.8,
  #           nudge_y = 1000)+
  facet_wrap(~Category, scales = "free") +
  coord_flip() +
  scale_y_continuous(labels = function(x) {
    format(x, big.mark = " ", scientific = FALSE)
  }) +
  scale_fill_viridis_d(direction = -1, option = "E") +
  labs(x = "", y = "", title = paste0("All in tons or TJ", start_year, "-2050")) +
  theme_bw(8) +
  theme(panel.grid = element_blank())

ggsave("Figures/fleet_impacts.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7, height = 8.7)


# Common sense checks ----

unique(df$impact)

## Total emissions by year -----
df %>%
  filter(Year %in% c(2030, 2040, 2050)) %>%
  filter(impact == "kgCO2eq") %>%
  group_by(vehicle_type, Year) %>%
  reframe(mtons = sum(value) / 1e9) %>%
  pivot_wider(names_from = Year, values_from = mtons)

# For reference, US entire transport sector emissions in 2022: 1800 Mtons CO2e
# source: https://cfpub.epa.gov/ghgdata/inventoryexplorer/#transportation/entiresector/allgas/category/current

# by vmt
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


vmt[2, 4] / 1e6 # trillion
vmt[22, 4] / 1e6 # trillion

# 2023, 3.19 trillion miles in USA
# https://afdc.energy.gov/data/10315
vmt_total <- vmt %>% group_by(Scenario_Sales, Scenario_Lifetime) %>% reframe(vmt = sum(vmt) * 1e6) # total miles
vmt_total <- vmt_total %>% filter(Scenario_Sales == "Ambitious", Scenario_Lifetime == "Reference")


## GHG intensity (gr CO2 per mile) ----
df %>%
  filter(impact == "kgCO2eq") %>%
  group_by(Scenario_Sales, Scenario_Lifetime, vehicle_type) %>%
  reframe(CI = sum(value) * 1e3) %>%
  ungroup() %>% # to grams
  left_join(vmt_total) %>%
  mutate(CI = CI / vmt, CI_km = CI / 1.609)
# 163 g/mile for EV, 310 for ICE , same ballpark as https://www.nature.com/articles/s41598-024-51697-1

# by size
df %>%
  group_by(Scenario_Sales, Scenario_Lifetime, vehicle_type, vehSize) %>%
  reframe(CI = sum(tonsCO2e) * 1e3) %>%
  ungroup() %>% # to grams
  left_join(vmt_size) %>%
  mutate(CI = CI / vmt, CI_km = CI / 1.609)


##  By kg lithium ------
total_type <- df %>%
  group_by(Scenario, vehicle_type) %>%
  reframe(tonsCO2e = sum(tonsCO2e) / 1e6) %>%
  ungroup() %>%
  mutate(lab_total = paste0(round(tonsCO2e, 0), " Mtons"))

# load lithium
mineral_demand <- read.csv("Results/MineralDemand.csv")
mineral_demand <- mineral_demand %>% filter(Year >= start_year)

lithium_demand <- mineral_demand %>%
  filter(Mineral == "Lithium", scen_recyc == "Recycling 5%") %>%
  group_by(Scenario) %>%
  reframe(mtons = sum(Mineral_tons) / 1e6) %>%
  ungroup()

# reduction in ton CO2e per ton of lithium
li_red <- total_type %>%
  dplyr::select(-lab_total) %>%
  pivot_wider(names_from = vehicle_type, values_from = tonsCO2e) %>%
  mutate(red = ICE - EV) %>%
  left_join(lithium_demand) %>%
  mutate(red_pkgLithium = red / mtons)
li_red
# ~4.1 tons CO2e avoided per kg of lithium
# why allocate everything to lithium????
ggplot(li_red, aes(red, mtons)) + geom_point()

# based on value by content
(li_value <- read.csv("Results/MineralValue.csv"))
li_red %>% mutate(red_pkgLithium = red_pkgLithium * li_value[4, 4])
# ~145 kg CO2e per kg Lithium extracted

## By kWh battery pack -----
# Reduction by kWh of battery pack - units are in MWh
total_kwh <- mineral_demand %>%
  filter(Mineral == "kWh", scen_recyc == "Recycling 5%") %>%
  group_by(Scenario) %>%
  reframe(kWh = sum(Mineral_tons) * 1e3) %>%
  ungroup()

kwh_red <- total_type %>%
  dplyr::select(-lab_total) %>%
  pivot_wider(names_from = vehicle_type, values_from = tonsCO2e) %>%
  mutate(red = ICE - EV) %>%
  left_join(total_kwh) %>%
  mutate(red_pkWh = red * 1e9 / kWh) #from mtons to kg
kwh_red
# ~400 kg CO2 per kWh

## by kg Mineral ------
# Reduction per kg of critical Mineral - Li,Ni,Co,Graphite,Cu
total_mineral <- mineral_demand %>%
  filter(Mineral != "kWh", scen_recyc == "Recycling 5%") %>%
  group_by(Scenario) %>%
  reframe(mtons = sum(Mineral_tons) / 1e6)

min_red <- total_type %>%
  dplyr::select(-lab_total) %>%
  pivot_wider(names_from = vehicle_type, values_from = tonsCO2e) %>%
  mutate(red = ICE - EV) %>%
  left_join(total_mineral) %>%
  mutate(red_kg_per_kg = red / mtons)
min_red
# ~200 kg per kg of critical mineral (Li,Ni,Cu,Co,Graphite)

## FIGURE million tons -----
total_df <- df %>%
  filter(impact == "kgCO2eq") %>%
  group_by(vehicle_type, Stage) %>%
  reframe(tonsCO2e = sum(value) / 1e9) %>%
  ungroup() %>%
  mutate(Stage = factor(Stage, levels = stage_lvl))

total_type_p <- total_type %>% filter(Scenario %in% c("Ambitious", "Ambitious-Reference"))

start_year <- 2024
ggplot(total_df, aes(vehicle_type, tonsCO2e)) +
  geom_col(aes(fill = Stage), position = position_stack(), col = "black", linewidth = 0.1) +
  # geom_text(data=total_type_p,aes(label=lab_total),size=8*5/14 * 0.8,
  #           nudge_y = 1000)+
  # facet_wrap(~Scenario)+
  scale_y_continuous(labels = function(x) {
    format(x, big.mark = " ", scientific = FALSE)
  }) +
  scale_fill_viridis_d(direction = -1, option = "E") +
  labs(x = "", y = "", title = paste0("Million tons CO2e emissions, ", start_year, "-2050")) +
  theme_bw(8) +
  theme(panel.grid = element_blank())

ggsave("Figures/GHG_fleet.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7, height = 8.7)

# over time
df %>%
  group_by(Scenario, Year, vehicle_type, Stage) %>%
  reframe(tonsCO2e = sum(tonsCO2e)) %>%
  ungroup() %>%
  mutate(tonsCO2e = tonsCO2e / 1e6) %>%
  ggplot(aes(Year, tonsCO2e, fill = Stage)) +
  geom_area() +
  facet_grid(vehicle_type ~ Scenario) +
  coord_cartesian(expand = F) +
  scale_y_continuous(labels = function(x) {
    format(x, big.mark = " ", scientific = FALSE)
  }) +
  scale_fill_viridis_d(direction = -1, option = "E") +
  scale_x_continuous(breaks = c(2024, 2030, 2040, 2050)) +
  labs(x = "", y = "", title = "Million tons CO2e emissions") +
  theme_bw(8) +
  theme(panel.grid = element_blank(), panel.spacing = unit(1, "cm"))

ggsave("Figures/GHG_fleet_time.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)

# By stage and vehicle size
stage_lvl2 <- expand.grid(stage_lvl, c("Car", "Light Truck")) %>% mutate(x = paste0(Var2, "-", Var1)) %>% pull(x)

# million tons
total_df <- df %>%
  filter(Scenario == "Ambitious") %>%
  group_by(Scenario, vehicle_type, Stage, vehSize) %>%
  reframe(tonsCO2e = sum(tonsCO2e) / 1e6) %>%
  ungroup() %>%
  mutate(Stage = paste0(vehSize, "-", Stage), Stage = factor(Stage, levels = stage_lvl2))

ggplot(total_df, aes(vehicle_type, tonsCO2e)) +
  geom_col(aes(fill = Stage), position = position_stack(), col = "black", linewidth = 0.1) +
  geom_text(data = total_type_p, aes(label = lab_total), size = 8 * 5 / 14 * 0.8, nudge_y = 1000) +
  # facet_wrap(~Scenario)+
  scale_y_continuous(labels = function(x) {
    format(x, big.mark = " ", scientific = FALSE)
  }) +
  scale_fill_viridis_d(direction = -1, option = "E") +
  labs(x = "", y = "", title = paste0("Million tons CO2e emissions, ", start_year, "-2050")) +
  theme_bw(8) +
  theme(panel.grid = element_blank())

ggsave("Figures/GHG_fleet_type.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7, height = 8.7)
