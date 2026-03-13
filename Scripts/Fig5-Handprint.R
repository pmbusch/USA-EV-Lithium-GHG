# Handprint (avoided emissions) for lithium-ion batteries and lithium
# Heatmap for key sensitivity
# PBH OCt 2025

source("Scripts/02b-Scenario_Load_Results.R")

head(df_all_scen)
unique(df_all_scen$Scenario_Grid)

# Figure -----
dict_grid <- read.csv("Inputs/Join_EIAScenarios.csv")

unique(df_all_scen$impact)

key_impacts <- c("kg_Crudeoil", "kg_Coal", "kg_Naturalgas", "MJ_Crudeoil", "MJ_Coal", "MJ_Naturalgas")
df <- df_all_scen %>%
  filter(impact %in% c("kgCO2eq", "kgLithium", key_impacts)) %>%
  filter(Scenario_Sales == "Ambitious") %>%
  group_by(
    Scenario_Sales,
    Scenario_Lifetime,
    Scenario_Capacity,
    Scenario_Grid,
    Scenario_mpg,
    Scenario_Recycling,
    vehicle_type,
    impact
  ) %>%
  reframe(value = sum(value)) %>%
  left_join(dict_grid, by = c("Scenario_Grid" = "scenario")) %>%
  mutate(Scenario_Grid = scenarioDescription, scenarioDescription = NULL)

# fmt: skip
table(df$Scenario_Sales,df$Scenario_Lifetime,df$Scenario_Capacity,df$Scenario_Grid,df$Scenario_mpg,df$Scenario_Recycling)

# get delta
head(df)
df <- df %>%
  pivot_wider(names_from = c(vehicle_type, impact), values_from = value) %>%
  mutate(
    delta_co2 = (EV_kgCO2eq - ICE_kgCO2eq) / 1e3, # to tons
    delta_li = EV_kgLithium - ICE_kgLithium,
    delta_coal_kg = (EV_kg_Coal - ICE_kg_Coal),
    delta_oil_kg = (EV_kg_Crudeoil - ICE_kg_Crudeoil),
    delta_gas_kg = (EV_kg_Naturalgas - ICE_kg_Naturalgas),
    delta_coal_mj = (EV_MJ_Coal - ICE_MJ_Coal),
    delta_oil_mj = (EV_MJ_Crudeoil - ICE_MJ_Crudeoil),
    delta_gas_mj = (EV_MJ_Naturalgas - ICE_MJ_Naturalgas)
  ) %>%
  mutate(
    metric = -delta_co2 / delta_li,
    metric_coal_kg = -delta_coal_kg / delta_li,
    metric_oil_kg = -delta_oil_kg / delta_li,
    metric_gas_kg = -delta_gas_kg / delta_li,
    metric_coal_mj = -delta_coal_mj / delta_li,
    metric_oil_mj = -delta_oil_mj / delta_li,
    metric_gas_mj = -delta_gas_mj / delta_li
  )

df <- df %>%
  mutate(
    Scenario_Recycling = str_remove(Scenario_Recycling, "Recycling ") %>%
      factor(levels = c("0%", "20%", "40%", "60%", "80%"))
  ) %>%
  mutate(
    Scenario_Capacity = Scenario_Capacity %>%
      str_to_title() %>%
      str_replace("_capacity", " LIB Capacity") %>%
      factor(levels = c("Low LIB Capacity", "Reference", "High LIB Capacity"))
  ) %>%
  mutate(
    Scenario_Lifetime = Scenario_Lifetime %>%
      str_replace("Long", "Long LIB Duration") %>%
      str_replace("Short", "Short LIB Duration") %>%
      factor(levels = c("Short LIB Duration", "Reference", "Long LIB Duration"))
  ) %>%
  mutate(Scenario_Grid = factor(Scenario_Grid)) |>
  mutate(
    Scenario_mpg = Scenario_mpg |>
      str_replace("Improved", "Improved MPG") |>
      factor(levels = c("Reference", "Improved MPG"))
  )


range_metric <- range(df$metric)

# Reference
df |>
  filter(
    Scenario_Grid == "Reference case",
    Scenario_Capacity == "Reference",
    Scenario_Lifetime == "Reference",
    Scenario_Recycling == "0%",
    Scenario_mpg == "Reference"
  ) |>
  pull(metric)

# Case no Recyc
(mid1 <- df |>
  filter(
    Scenario_Grid == "Reference case",
    Scenario_Capacity == "Reference",
    Scenario_Recycling == "0%",
    Scenario_Lifetime == "Reference",
    Scenario_mpg == "Reference"
  ) |>
  pull(metric))

# Save Figure Data
write.csv(df, "Results/Data Figures/Fig5b.csv", row.names = FALSE)

p_li <- df |>
  filter(Scenario_mpg == "Reference") |> # little change
  filter(Scenario_Recycling %in% c("0%", "40%", "80%")) |>
  ggplot(aes(Scenario_Capacity, Scenario_Grid, fill = metric)) +
  geom_tile(color = "grey80") +
  facet_grid(Scenario_Recycling ~ Scenario_Lifetime) +
  scico::scale_fill_scico(palette = "vik", direction = -1, midpoint = mid1) +
  labs(
    y = "Electricity Grid Forecast",
    x = "",
    col = "",
    fill = expression("tons " ~ CO[2] * e ~ " avoided per kg Lithium"),
    tag = "(b)",
    title = "Carbon Handprint of Lithium"
  ) +
  scale_y_discrete(sec.axis = dup_axis(name = "Lithium Recycling rate")) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.tag = element_text(face = "bold"),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.line.y.right = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


df$metric_coal_mj / df$metric_coal_kg
df$metric_gas_mj / df$metric_gas_kg
df$metric_oil_mj / df$metric_oil_kg

data_fig_fuels <- df |>
  filter(Scenario_mpg == "Reference") |>
  filter(Scenario_Grid == "Reference case") |>
  filter(Scenario_Recycling %in% c("0%", "80%")) |>
  filter(Scenario_Capacity == "Reference") |>
  filter(Scenario_Lifetime == "Reference") |>
  dplyr::select(
    Scenario_Recycling,
    metric_coal_kg,
    metric_gas_kg,
    metric_oil_kg,
    metric_coal_mj,
    metric_gas_mj,
    metric_oil_mj
  ) |>
  pivot_longer(
    c(metric_coal_kg, metric_gas_kg, metric_oil_kg, metric_coal_mj, metric_gas_mj, metric_oil_mj),
    names_to = 'key',
    values_to = 'value'
  ) |>
  mutate(source = str_extract(key, "kg|mj")) |>
  mutate(energy = str_extract(key, "coal|gas|oil") |> str_to_title() |> str_replace("Gas", "NG")) |>
  mutate(key = NULL) |>
  pivot_wider(names_from = source, values_from = value)

# Figure (added to SI)
base_size <- 8
p_others <- data_fig_fuels |>
  mutate(label = sprintf("%.0f kg\n(~%.1f GJ)", kg, mj / 1000)) |>
  mutate(label_nudge = ifelse(kg > 0, 1000, -1000), energy_nudge = ifelse(kg > 0, -500, 500)) |>
  ggplot(aes(x = Scenario_Recycling, y = kg, fill = energy)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.2) +
  geom_col(width=0.65,linewidth = 0.1,col="black", position = position_dodge(width = 0.7)) +
  geom_text(
    aes(label = label,y=kg+label_nudge),
    position = position_dodge(width = 0.7),
    size = 6 * 5 / 14 * 0.8
  ) +
  geom_text(
    aes(label = energy,y=energy_nudge),
    position = position_dodge(width = 0.7),
    size = 6 * 5 / 14 * 0.8,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("Coal" = "#8c564b", "Oil" = "#9467bd", "NG" = "#CD7F32")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  labs(
    x = "Recycling scenario",
    y = "Fossil fuel (kg) avoided per kg lithium",
    fill = "Energy source",
    tag = "(b)",
    title = "Fossil fuel handprint of Lithium"
  ) +
  coord_flip(ylim = c(-3e3, 8e3)) +
  theme_bw(base_size = base_size) +
  theme(
    text = element_text(colour = "black"),
    plot.title = element_text(size = base_size, face = "bold"),
    plot.subtitle = element_text(size = base_size, hjust = 0),
    plot.caption = element_text(size = base_size - 2, hjust = 1, lineheight = 0.9),
    plot.tag = element_text(size = base_size + 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.title = element_text(size = base_size - 1.5),
    axis.text = element_text(size = base_size - 1.5),
    axis.ticks = element_line(size = 0.35, colour = "black"),
    legend.position = "none"
  )


# LIB ----

# per kWh, in text
df_kwh <- df_all_scen %>%
  filter(impact %in% c("kgCO2eq", key_impacts)) %>%
  filter(Scenario_Grid %in% scens_grid) %>%
  filter(Scenario_Sales == "Ambitious") %>%
  filter(Scenario_Recycling == "Recycling 0%") %>%
  filter(Scenario_mpg == "Reference") |>
  group_by(Scenario_Grid, Scenario_Lifetime, Scenario_Capacity, vehicle_type, impact) %>%
  reframe(value = sum(value)) %>%
  left_join(dict_grid, by = c("Scenario_Grid" = "scenario")) %>%
  mutate(Scenario_Grid = scenarioDescription, scenarioDescription = NULL)


kwh <- kwh_total %>%
  filter(Scenario_Sales == "Ambitious") %>%
  group_by(Scenario_Lifetime, Scenario_Capacity, vehicle_type) %>%
  reframe(kwh = sum(kwh)) %>%
  ungroup()

data_fig2_all <- df_kwh %>%
  pivot_wider(names_from = c(vehicle_type), values_from = value) %>%
  left_join(kwh) %>%
  mutate(delta_co2 = (ICE - EV) / 1e3) %>% # to tons
  mutate(metric = delta_co2 / kwh) # tons per kWh

data_fig2 <- data_fig2_all %>%
  filter(impact == "kgCO2eq") %>%
  mutate(
    Scenario_Capacity = Scenario_Capacity %>%
      str_to_title() %>%
      str_replace("_capacity", " LIB Capacity") %>%
      factor(levels = c("Low LIB Capacity", "Reference", "High LIB Capacity"))
  ) %>%
  mutate(
    Scenario_Lifetime = Scenario_Lifetime %>%
      str_replace("Long", "Long LIB Duration") %>%
      str_replace("Short", "Short LIB Duration") %>%
      factor(levels = c("Short LIB Duration", "Reference", "Long LIB Duration"))
  ) %>%
  mutate(Scenario_Grid = factor(Scenario_Grid))

(range_metric <- range(data_fig2$metric))

# midpoint
(mid2 <- data_fig2 |>
  filter(Scenario_Grid == "Reference case", Scenario_Capacity == "Reference", Scenario_Lifetime == "Reference") |>
  pull(metric))

# Save Figure Data
write.csv(data_fig2, "Results/Data Figures/Fig5a.csv", row.names = FALSE)

p_lib <- ggplot(data_fig2, aes(Scenario_Capacity, Scenario_Grid, fill = metric)) +
  geom_tile(color = "grey80") +
  facet_grid(~Scenario_Lifetime) +
  scico::scale_fill_scico(palette = "broc", direction = -1, midpoint = mid2) +
  # scale_fill_gradient(low = "#fce5d7", high = "#c64b55") +
  labs(x = "", y = "", col = "", fill = "tons CO2e avoided per kWh of Lithium-ion battery produced") +
  labs(
    y = "Electricity Grid Forecast",
    x = "",
    col = "",
    fill = expression("tons " ~ CO[2] * e ~ " avoided per kWh of Lithium-ion battery produced"),
    tag = "(a)",
    title = "Carbon Handprint of Lithium-ion Batteries"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.tag = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

cowplot::plot_grid(p_lib, p_li, ncol = 1, rel_heights = c(0.4, 0.6))

ggsave("Figures/Fig5.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 17.4)

pdf("Figures/PDF/Figure5.pdf", width = 18 / 2.54, height = 17.4 / 2.54)
ggplot2::last_plot()
dev.off()

# Other metal figures
# Figure (added to SI)

data_fig_fuels2 <- data_fig2_all |>
  filter(impact != "kgCO2eq") |>
  filter(Scenario_Grid == "Reference case") |>
  # filter(Scenario_Capacity == "Reference") |>
  filter(Scenario_Lifetime == "Reference") |>
  dplyr::select(-Scenario_Grid, -Scenario_Lifetime) |>
  mutate(source = str_extract(impact, "kg|MJ")) |>
  mutate(energy = str_extract(impact, "Coal|gas|oil") |> str_to_title() |> str_replace("Gas", "NG")) |>
  mutate(impact = NULL, delta_co2 = NULL, kwh = NULL, vehicle_type = NULL, EV = NULL, ICE = NULL) |>
  mutate(metric = metric * 1e3) |> #back to kg
  pivot_wider(names_from = source, values_from = metric) |>
  mutate(
    Scenario_Capacity = Scenario_Capacity %>%
      str_to_title() %>%
      str_replace("_capacity", " LIB Capacity") %>%
      factor(levels = c("Low LIB Capacity", "Reference", "High LIB Capacity"))
  )

p_others_LIB <- data_fig_fuels2 |>
  mutate(label = sprintf("%.0f kg\n(~%.1f GJ)", kg, MJ / 1000)) |>
  mutate(label_nudge = ifelse(kg > 0, 100, -100), energy_nudge = ifelse(kg > 0, -50, 50)) |>
  ggplot(aes(x = Scenario_Capacity, y = kg, fill = energy)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.2) +
  geom_col(width=0.65,linewidth = 0.1,col="black", position = position_dodge(width = 0.7)) +
  geom_text(
    aes(label = label,y=kg+label_nudge),
    position = position_dodge(width = 0.7),
    size = 6 * 5 / 14 * 0.8
  ) +
  geom_text(
    aes(label = energy,y=energy_nudge),
    position = position_dodge(width = 0.7),
    size = 6 * 5 / 14 * 0.8,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("Coal" = "#8c564b", "Oil" = "#9467bd", "NG" = "#CD7F32")) +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  labs(
    x = "Battery Capacity Scenario",
    y = "Fossil fuel (kg) avoided per kWh of Lithium-ion battery",
    fill = "Energy source",
    tag = "(a)",
    title = "Fossil fuel handprint of Lithium-ion batteries"
  ) +
  coord_flip(ylim = c(-300, 500)) +
  theme_bw(base_size = base_size) +
  theme(
    text = element_text(colour = "black"),
    plot.title = element_text(size = base_size, face = "bold"),
    plot.subtitle = element_text(size = base_size, hjust = 0),
    plot.caption = element_text(size = base_size - 2, hjust = 1, lineheight = 0.9),
    plot.tag = element_text(size = base_size + 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA),
    axis.title = element_text(size = base_size - 1.5),
    axis.text = element_text(size = base_size - 1.5),
    axis.ticks = element_line(size = 0.35, colour = "black"),
    legend.position = "none"
  )

cowplot::plot_grid(p_others_LIB, p_others, ncol = 2)
# fmt: skip
ggsave("Figures/Fossil_Handprint.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 18, height = 8.7)

# EoF
