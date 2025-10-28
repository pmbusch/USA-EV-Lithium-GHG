# Heatmap for key sensitivity
# PBH OCt 2025

source("Scripts/02b-Scenario_Load_Results.R")

head(df_all_scen)
unique(df_all_scen$Scenario_Grid)

# Figure -----
dict_grid <- read.csv("Inputs/Join_EIAScenarios.csv")

df <- df_all_scen %>%
  filter(impact %in% c("kgCO2eq", "kgLithium")) %>%
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
    delta_li = EV_kgLithium - ICE_kgLithium
  ) %>%
  mutate(metric = -delta_co2 / delta_li)

df <- df %>%
  mutate(
    Scenario_Recycling = str_remove(Scenario_Recycling, "Recycling ") %>%
      factor(levels = c("0%", "20%", "40%", "60%", "80%"))
  ) %>%
  mutate(
    Scenario_Capacity = Scenario_Capacity %>%
      str_replace("_capacity", " Capacity") %>%
      str_to_title() %>%
      factor(levels = c("Low Capacity", "Reference", "High Capacity"))
  ) %>%
  mutate(
    Scenario_Lifetime = Scenario_Lifetime %>%
      str_replace("Long", "Long Duration") %>%
      str_replace("Short", "Short Duration") %>%
      factor(levels = c("Short Duration", "Reference", "Long Duration"))
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
df |> filter(Scenario_Grid == "Reference case", Scenario_Recycling == "0%", Scenario_mpg == "Reference") |> pull(metric)

p_li <- df |>
  filter(Scenario_mpg == "Reference") |> # little change
  filter(Scenario_Recycling %in% c("0%", "40%", "80%")) |>
  ggplot(aes(Scenario_Capacity, Scenario_Grid, fill = metric)) +
  geom_tile(color = "grey80") +
  facet_grid(Scenario_Recycling ~ Scenario_Lifetime) +
  scico::scale_fill_scico(palette = "vik", direction = -1, midpoint = 8.976) +
  labs(
    y = "Electricity Grid Forecast",
    x = "",
    col = "",
    fill = expression("tons " ~ CO[2] * e ~ " avoided per kg Lithium"),
    tag = "(b)",
    title = "Handprint of Lithium"
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

# LIB ----

# per kWh, in text
df_kwh <- df_all_scen %>%
  filter(impact %in% c("kgCO2eq")) %>%
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

data_fig2 <- df_kwh %>%
  pivot_wider(names_from = c(vehicle_type), values_from = value) %>%
  left_join(kwh) %>%
  mutate(delta_co2 = (ICE - EV) / 1e3) %>% # to tons
  mutate(metric = delta_co2 / kwh) # tons per kWh

data_fig2 <- data_fig2 %>%
  mutate(
    Scenario_Capacity = Scenario_Capacity %>%
      str_replace("_capacity", " Capacity") %>%
      str_to_title() %>%
      factor(levels = c("Low Capacity", "Reference", "High Capacity"))
  ) %>%
  mutate(
    Scenario_Lifetime = Scenario_Lifetime %>%
      str_replace("Long", "Long Duration") %>%
      str_replace("Short", "Short Duration") %>%
      factor(levels = c("Short Duration", "Reference", "Long Duration"))
  ) %>%
  mutate(Scenario_Grid = factor(Scenario_Grid))

(range_metric <- range(data_fig2$metric))

# midpoint
data_fig2 |>
  filter(Scenario_Grid == "Reference case", Scenario_Capacity == "Reference", Scenario_Lifetime == "Reference")

p_lib <- ggplot(data_fig2, aes(Scenario_Capacity, Scenario_Grid, fill = metric)) +
  geom_tile(color = "grey80") +
  facet_grid(~Scenario_Lifetime) +
  scico::scale_fill_scico(palette = "broc", direction = -1, midpoint = 0.437) +
  # scale_fill_gradient(low = "#fce5d7", high = "#c64b55") +
  labs(x = "", y = "", col = "", fill = "tons CO2e avoided per kWh of Lithium-ion battery produced") +
  labs(
    y = "Electricity Grid Forecast",
    x = "",
    col = "",
    fill = expression("tons " ~ CO[2] * e ~ " avoided per kWh of Lithium-ion battery produced"),
    tag = "(a)",
    title = "Handprint of Lithium-ion Batteries"
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
