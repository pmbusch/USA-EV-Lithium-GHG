# LIB Size scenario definition

library(tidyverse)

# Battery size for USA
# Data: EV Volumes 2025
# Only LDV, BEV, for USA, baseline scenario 2024
# Cars: LDV, Light Trucks: Vans (or commercial vehicles)
bat <- read.csv("Inputs/USA_bat_size.csv") %>%
  rename(LIB_Chem = chemistry) %>%
  mutate(LIB_Chem = str_remove(LIB_Chem, " "))

# 721 upstream missing, assume 811
bat <- bat %>%
  mutate(LIB_Chem = if_else(LIB_Chem == "NMC721", "NMC811", LIB_Chem)) %>%
  group_by(Type, LIB_Chem) %>%
  reframe(kwh_veh = sum(kwh_veh)) %>%
  ungroup()

# scenario based on 95 qtls of bat size - EV Vol 2025
bat_scen <- tibble(
  Type = c("CAR", "PUT"),
  low_capacity = c(68, 83),
  high_capacity = c(100, 100)
)

# scale bat chemistry proportionally - equivalent to constant chem shares
bat <- bat %>%
  group_by(Type) %>%
  mutate(total = sum(kwh_veh)) %>%
  ungroup() %>%
  left_join(bat_scen) %>%
  mutate(
    scale_low = low_capacity / total,
    scale_high = high_capacity / total
  ) %>%
  mutate(
    low_capacity = kwh_veh * scale_low,
    high_capacity = kwh_veh * scale_high
  ) %>%
  rename(Reference = kwh_veh) %>%
  dplyr::select(-total, -scale_low, -scale_high)

bat <- bat %>%
  rename(vehSize = Type) %>%
  mutate(vehSize = if_else(vehSize == "CAR", "Car", "Light Truck"))


# convert to scenario
bat <- bat %>%
  pivot_longer(
    c(-vehSize, -LIB_Chem),
    names_to = "Scenario_Capacity",
    values_to = "kwh_veh"
  )

write.csv(bat, "Parameters/Manufacturing/batsize.csv", row.names = F)


# Figure inputs
bat %>%
  filter(Scenario_Capacity == "Reference") %>%
  ggplot(aes(vehSize, kwh_veh, fill = LIB_Chem)) +
  geom_col(linewidth = .2, col = "black") +
  coord_flip(expand = F, ylim = c(0, 98)) +
  scale_fill_viridis_d(direction = -1) +
  labs(
    x = "",
    y = "Battery capacity per vehicle [kWh]",
    fill = "Cathode Chemistry",
    col = ""
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw(8) +
  theme(panel.grid = element_blank(), legend.position = "bottom")

ggsave(
  "Figures/Fleet/batsize.png",
  ggplot2::last_plot(),
  units = "cm",
  dpi = 600,
  width = 8.7 * 1.5,
  height = 8.7
)

# EoF
