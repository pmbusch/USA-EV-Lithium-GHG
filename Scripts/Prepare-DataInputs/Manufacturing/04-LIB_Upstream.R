# LIB manufacturing Upstream production impacts
# Source: ecoinvent 3.11

library(tidyverse)
library(readxl)

upstream <- read.csv("Parameters/Manufacturing/ecoinvent_upstream.csv")
up_mat <- read.csv("Parameters/Manufacturing/ecoinvent_upstream_material.csv")

upstream <- upstream %>% left_join(up_mat)

# Battery
lib_upstream <- upstream %>% filter(str_detect(Name, "battery production"))
lib_upstream <- lib_upstream %>% mutate(LIB_Chem = str_extract(Name, "LFP|NCA|NMC111|NMC532|NMC622|NMC811"))

# Battery size USA
bat <- read.csv("Parameters/Manufacturing/batsize.csv")

# Battery density - ecoinvent 3.11 assumptioonts
bat_dens <- read_excel("Inputs/CellChemistries.xlsx", range = "A5:E12")
bat_dens <- bat_dens[, c(1, 5)]
names(bat_dens) <- c("LIB_Chem", "Wh_kg")

# kg per kWh- for comparsion
eco <- lib_upstream %>%
  dplyr::select(-sheet, -Name, -Region, -fu) %>%
  left_join(bat_dens) %>%
  mutate(across(-c(LIB_Chem, Wh_kg), ~ .x * 1000 / Wh_kg)) %>%
  dplyr::select(-Wh_kg)

# convert it to impact per vehicle
lib_upstream <- lib_upstream %>%
  dplyr::select(-sheet, -Name, -Region, -fu) %>%
  full_join(bat) %>%
  filter(!is.na(kwh_veh)) %>% #NMC111 not present
  left_join(bat_dens) %>%
  mutate(across(-c(Scenario_Capacity, LIB_Chem, vehSize, kwh_veh, Wh_kg), ~ .x * kwh_veh * 1000 / Wh_kg)) %>%
  dplyr::select(-kwh_veh, -Wh_kg)

# sum over chemistry - mineral content comes directly from ecoinvent
lib_upstream <- lib_upstream %>%
  group_by(Scenario_Capacity, vehSize) %>%
  reframe(across(-c(LIB_Chem), ~ sum(.x))) %>%
  ungroup()

names(lib_upstream)[-(1:2)] <- paste0("LIB_", names(lib_upstream)[-(1:2)])

# impacts per vehicle
write.csv(lib_upstream, "Parameters/Manufacturing/LIB_prod.csv", row.names = F)

# Compare mineral content with BatPac 5.2
intensity <- read_excel("Inputs/Mineral_Intensity.xlsx", sheet = "Li")
intensity <- intensity %>%
  filter(Anode == "Graphite", str_detect(Electrolyte, "liquid")) %>%
  mutate(chemistry = str_remove_all(chemistry, " "))

unique(intensity$Mineral)

eco <- eco %>%
  pivot_longer(c(-LIB_Chem), names_to = "Mineral", values_to = "kg_per_kwh_eco") %>%
  mutate(Mineral = str_remove(Mineral, "kg"))

intensity %>%
  left_join(eco, by = c("chemistry" = "LIB_Chem", "Mineral")) %>%
  filter(!is.na(kg_per_kwh_eco)) %>%
  dplyr::select(Mineral, chemistry, kg_per_kwh, kg_per_kwh_eco) %>%
  rename(`BatPac 5.2` = kg_per_kwh, `ecoinvent 3.11` = kg_per_kwh_eco) %>%
  pivot_longer(-c(Mineral, chemistry), names_to = "key", values_to = "value") %>%
  mutate(value = value * 1e3) %>%
  ggplot(aes(chemistry, value, fill = key)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~Mineral, scales = "free") +
  labs(x = "", y = "Mineral intensity [grams per kWh]", fill = "", col = "") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw(8) +
  theme(panel.grid = element_blank())

ggsave(
  "Figures/Fleet/MineralIntensity.png",
  ggplot2::last_plot(),
  units = "cm",
  dpi = 600,
  width = 8.7 * 2,
  height = 8.7
)


# compare full vehicle
batpac <- bat %>% left_join(intensity, by = c("LIB_Chem" = "chemistry")) %>% mutate(kg_veh = kwh_veh * kg_per_kwh)

batpac <- batpac %>% group_by(Scenario_Capacity, vehSize, Mineral) %>% reframe(kg_veh = sum(kg_veh)) %>% ungroup() # in kg

batpac %>% pivot_wider(names_from = Mineral, values_from = kg_veh)
lib_upstream %>% dplyr::select(Scenario_Capacity, vehSize, LIB_kgLithium, LIB_kgCobalt, LIB_kgCopper, LIB_kgNickel)

# ecoinvent is lower for lithium, cobalt, but higher for copper

# GREET version ----

# Battery
lib_materials <- read.csv("Inputs/upstream_libmaterial.csv")
lib_assembly <- read.csv("Inputs/LIB_Assembly.csv")
lib_assembly <- sum(lib_assembly$GWP_component, na.rm = T) / 1e3 # kg CO2 per kWh
lib_materials$kgco2e_kwh <- lib_materials$kgco2e_kwh + lib_assembly
lib_extraction <- read.csv("Inputs/Li_extract.csv")
lib_extraction <- sum(lib_extraction$co2e_g, na.rm = TRUE) / 1e3 # kg CO2 per ton Li2CO3

# Battery size USA
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

lib_upstream <- lib_materials %>%
  left_join(bat) %>%
  mutate(kwh_veh = if_else(is.na(kwh_veh), 0, kwh_veh)) %>%
  mutate(LIB_kgco2 = kgco2e_kwh * kwh_veh) %>%
  group_by(Type) %>%
  reframe(LIB_kgco2 = sum(LIB_kgco2)) %>%
  ungroup()

lib_upstream <- lib_upstream %>%
  rename(vehSize = Type) %>%
  mutate(vehSize = if_else(vehSize == "CAR", "Car", "Light Truck"))

write.csv(lib_upstream, "Parameters/Manufacturing/GREET_LIB_prod.csv", row.names = F)

# EoF
