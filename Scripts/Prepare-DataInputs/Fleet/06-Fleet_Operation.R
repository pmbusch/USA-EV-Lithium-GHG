# Based on Fleet size (with age), fuel consumption and VMT, calculate
# yearly fuel or electricity consumption
# PBH March 2025

library(tidyverse)

# Fuel Consumption ------
# get Fuel Consumption from EIA data
fe <- readxl::read_excel("Inputs/EIA2025Table40.xlsx", skip = 4)
names(fe)
fe[, 1]

# Note: ICCT Vans = Light Commercial Vehicles < 3.5 tonnes
# According to EIA Table 38, no Diesel sales for LDV, so all gasoline

# get only rows of
# 3: Gasoline ICE Vehicles
# 7 to 9 : EVs car
# 23: Gasoline ICE light trucks
# 28 to 29: EVs light trucks

fe <- fe[c(3, 7:9, 23, 28:29), ]
names(fe)
fe[, 1]
fe <- fe %>%
  dplyr::select(-...1, -`api key`, -units, -`Growth (2024-2050)`, -`2023`) %>%
  pivot_longer(c(-`full name`), names_to = "period", values_to = "mpge") %>%
  mutate(period = as.integer(str_remove(period, "X"))) %>%
  rename(full.name = `full name`)
unique(fe$full.name)

## EV Range Share range --------
# get share of sales of 100-200-300 mile EV, to average them
share_eia <- read.csv("Inputs/EIA2025Table38.csv")
names(share_eia)
unique(share_eia$scenario)
unique(share_eia$tableName)
unique(share_eia$unit)
unique(share_eia$seriesName)
unique(share_eia$regionName)

share_eia_ev <- share_eia %>%
  filter(str_detect(seriesName, "Electric Vehicle")) %>%
  filter(regionName != "United States") %>%
  group_by(period, seriesName) %>%
  reframe(units = sum(value)) %>%
  ungroup() %>%
  filter(units > 1) %>%
  group_by(period) %>%
  mutate(share = units / sum(units)) %>%
  mutate(seriesName = str_remove_all(seriesName, "Light-Duty Vehicle Sales : |Alternative-Fuel |Conventional ")) %>%
  mutate(vehSize = str_extract(seriesName, "Car|Light Truck")) %>%
  group_by(period, vehSize) %>% # share by size
  mutate(share_size = units / sum(units))
unique(share_eia_ev$seriesName)


# Almost all vehicles are 300-mile, and the regional variation is similar
ggplot(share_eia_ev, aes(period, share_size, fill = seriesName)) +
  geom_area() +
  # facet_wrap(~regionName)+
  coord_cartesian(expand = F) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", fill = "", title = "Share of new sales USA") +
  theme_bw(8)

## Car or Truck Share ------------

# As EIA mainly forecast gasoline adoption, the comparisons needs the national assumption
# on vehicle type (car or light truck) adoption
unique(share_eia$seriesName)
share_eia_size <- share_eia %>%
  filter(regionName != "United States") %>%
  filter(str_detect(seriesName, "Total New")) %>%
  group_by(period, regionName, seriesName) %>%
  reframe(units = sum(value)) %>%
  ungroup() %>%
  filter(units > 1) %>%
  group_by(period, regionName) %>%
  mutate(share = units / sum(units)) %>%
  mutate(seriesName = str_remove_all(seriesName, "Light-Duty Vehicle Sales : |Total New ")) %>%
  mutate(vehSize = if_else(seriesName == "Truck", "Light Truck", "Car"))
unique(share_eia_size$seriesName)

# tiny variations by region, still worth including
ggplot(share_eia_size, aes(period, share, fill = vehSize)) +
  geom_area() +
  facet_wrap(~regionName) +
  coord_cartesian(expand = F) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "", fill = "", title = "Share of new sales USA") +
  theme_bw(8)

ggsave("Figures/Fleet/EIA_types.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)

write.csv(share_eia_size, "Parameters/Operation/EIA_carTrucks_share.csv", row.names = F)

## Join to data to get FE by year of model
unique(fe$full.name)
unique(share_eia_ev$seriesName)

fe <- fe %>%
  mutate(vehSize = str_extract(full.name, "Car|Light Truck"), vehType = str_extract(full.name, "Gasoline|Electric"))

fe_ice <- fe %>% filter(vehType == "Gasoline")

# 15% improvement towards 2040 (linear)
improv <- fe_ice %>% filter(period == 2040) %>% mutate(add_mpge = mpge * 0.15) %>% dplyr::select(vehSize, add_mpge)
fe_ice_improv <- fe_ice %>%
  left_join(improv) %>%
  mutate(
    mpge = case_when(period < 2025 ~ mpge, period <= 2040 ~ mpge + add_mpge / 15 * (period - 2025), T ~ mpge + add_mpge)
  ) %>%
  mutate(add_mpge = NULL) %>%
  mutate(vehType = "Gasoline Improvement")
fe_ice <- rbind(fe_ice, fe_ice_improv)


fe_ev <- fe %>%
  filter(vehType == "Electric") %>%
  mutate(
    seriesName = str_remove_all(full.name, "Light-Duty Fuel Economy: Alternative-Fuel |: Reference case") %>%
      str_replace("Cars:", "Cars :") %>%
      str_replace("Trucks:", "Trucks :")
  ) %>%
  left_join(share_eia_ev)

# get weighted avg of mpge for different EV ranges
fe_ev <- fe_ev %>%
  mutate(mpge = mpge * share_size) %>%
  group_by(period, vehSize, vehType) %>%
  reframe(mpge = sum(mpge)) %>%
  ungroup()

# Figure before adjustments
data_mpg <- fe_ice %>%
  mutate(full.name = NULL, unit = "gallons") %>%
  rbind(mutate(fe_ev, unit = "kWh")) %>%
  mutate(type = paste0(vehSize, "-", vehType))

ggplot(data_mpg, aes(period, mpge, col = type)) +
  geom_line(linewidth = 0.3) +
  geom_text(
    data = filter(data_mpg, period == 2040),
    aes(label = type),
    nudge_y = c(0.8, -1, 1, 1, 1, 1) * 5,
    size = 8 * 5 / 14 * 0.8
  ) +
  coord_cartesian(expand = F, ylim = c(0, 200)) +
  scale_color_manual(values = c("#4daf4a", "#e41a1c", "#e41a1c90", "#377eb8", "#984ea3", "#984ea390")) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 33.7, name = "kWh per mile")) +
  labs(x = "", y = "Miles per gallon equivalent (mpge)", fill = "", title = "", col = "") +
  theme_bw(8) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(hjust = 1), legend.position = "none")
ggsave("Figures/Fleet/mpge.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 1.5, height = 8.7)


# convert to kWh based on EPA assumption of 33.7 kWh per gallon https://www.epa.gov/greenvehicles/fuel-economy-and-ev-range-testing
fe_ev$mpge <- fe_ev$mpge / 33.7 # miles per kWh, really high efficiency

# Adjustment from 2-cycle to aggressive driving, no temperature yet
# Equations for city
fe_ice$mpge <- 1 / (0.00187 + 1.134 / fe_ice$mpge)

# Adjustment from 2-cycle to aggressive driving, no temperature yet
# Equations for city
fe_ev$mpge <- 1 / (1 / fe_ev$mpge + 0.67 * (1 / fe_ev$mpge / 0.7 - 1 / fe_ev$mpge))

# join them
fe <- fe_ice %>% mutate(full.name = NULL, unit = "gallons") %>% rbind(mutate(fe_ev, unit = "kWh"))

write.csv(fe, "Parameters/Operation/mpg.csv", row.names = F)

# VMT ------
vmt <- readxl::read_excel("Inputs/VMT.xlsx", range = "C14:G45")
vmt[, c(2, 4)] <- NULL
names(vmt) <- c("age", "Car", "Light Truck")
vmt <- vmt %>% pivot_longer(c(-age), names_to = "vehSize", values_to = "vmt")
(tot <- vmt %>%
  group_by(vehSize) %>%
  reframe(vmt = sum(vmt)) %>%
  mutate(labe = paste0("", round(vmt / 1e3, 0), " thousand miles\n", round(vmt / 1e3 * 1.61, 0), " thousand kms")))

ggplot(vmt, aes(age, vmt, fill = vehSize)) +
  geom_col(position = "dodge", col = "black", linewidth = 0.1) +
  geom_text(
    data = tot,
    x = 16,
    y = 14e3,
    aes(label = labe),
    size = 9 * 5 / 14 * 0.8
  ) +
  facet_wrap(~vehSize) +
  coord_cartesian(expand = F) +
  labs(x = "Vehicle Age", fill = "", y = "Annual miles traveled", col = "") +
  scale_y_continuous(
    labels = function(x) {
      format(x, big.mark = " ", scientific = FALSE)
    },
    sec.axis = sec_axis(~ . * 1.61, name = "Annual kilometers traveled")
  ) +
  theme_bw(8) +
  theme(panel.grid = element_blank(), legend.position = "none")

ggsave("Figures/Fleet/vmt.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 1.5, height = 8.7)


# Fleet ----
fleet <- read.csv("Parameters/Operation/USA_fleet.csv") %>% mutate(modelYear = Year - age)
range(fleet$modelYear)
fleet %>% group_by(Scenario) %>% reframe(x = sum(fleet) / 1e9) # 3.40, total fleet over time

# divide EV fleet into US States, based on dissagregate EV script
ev_state_share <- read.csv("Parameters/Operation/EV_share_state.csv")
head(ev_state_share)
ev_state_share %>% group_by(period) %>% reframe(x = sum(ev_share))

# divide fleet into states based on modelYear
# Key assumption: No Trade between states across the fleet

consumption <- fleet %>% left_join(ev_state_share, by = c("modelYear" = "period")) %>% mutate(fleet = fleet * ev_share)
consumption %>% group_by(Scenario) %>% reframe(x = sum(fleet) / 1e9) # 3.40

# add VMT, but first divide fleet into cars-light trucks at each modelyear based on national EIA projections
dict_reg <- read.csv("Inputs/Join_TransportEIA_State.csv")
join_size <- share_eia_size %>%
  dplyr::select(period, regionName, vehSize, share) %>%
  left_join(dict_reg, by = c("regionName" = "Region_Transport")) %>%
  rename(State = NAME) %>%
  dplyr::select(period, vehSize, State, share)
range(join_size$period) # use 2024 for past model years
aux <- join_size %>% filter(period == 2024)
for (i in 2015:2023) {
  aux$period <- i
  join_size <- rbind(join_size, aux)
}
rm(i, aux)

# Assume all Vans (light commercial vehicles) are light trucks
join_size <- rbind(
  mutate(join_size, Vehicle = "Cars"),
  mutate(filter(join_size, vehSize == "Light Truck"), Vehicle = "Vans", share = 1)
)

consumption <- consumption %>%
  mutate(ev_share = NULL) %>%
  left_join(join_size, by = c("State", "modelYear" = "period", "Vehicle")) %>%
  mutate(fleet = fleet * share, share = NULL)
consumption %>% group_by(Scenario) %>% reframe(x = sum(fleet) / 1e9) # 3.40

# BEGIN PARENTHESIS

# save fleet with vehicle type
fleet_type <- consumption %>%
  group_by(Scenario, Year, age, modelYear, vehSize) %>%
  reframe(fleet = sum(fleet)) %>%
  ungroup()
fleet_type %>% group_by(Scenario) %>% reframe(x = sum(fleet) / 1e9) # 3.40
write.csv(fleet_type, "Parameters/Operation/USA_fleet_type.csv", row.names = F)

# do the same for sales
sales <- read.csv("Parameters/Operation/salesEV.csv")
sales %>% group_by(Scenario) %>% reframe(x = sum(Sales) / 1e6) # 353
# first by state, then by vehicle type
sales <- sales %>%
  left_join(ev_state_share, by = c("Year" = "period")) %>%
  mutate(Sales = Sales * ev_share, ev_share = NULL) %>%
  left_join(join_size, by = c("State", "Year" = "period", "Vehicle")) %>%
  mutate(Sales = Sales * share, share = NULL) %>%
  group_by(Scenario, Year, vehSize) %>%
  reframe(Sales = sum(Sales)) %>%
  ungroup()
sales %>% group_by(Scenario) %>% reframe(x = sum(Sales) / 1e6) # 353
write.csv(sales, "Parameters/Operation/salesEV_type.csv", row.names = F)

# LIB replacement
addLIB <- read.csv("Parameters/LIB_replacement.csv")
addLIB %>% group_by(Scenario) %>% reframe(x = sum(LIB) / 1e6) # 24.2
addLIB <- addLIB %>%
  left_join(ev_state_share, by = c("modelYear" = "period")) %>%
  mutate(LIB = LIB * ev_share, ev_share = NULL) %>%
  left_join(join_size, by = c("State", "modelYear" = "period", "Vehicle")) %>%
  mutate(LIB = LIB * share, share = NULL) %>%
  group_by(Scenario, Year, age, modelYear, vehSize) %>%
  reframe(LIB = sum(LIB)) %>%
  ungroup()
addLIB %>% group_by(Scenario) %>% reframe(x = sum(LIB) / 1e6) # 24.2
write.csv(addLIB, "Parameters/LIB_replacement_type.csv", row.names = F)

# LIB Failure
LIB_failure <- read.csv("Parameters/LIB_failure.csv")
LIB_failure %>% group_by(Scenario) %>% reframe(x = sum(LIB) / 1e6) # 97.4
LIB_failure <- LIB_failure %>%
  left_join(ev_state_share, by = c("modelYear" = "period")) %>%
  mutate(LIB = LIB * ev_share, ev_share = NULL) %>%
  left_join(join_size, by = c("State", "modelYear" = "period", "Vehicle")) %>%
  mutate(LIB = LIB * share, share = NULL) %>%
  group_by(Scenario, Year, age, modelYear, vehSize) %>%
  reframe(LIB = sum(LIB)) %>%
  ungroup()
LIB_failure %>% group_by(Scenario) %>% reframe(x = sum(LIB) / 1e6) # 97.4
write.csv(LIB_failure, "Parameters/LIB_failure_type.csv", row.names = F)
# LIB available
LIB_available <- read.csv("Parameters/LIB_available.csv")
LIB_available %>% group_by(Scenario) %>% reframe(x = sum(LIB) / 1e6) # 47.9
LIB_available <- LIB_available %>%
  left_join(ev_state_share, by = c("modelYear" = "period")) %>%
  mutate(LIB = LIB * ev_share, ev_share = NULL) %>%
  left_join(join_size, by = c("State", "modelYear" = "period", "Vehicle")) %>%
  mutate(LIB = LIB * share, share = NULL) %>%
  group_by(Scenario, Year, age, modelYear, vehSize) %>%
  reframe(LIB = sum(LIB)) %>%
  ungroup()
LIB_available %>% group_by(Scenario) %>% reframe(x = sum(LIB) / 1e6) # 47.9
write.csv(LIB_available, "Parameters/LIB_available_type.csv", row.names = F)


# EV Failure
EV_failure <- read.csv("Parameters/EV_failure.csv")
EV_failure %>% group_by(Scenario) %>% reframe(x = sum(EV) / 1e6) # 129
EV_failure <- EV_failure %>%
  left_join(ev_state_share, by = c("modelYear" = "period")) %>%
  mutate(EV = EV * ev_share, ev_share = NULL) %>%
  left_join(join_size, by = c("State", "modelYear" = "period", "Vehicle")) %>%
  mutate(EV = EV * share, share = NULL) %>%
  group_by(Scenario, Year, age, modelYear, vehSize) %>%
  reframe(EV = sum(EV, na.rm = T)) %>%
  ungroup()
EV_failure %>% group_by(Scenario) %>% reframe(x = sum(EV) / 1e6) # 97.4
write.csv(EV_failure, "Parameters/EV_failure_type.csv", row.names = F)


# END PARENTHESIS

# add VMT
head(vmt)
consumption <- consumption %>% left_join(vmt) %>% mutate(total_vmt = vmt * fleet)
consumption %>% group_by(Scenario) %>% reframe(x = sum(fleet) / 1e9) # 3.40

write.csv(consumption, "Results/total_VMT.csv", row.names = F)

# get total gallons of gasoline or kWh consumed

# get data for 2015-2023 - assume same as 2024
range(fe$period)
aux <- fe %>% filter(period == 2024)
for (i in 2015:2023) {
  aux$period <- i
  fe <- rbind(fe, aux)
}
rm(i, aux)

# Electric Vehicles only
fe_bev <- fe %>% filter(vehType == "Electric")

# Temperature adjustment factors
temp_factor <- read.csv("Parameters/Operation/TempAdjFactorsState.csv")
# Need to extrapolate data to missing states, based on temperature profiles
# Alaska is closer to North Dakota
# Hawaii is closer to Florida
temp_factor <- rbind(
  temp_factor,
  mutate(filter(temp_factor, State == "North Dakota"), State = "Alaska"),
  mutate(filter(temp_factor, State == "Florida"), State = "Hawaii")
)

consumption_ev <- consumption %>%
  left_join(fe_bev, by = c("vehSize", "modelYear" = "period")) %>%
  left_join(dplyr::select(temp_factor, State, BEVs)) %>%
  mutate(degradation_factor = (1 - 0.0033)^age) %>% # 0.33% per year
  mutate(total_kwh = total_vmt / (mpge * degradation_factor / BEVs))
consumption_ev %>% group_by(Scenario) %>% reframe(x = sum(fleet) / 1e9) # 3.40 fleet
consumption_ev %>% group_by(Scenario) %>% reframe(x = sum(total_kwh) / 1e9) # 19400 TWh
19400 / 3.4

# aggregate at state level
head(consumption_ev)
cons <- consumption_ev %>% group_by(Scenario, Year, State, vehSize) %>% reframe(total_kwh = sum(total_kwh))

write.csv(cons, "Parameters/Operation/EV_kwh_consumption.csv", row.names = F)

# Same but as the whole fleet was gasoline
fe_ice <- fe %>% filter(vehType == "Gasoline")

fe_ice

consumption_ice <- consumption %>%
  left_join(fe_ice, by = c("vehSize", "modelYear" = "period")) %>%
  left_join(dplyr::select(temp_factor, State, ICEVs)) %>%
  mutate(degradation_factor = (1 - 0.01)^age) %>% # 1% per year
  mutate(total_gallons = total_vmt / (mpge * degradation_factor / ICEVs))
consumption_ice %>% group_by(Scenario) %>% reframe(x = sum(fleet) / 1e9) # 3.4
consumption_ice %>% group_by(Scenario) %>% reframe(x = sum(total_gallons) / 1e9) # 1796

# aggregate at state level
head(consumption_ice)
cons <- consumption_ice %>% group_by(Scenario, Year, State, vehSize) %>% reframe(total_gallons = sum(total_gallons))

write.csv(cons, "Parameters/Operation/ICE_gasGallons_consumption.csv", row.names = F)

# Improvement in MPG
fe_ice <- fe %>% filter(vehType == "Gasoline Improvement")


consumption_ice <- consumption %>%
  left_join(fe_ice, by = c("vehSize", "modelYear" = "period")) %>%
  left_join(dplyr::select(temp_factor, State, ICEVs)) %>%
  mutate(degradation_factor = (1 - 0.01)^age) %>% # 1% per year
  mutate(total_gallons = total_vmt / (mpge * degradation_factor / ICEVs))
consumption_ice %>% group_by(Scenario) %>% reframe(x = sum(fleet) / 1e9) # 3.4
consumption_ice %>% group_by(Scenario) %>% reframe(x = sum(total_gallons) / 1e9) # 1796

# aggregate at state level
head(consumption_ice)
cons <- consumption_ice %>% group_by(Scenario, Year, State, vehSize) %>% reframe(total_gallons = sum(total_gallons))

write.csv(cons, "Parameters/Operation/ICE_gasGallons_consumption_MPGimproved.csv", row.names = F)

# EoF
