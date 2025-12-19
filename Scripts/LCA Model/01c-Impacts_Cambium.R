# Cambium Long Term Marginal Emissions factors -----
# For electricty grid, by state.

library(tidyverse)

# Factors
lme_state <- read.csv("Parameters/Electricity/cambium_LongMarginal.csv")

# get GWP - AR6 (ecoinvent 3.11)
# ch4 and n2o are by grams, so need to make adjustment
gwp <- tibble(pollutant = c("co2", "ch4", "n2o"), gwp = c(1, 29.8 / 1e3, 273 / 1e3))

lme <- lme_state |>
  left_join(gwp) |>
  mutate(kg_kWh = kg_MWh * gwp / 1000) |>
  group_by(period, State) |>
  reframe(kg_kWh = sum(kg_kWh)) |>
  ungroup() |>
  rename(Year = period)

## Add Hawaii and Alaska ------
# from own model
electricity <- read.csv("Parameters/Electricity/countyElectricityImpacts.csv")
ak <- electricity |>
  filter(scenario == "ref2025") |>
  filter(State %in% c("Alaska", "Hawaii")) |>
  group_by(period, State) %>%
  reframe(kg_kWh = weighted.mean(kgCO2eq, pop)) %>%
  ungroup() |>
  rename(Year = period)

lme <- rbind(lme, ak)


## kWh consumed ------
start_year <- 2025
ev_kwh <- read.csv("Parameters/Operation/EV_kwh_consumption.csv")
ev_kwh <- ev_kwh %>%
  filter(Year >= start_year) %>%
  filter(!str_detect(Scenario, "Baseline")) %>%
  mutate(
    Scenario_Lifetime = str_extract(Scenario, "Reference|Short|Long"),
    Scenario_Sales = str_extract(Scenario, "Momentum|Ambitious"),
    Scenario = NULL
  )

# Reference scenario only
ev_kwh <- ev_kwh |>
  filter(Scenario_Lifetime == "Reference") |>
  group_by(State, Year) |>
  reframe(total_kwh = sum(total_kwh)) |>
  ungroup()

# EV Driving Emissions --------
df <- ev_kwh |>
  left_join(lme) |>
  mutate(tonsCO2e = total_kwh * kg_kWh / 1e3) |>
  group_by(Year) |>
  reframe(tonsCO2e = sum(tonsCO2e)) |>
  ungroup() |>
  mutate(vehicle_type = "EV")

write.csv(df, "Results/Cambium/ev_usage.csv", row.names = F)

# EoF
