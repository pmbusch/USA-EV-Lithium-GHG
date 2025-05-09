
#Lib material unit: kg co2e/ kwh
upstream_libmaterial<- read.csv("Inputs/upstream_libmaterial.csv")

#lib assembly unit: kg co2e/ kwh
upstream_libassembly <- sum(LIB_Assembly$GWP_component,na.rm = TRUE) / 1000

#Gasoline upstream unit: kg co2e/ gal
upstream_gas <- sum(Gas_upstream$GWP_component, na.rm = TRUE) / 1000

#Vehicle production unit: kg co2e/veh life
Upstream_Prod_Veh <- Prod_Veh %>%
  group_by(vehicle_type, vehicle_class) %>%
  summarise(
    kgco2e = sum(GWP_component, na.rm = TRUE) / 1000,
    .groups = "drop"
  )

#LIB Recycling unit: kg co2e/ ton of LCE generated from LIB recycling 
recycling_lib <- LIBRec_GWP %>%
  select(Chemistry, Total_GWP_g_per_ton) %>%
  mutate(kgCO2e_per_ton = Total_GWP_g_per_ton / 1000) %>%
  select(Chemistry, kgCO2e_per_ton)

