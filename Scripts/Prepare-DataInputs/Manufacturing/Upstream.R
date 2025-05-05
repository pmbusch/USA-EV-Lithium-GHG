
#Lib material unit: kg co2e/ kwh
upstream_libmaterial<- read.csv("Inputs/upstream_libmaterial.csv")

#lib assembly unit: kg co2e/ kwh
upstream_libassembly <- sum(LIB_Assembly$GWP_component,na.rm = TRUE) / 1000

#Gasoline upstream unit: kg co2e/ gal
upstream_gas <- sum(Gas_upstream$GWP_component, na.rm = TRUE) / 1000

#Vehicle production unit: kg co2e/veh life
#ICE(components, ADR, Batteries,Fluids)
upstream_ICEProd <- Prod_Veh %>%
  filter(vehicle_type == "ICE") %>%
  summarise(total_kgCO2e = sum(GWP_component, na.rm = TRUE) / 1000) %>%
  pull(total_kgCO2e)

#EV(components, ADR, Fluids)
upstream_EvProd <- Prod_Veh %>%
  filter(vehicle_type == "EV") %>%
  summarise(total_kgCO2e = sum(GWP_component, na.rm = TRUE) / 1000) %>%
  pull(total_kgCO2e)