# Vehicle Recycling  emissions (low)
# No displacement of any production, just less materials
# Source: ecoinvent 3.11

library(tidyverse)
library(readxl)

upstream <- read.csv("Parameters/Manufacturing/ecoinvent_upstream.csv")
up_mat <- read.csv("Parameters/Manufacturing/ecoinvent_upstream_material.csv")

upstream <- upstream %>% left_join(up_mat)

# No Economic allocation ....

# per kg of LIB treated
# need to work economic allocation, and LCE amount recovered
veh_recyc_upstream <- upstream %>% 
  filter(str_detect(Name,"treatment of used light"))
veh_recyc_upstream$fu

veh_recyc_upstream$sheet <- veh_recyc_upstream$Region <- veh_recyc_upstream$Name <- veh_recyc_upstream$fu <- NULL


names(veh_recyc_upstream) <- paste0("VehRecyc_",names(veh_recyc_upstream))

# same recycle per vehicle 
veh_recyc_upstream <- expand.grid(vehicle_type=c("EV","ICE"),vehSize=c("Car","Light Truck")) %>% 
  cbind(veh_recyc_upstream)

# Substract material requirements
veh_upstream <- read.csv("Parameters/Manufacturing/Vehprod.csv") %>% 
  pivot_longer(-c(vehicle_type,vehSize), names_to = "key", values_to = "subst") %>% 
  mutate(key=str_replace(key,"vehProd_","VehRecyc_"))


# Sources: all except copper are GREET
# Steel: https://www.steel.org/aisi-and-sma-steel-recycling-rates-report-final-07-27-2021.
# Aluminum: Kelly, Sean, and Diran Apelian. 2016. “Automotive Aluminum Recycling at End of Life: A Grave-to-Gate Analysis.” Rep. Cent. Resour. Recovery Recycl. CR3 Met. Process. Inst. Worcest. Polytech. Inst.
# "End-of-Life Recycling Methodology for Other Vehicle Materials" by Iyer and Kelly, Argonne 
# Copper: https://pubs.acs.org/doi/10.1021/es400069b
recovery_assumptions <- tibble(Mineral=c("Iron","Aluminium","Lead","Nickel","Magnesium","Copper"),
                               recov_rate=c(0.96,0.91,0.99,0.8,0.7,0.5))


# join based on flat tables, only minerals
recovery <- veh_upstream %>% 
  mutate(Mineral=str_remove(key,"VehRecyc_kg")) %>% 
  left_join(recovery_assumptions) %>% 
  filter(recov_rate>0) %>% 
  mutate(subst=subst*recov_rate,recov_rate=NULL,Mineral=NULL)

# substract all materials and metals as well
tot <- recovery %>% 
  group_by(vehicle_type,vehSize) %>% 
  reframe(subst=sum(subst)) %>% ungroup()
recovery <- rbind(recovery,
                  mutate(tot,key="VehRecyc_kgMat"),
                  mutate(tot,key="VehRecyc_kgMetal"))

# substract recovered materials
veh_recyc_upstream2 <- veh_recyc_upstream %>% 
  pivot_longer(c(-vehicle_type,-vehSize), names_to = "key", values_to = "value") %>% 
  left_join(recovery) %>%
  mutate(value=if_else(!is.na(subst),value-subst,value),subst=NULL) %>%
  pivot_wider(names_from = key, values_from = value)


# impacts per vehicle
write.csv(veh_recyc_upstream2,"Parameters/Manufacturing/Veh_recyc.csv",row.names = F)


## GREET ----


# in grams CO2eq per ton of LCE recovered. Economic allocation already done on GREEt
lib_recyc_upstream <- read.csv("Inputs/LIBRec_GWP.csv")


# use average of battery size
bat$share <- bat$kwh_veh/sum(bat$kwh_veh)

lib_recyc_upstream <- lib_recyc_upstream %>% 
  mutate(LIB_Chem=str_remove_all(Chemistry,"\\(|\\)")) %>% 
  mutate(tonsCO2e=Total_GWP_g_per_ton/1e3/1e3) %>% 
  left_join(bat) %>% 
  mutate(tonsCO2e=tonsCO2e*share)
lib_recyc_upstream <- sum(lib_recyc_upstream$tonsCO2e,na.rm=T)

write.csv(lib_recyc_upstream,"Parameters/Manufacturing/GREET_LIB_recyc.csv",row.names = F)


