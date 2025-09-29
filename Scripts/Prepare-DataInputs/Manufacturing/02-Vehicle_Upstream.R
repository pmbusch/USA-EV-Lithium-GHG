# Vehicle Upstream production emissions
# Source: ecoinvent 3.11

library(tidyverse)

# Vehicle production - no Battery
upstream <- read.csv("Parameters/Manufacturing/ecoinvent_upstream.csv")
up_mat <- read.csv("Parameters/Manufacturing/ecoinvent_upstream_material.csv")

upstream <- upstream %>% left_join(up_mat)

veh_prod <- upstream %>% 
  filter(str_detect(Name,"passenger car production")) %>% 
  filter(!str_detect(Name,"diesel"))

# kerb weight - GREET (see excel for detail)
# year 2025, EV 300MILE range
# excludes battery and fluids
# in pounds, convert to kg
weight <- tibble(vehSize=c("Car","Car","Light Truck","Light Truck"),
                 vehicle_type=c("ICE","EV","ICE","EV"),
                 kerbWeight=c(3234,2813,4577,4063)*0.453592)

# scale to kerb weight
veh_prod <- veh_prod %>% 
  mutate(vehicle_type=if_else(str_detect(Name,"electric"),"EV","ICE")) %>% 
  full_join(weight) %>% 
  dplyr::select(-sheet,-Name,-Region,-fu) %>% 
  mutate(across(-c(vehSize, vehicle_type, kerbWeight), ~ .x * kerbWeight)) %>% 
  mutate(kerbWeight=NULL)

names(veh_prod)[-(36:37)] <- paste0("vehProd_",names(veh_prod)[-(36:37)])

# save
write.csv(veh_prod,"Parameters/Manufacturing/vehProd.csv",row.names = F)

# Old CO2 for GREET ----

# Vehicle production - no Battery
veh_prod <- read.csv("Inputs/Prod_Veh.csv")
veh_prod <- veh_prod %>% group_by(vehicle_type,vehicle_class) %>% 
  reframe(vehProd_kgCO2=sum(GWP_component,na.rm=T)/1e3) # kg CO2e per vehicle
veh_prod <- veh_prod %>% rename(vehSize=vehicle_class) %>% 
  mutate(vehSize=if_else(vehSize=="CAR","Car","Light Truck")) %>% 
  group_by(vehicle_type,vehSize) %>% 
  reframe(vehProd_kgCO2=mean(vehProd_kgCO2)) 

write.csv(veh_prod,"Parameters/Manufacturing/GREET_vehProd.csv",row.names = F)


# EoF