# Vehicle maitenance impacts
# Source: ecoinvent 3.11

library(tidyverse)


upstream <- read.csv("Parameters/Manufacturing/ecoinvent_upstream.csv")
up_mat <- read.csv("Parameters/Manufacturing/ecoinvent_upstream_material.csv")

upstream <- upstream %>% left_join(up_mat)

veh_main <- upstream %>% 
  filter(str_detect(Name,"maintenance")) 
veh_main$fu

# scale to kerb weight
veh_main <- veh_main %>% 
  mutate(vehicle_type=if_else(str_detect(Name,"electric"),"EV","ICE")) %>% 
  dplyr::select(-sheet,-Name,-Region,-fu) 
  
names(veh_main)[-(36)] <- paste0("vehMain_",names(veh_main)[-(36)])

# save
write.csv(veh_main,"Parameters/Manufacturing/vehMaintenance.csv",row.names = F)


# EoF