# LIB Recycling  emissions
# Only displaces Lithium production
# Source: ecoinvent 3.11

library(tidyverse)
library(readxl)

upstream <- read.csv("Parameters/Manufacturing/ecoinvent_upstream.csv")
up_mat <- read.csv("Parameters/Manufacturing/ecoinvent_upstream_material.csv")

upstream <- upstream %>% left_join(up_mat)

# No Economic allocation ....

# per kg of LIB treated
# need to work economic allocation, and LCE amount recovered
lib_recyc_upstream <- upstream %>% 
  filter(str_detect(Name,"treatment of used Li"))
lib_recyc_upstream$fu

# Battery size USA
bat <- read.csv("Parameters/Manufacturing/batsize.csv")

# Battery density - BatPac 5.2
bat_dens <- read_excel("Inputs/CellChemistries.xlsx",
                       range = "A5:B12")
names(bat_dens) <- c("LIB_Chem","Wh_kg")


# Load economic allocation GREET
allocation <- read_excel("Inputs/GREET_EconomicAllocation_Recycling.xlsx")

# filter only lithium carbonate
allocation <- allocation %>% 
  filter(str_detect(Product,"Lithium")) %>% 
  pivot_longer(c(-Process,-Product), names_to = "LIB_Chem", values_to = "allocation")
  

# use average of battery size and chem
bat_agg <- bat %>% 
  left_join(bat_dens) %>% 
  group_by(Scenario_Capacity,vehSize) %>% 
  mutate(share=kwh_veh/sum(kwh_veh)) %>% 
  group_by(Scenario_Capacity,vehSize) %>% 
  reframe(kwh_veh=sum(kwh_veh),
          Wh_kg=weighted.mean(Wh_kg,share)) %>% ungroup()


# convert it to impact per vehicle
lib_recyc_upstream <- lib_recyc_upstream %>% 
  mutate(process=str_extract(Name,"hydrometallurgical|pyrometallurgical")) %>% 
  dplyr::select(-sheet,-Name,-Region,-fu) %>% 
  cross_join(bat_agg) %>% 
  mutate(across(-c(Scenario_Capacity,process,vehSize, kwh_veh,Wh_kg), ~ .x * kwh_veh*1000/Wh_kg)) %>%
  relocate(process,Scenario_Capacity,vehSize,.before=kgCO2eq) %>% 
  dplyr::select(-kwh_veh,-Wh_kg)

names(lib_recyc_upstream)[-(1:3)] <- paste0("LIBRecyc_",names(lib_recyc_upstream)[-(1:3)])

# Substract new LCE or mineral production 
# for now from battery LCE production
lib_upstream <- read.csv("Parameters/Manufacturing/LIB_prod.csv") %>% 
  pivot_longer(-c(Scenario_Capacity,vehSize), names_to = "key", values_to = "subst") %>% 
  mutate(key=str_replace(key,"LIB_","LIBRecyc_"))


# Maximum recovery rates
recovery_assumptions <- tibble(Mineral=c("Nickel","Cobalt","Lithium","Copper"),
                               recov_rate=c(0.9,0.9,0.8,0.95))
  
# join based on flat tables, only minerals
recovery <- lib_upstream %>% 
  mutate(Mineral=str_remove(key,"LIBRecyc_kg")) %>% 
  left_join(recovery_assumptions) %>% 
  filter(recov_rate>0) %>% 
  mutate(subst=subst*recov_rate,recov_rate=NULL)
# substract all materials and metals as well
tot <- recovery %>% 
  group_by(Scenario_Capacity,vehSize) %>% 
  reframe(subst=sum(subst)) %>% ungroup()
recovery <- rbind(recovery,
                  mutate(tot,key="LIBRecyc_kgMat",Mineral="Mat"),
                  mutate(tot,key="LIBRecyc_kgMetal",Mineral="Metal"))

# avoided impacts (only prevented lithium extraction)
li_extract <- upstream %>% 
  filter(str_detect(Name,"lithium carbonate"))
# average 50-50 brine and hard rock, and convert to per kg of lithium
li_extract <- (li_extract[1,5:52]+li_extract[2,5:52])/2*5.323

li_avoided <- recovery %>% 
  filter(Mineral=="Lithium") %>% 
  cbind(li_extract) %>% 
  mutate(across(-c(Scenario_Capacity, vehSize, key,Mineral,subst), ~ .x * subst)) %>% 
  mutate(Mineral=NULL,subst=NULL,key=NULL)
  
recovery$Mineral <- NULL

# substract recovered materials
lib_recyc_upstream2 <- lib_recyc_upstream %>% 
  pivot_longer(-c(process,Scenario_Capacity,vehSize), names_to = "key", values_to = "value") %>% 
  left_join(recovery) %>%
  mutate(value=if_else(!is.na(subst),value-subst,value),subst=NULL) %>%
  pivot_wider(names_from = key, values_from = value)

# substract Li avoided - order is the same, so do it rowwise twice
head(lib_recyc_upstream2[,1:4],12)
head(li_avoided[,1:4])  

lib_recyc_upstream2[1:6,4:51] <- lib_recyc_upstream2[1:6,4:51]-li_avoided[,3:50]
lib_recyc_upstream2[7:12,4:51] <- lib_recyc_upstream2[7:12,4:51]-li_avoided[,3:50]


# impacts per vehicle
write.csv(lib_recyc_upstream2,"Parameters/Manufacturing/LIB_recyc.csv",row.names = F)


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


