# Master code to estimate GHG impacts
# PBH and YC Nov 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# electricity starts at 2024
start_year <- 2025

# UPSTREAM EMISSIONS -------

# Vehicle production - no Battery
veh_prod <- read.csv("Parameters/Manufacturing/vehProd.csv")

# Battery - agg. over chemistries, per vehicle
lib_upstream <- read.csv("Parameters/Manufacturing/LIB_prod.csv")

# Vehicle maintenance
veh_main <- read.csv("Parameters/Manufacturing/vehMaintenance.csv")


## Fleet, Sales and LIB -----
fleet <- read.csv("Parameters/Operation/USA_fleet_type.csv")
range(fleet$Year)
fleet <- fleet %>% filter(Year>=start_year)
head(fleet)
unique(fleet$Scenario)
fleet <- fleet %>% 
  mutate(Scenario_Lifetime=str_extract(Scenario,"Reference|Short|Long"),
         Scenario_Sales=str_extract(Scenario,"Momentum|Ambitious"),
         Scenario=NULL) %>% 
  filter(Scenario_Sales!="Baseline")

table(fleet$Scenario_Lifetime,fleet$Scenario_Sales)

# Amortize 2050 stock
veh_life_amort <- 10 # years, uniform...
amort <- fleet %>% 
  filter(Year==2050) %>% 
  # remaining life fraction
  mutate(remain_frac=if_else(age>veh_life_amort,0,
                             (veh_life_amort-age)/veh_life_amort)) %>% 
  mutate(fleet=fleet*remain_frac) %>% 
  group_by(Scenario_Sales,Scenario_Lifetime,Year,vehSize) %>% 
  reframe(amort=sum(fleet)) %>% ungroup()

# new sales
sales <- read.csv("Parameters/Operation/salesEV_type.csv")
range(sales$Year)
sales <- sales %>% filter(Year>=start_year) %>% 
  rename(Scenario_Sales=Scenario) %>% 
  filter(Scenario_Sales!="Baseline")
head(sales)
unique(sales$Scenario_Sales)
# dissagregation into state can be done later, but upstream does not depend on state...

# as if sales were either EV or ICE (comparative scenario)
veh_prod_total <- sales %>%
  left_join(veh_prod) %>%
  mutate(across(starts_with("vehProd"), ~ .x * Sales)) %>% 
  group_by(Scenario_Sales,Year,vehSize,vehicle_type) %>% 
  reframe(across(starts_with("vehProd"), ~ sum(.x))) %>% ungroup() %>% 
  mutate(Stage="Vehicle production")

# LIB requirements
lib_prod_total <- sales %>% 
  left_join(lib_upstream) %>% 
  mutate(across(starts_with("LIB_"), ~ .x * Sales)) %>% 
  group_by(Scenario_Sales,Scenario_Capacity,Year,vehSize) %>% 
  reframe(across(starts_with("LIB_"), ~ sum(.x))) %>% ungroup() %>% 
  mutate(Stage="LIB production") %>% 
  mutate(vehicle_type="EV")

# LIB replacement needs
LIB_replacement <- read.csv("Parameters/LIB_replacement_type.csv") # units of LIB
LIB_replacement <- LIB_replacement %>% filter(Year>=start_year) %>% 
  mutate(Scenario_Lifetime=str_extract(Scenario,"Reference|Short|Long"),
       Scenario_Sales=str_extract(Scenario,"Momentum|Ambitious"),
       Scenario=NULL) %>% 
  filter(Scenario_Sales!="Baseline")

# addional LIBs due to replacement
LIB_replacement_total <- LIB_replacement %>% 
  group_by(Scenario_Lifetime,Scenario_Sales,Year,vehSize) %>% 
  reframe(LIB=sum(LIB)) %>% ungroup() %>% 
  left_join(lib_upstream) %>%  # assume all car for now
  mutate(across(starts_with("LIB_"), ~ .x * LIB)) %>%
  group_by(Scenario_Sales,Scenario_Lifetime,Scenario_Capacity,Year,vehSize) %>% 
  reframe(across(starts_with("LIB_"), ~ sum(.x))) %>% ungroup() %>% 
  mutate(Stage="LIB Replacement") %>% 
  mutate(vehicle_type="EV")

## kWh req ------

kwh <- LIB_replacement %>% 
  group_by(Year,Scenario_Lifetime,Scenario_Sales,vehSize) %>% 
  reframe(LIB=sum(LIB)) %>% ungroup() %>% 
  left_join(sales) %>% 
  rename(`LIB Production`=Sales,`LIB Replacement`=LIB) %>% 
  pivot_longer(c(`LIB Production`,`LIB Replacement`), 
               names_to = "Stage", values_to = "kwh")
  

# bat size on average
bat <- read.csv("Parameters/Manufacturing/batsize.csv") %>% 
  group_by(vehSize,Scenario_Capacity) %>% 
  reframe(kwh_veh=sum(kwh_veh)) %>% ungroup()

kwh <- kwh %>% 
  left_join(bat) %>% 
  mutate(kwh=kwh*kwh_veh,kwh_veh=NULL)

kwh_total <- kwh %>% 
  group_by(Year,Scenario_Lifetime,Scenario_Capacity,Scenario_Sales,
           vehSize,Stage) %>% 
  reframe(kwh=sum(kwh)) %>% ungroup() %>% 
  mutate(vehicle_type="EV")

# Amortization
# vehicle production
amort_total <- amort %>%
  left_join(veh_prod) %>%
  # negative value
  mutate(across(starts_with("vehProd"), ~ -.x * amort)) %>% 
  group_by(Scenario_Sales,Scenario_Lifetime,Year,vehSize,vehicle_type) %>% 
  reframe(across(starts_with("vehProd"), ~ sum(.x))) %>% ungroup() %>% 
  mutate(Stage="Vehicle production") # substract from stage

# lib production
amort_lib_total <- amort %>% 
  left_join(lib_upstream) %>% 
  # negative
  mutate(across(starts_with("LIB_"), ~ -.x * amort)) %>% 
  group_by(Scenario_Sales,Scenario_Lifetime,Scenario_Capacity,Year,vehSize) %>% 
  reframe(across(starts_with("LIB_"), ~ sum(.x))) %>% ungroup() %>% 
  mutate(Stage="LIB production") %>% # substract from stage
  mutate(vehicle_type="EV")


## Maintenance impact ------

# based on fleet age - every 4 years ...
unique(fleet$age)
veh_maintenance_total <- fleet %>% 
  filter(age %in% seq(4,29,4)) %>% 
  group_by(Scenario_Lifetime,Scenario_Sales,Year,vehSize) %>% 
  reframe(fleet=sum(fleet)) %>% ungroup() %>% 
  cross_join(veh_main) %>%  # assume all car for now
  mutate(across(starts_with("vehMain_"), ~ .x * fleet)) %>%
  group_by(Scenario_Sales,Scenario_Lifetime,Year,vehSize,vehicle_type) %>% 
  reframe(across(starts_with("vehMain_"), ~ sum(.x))) %>% ungroup() %>% 
  mutate(Stage="Vehicle Maintenance")

# Recycling Impact -----

# LIB end of life flows
LIB_failure <- read.csv("Parameters/LIB_failure_type.csv")
LIB_available <- read.csv("Parameters/LIB_available_type.csv")
LIB_available$LIB <- LIB_available$LIB*0.5 # Other half goes to SSPS


LIB_recycling <- rbind(LIB_failure,LIB_available) %>% 
  filter(Year>=start_year) %>%
  mutate(Scenario_Lifetime=str_extract(Scenario,"Reference|Short|Long"),
         Scenario_Sales=str_extract(Scenario,"Momentum|Ambitious"),
         Scenario=NULL) %>% 
  filter(Scenario_Sales!="Baseline")

# Account for recovery rates flows
us_recycling <- seq(0.05,0.85,0.2)
us_recycling <- tibble(Scenario_Recycling=paste0("Recycling ",round(us_recycling*100,0),"%"),
                       recyc_rate=us_recycling)

LIB_recycling <- LIB_recycling %>% 
  cross_join(us_recycling) %>% 
  mutate(LIB=LIB*recyc_rate,recyc_rate=NULL)

# impacts per vehicle
lib_recyc_upstream <- read.csv("Parameters/Manufacturing/LIB_recyc.csv")

LIB_recycling_total <- LIB_recycling %>% 
  group_by(Scenario_Recycling,Scenario_Lifetime,Scenario_Sales,Year,vehSize) %>% 
  reframe(LIB=sum(LIB)) %>% ungroup() %>% 
  left_join(filter(lib_recyc_upstream,str_detect(process,"hydro"))) %>%  
  mutate(across(starts_with("LIBRecyc"), ~ .x * LIB)) %>%
  group_by(Scenario_Recycling,Scenario_Sales,Scenario_Lifetime,Scenario_Capacity,Year,vehSize) %>% 
  reframe(across(starts_with("LIBRecyc"), ~ sum(.x))) %>% ungroup() %>% 
  mutate(Stage="LIB Recycling") %>% 
  mutate(vehicle_type="EV")

# kWh total
kwh_recycling_total <- LIB_recycling %>% 
  group_by(Scenario_Recycling,Scenario_Lifetime,Scenario_Sales,Year,vehSize) %>% 
  reframe(kwh=sum(LIB)) %>% ungroup() %>% 
  left_join(bat) %>% 
  mutate(kwh=-kwh*kwh_veh,kwh_veh=NULL) %>% 
  mutate(Stage="LIB Recycling") %>% 
  mutate(vehicle_type="EV")

## Vehicle recycling -----
# EV end of life flows
EV_failure <- read.csv("Parameters/EV_failure_type.csv")

veh_recycling <- EV_failure %>% 
  filter(Year>=start_year) %>%
  mutate(Scenario_Lifetime=str_extract(Scenario,"Reference|Short|Long"),
         Scenario_Sales=str_extract(Scenario,"Momentum|Ambitious"),
         Scenario=NULL) %>% 
  filter(Scenario_Sales!="Baseline") %>% 
  filter(modelYear>2015)

# impacts per vehicle
veh_recyc_upstream <- read.csv("Parameters/Manufacturing/veh_recyc.csv")

veh_recycling_total <- veh_recycling %>% 
  group_by(Scenario_Lifetime,Scenario_Sales,Year,vehSize) %>% 
  reframe(EV=sum(EV)) %>% ungroup() %>% 
  left_join(veh_recyc_upstream) %>%  
  mutate(across(starts_with("VehRecyc"), ~ .x * EV)) %>%
  group_by(Scenario_Sales,Scenario_Lifetime,Year,vehSize,vehicle_type) %>% 
  reframe(across(starts_with("VehRecyc"), ~ sum(.x))) %>% ungroup() %>% 
  mutate(Stage="Vehicle production")


# USAGE EMISSIONS ---------

## kWh consumed by EVs -------
# based on fleet operation
ev_kwh <- read.csv("Parameters/Operation/EV_kwh_consumption.csv")
ev_kwh <- ev_kwh %>% filter(Year>=start_year) %>% 
  filter(!str_detect(Scenario,"Baseline")) %>% 
  mutate(Scenario_Lifetime=str_extract(Scenario,"Reference|Short|Long"),
         Scenario_Sales=str_extract(Scenario,"Momentum|Ambitious"),
         Scenario=NULL)
head(ev_kwh)
ev_kwh %>% group_by(Scenario_Sales,Scenario_Lifetime) %>% reframe(x=sum(total_kwh)/1e9) # TWh
ev_kwh %>% group_by(Scenario_Sales,Scenario_Lifetime) %>% reframe(x=sum(total_kwh)*3.6/1e9) # trillion MJ


#  REDO IN ANOTHER SCRIPT
# # upstream fossil fuel consumption
# electricity_fossil <- read.csv("Parameters/Electricity/countyElectricityFossilFuel.csv")
# electricity_fossil <- electricity_fossil %>% 
#   filter(scenario=="ref2025") %>% 
#   group_by(period,State,fossilFuel,unit) %>% 
#   reframe(value=weighted.mean(value,pop)) %>% ungroup() # group it by state
# 
# # total
# # 1 barrel: 136 kg of oil
# ev_kwh %>% 
#   left_join(rename(electricity_fossil,Year=period),
#             relationship = "many-to-many") %>% 
#   mutate(div=case_when(
#     fossilFuel=="Coal"~1e3, # to tons
#     # fossilFuel=="Crude oil"~136, # to barrels
#     fossilFuel=="Crude oil"~1e3, # to tons
#     T ~ 1)) %>% 
#   mutate(x=total_kwh*value/div) %>%
#   group_by(Scenario,fossilFuel) %>% reframe(x=sum(x)/1e6) # to million
# 
# # by energy (MJ)
# ev_kwh %>% 
#   left_join(rename(electricity_fossil,Year=period),
#             relationship = "many-to-many") %>% 
#   mutate(energy_dens=case_when(
#     fossilFuel=="Coal"~24, # MJ/kg
#     fossilFuel=="Crude oil"~44, # MJ/kg
#     T ~ 36.4)) %>% # Natural gas, MJ/Nm3 
#   mutate(x=total_kwh*value*energy_dens) %>%
#   group_by(Scenario,fossilFuel) %>% reframe(x=sum(x)/1e9) # to trillion
# 

## Electricity impacts ----
electricity <- read.csv("Parameters/Electricity/countyElectricityImpacts.csv")
electricity_mat <- read.csv("Parameters/Electricity/countyElectricityImpactsMaterial.csv")
electricity <- electricity %>% 
  left_join(electricity_mat)

unique(electricity$scenario)
electricity <- electricity %>% 
  rename(Scenario_Grid=scenario)

# group it by state
names(electricity)
electricity <- electricity %>% 
  group_by(Scenario_Grid,period,State) %>% 
  reframe(across(-c(STATEFP,COUNTYFP,NAME,pop),
                 ~ weighted.mean(.x,pop))) %>% ungroup()
  
head(electricity) # kg CO2e per kWh

ev_usage_total <- ev_kwh %>% 
  left_join(rename(electricity,Year=period)) %>%
  mutate(across(-c(Year,State,vehSize,total_kwh,Scenario_Lifetime,
                   Scenario_Sales,Scenario_Grid),~ .x * total_kwh)) %>% 
  mutate(Stage="Driving")

# agg to national level
ev_usage_total <- ev_usage_total %>% 
  group_by(Scenario_Sales,Scenario_Lifetime,Scenario_Grid,Year,vehSize,Stage) %>% 
  reframe(across(-c(State),~ sum(.x))) %>% ungroup() %>% 
  mutate(vehicle_type="EV")

ev_usage_total$total_kwh <- NULL

ev_usage_total %>% 
  group_by(Scenario_Sales,Scenario_Lifetime,Scenario_Grid) %>% 
  reframe(x=sum(kgCO2eq)/1e9) # million tons

## Gasoline emissions ----
# based on fleet operation
gas_gallons <- read.csv("Parameters/Operation/ICE_gasGallons_consumption.csv")
gas_gallons_improved <- read.csv("Parameters/Operation/ICE_gasGallons_consumption_MPGimproved.csv")

gas_gallons <- rbind(mutate(gas_gallons,Scenario_mpg="Reference"),
                     mutate(gas_gallons_improved,Scenario_mpg="Improved"))

gas_gallons <- gas_gallons %>% filter(Year>=start_year) %>% 
  mutate(Scenario_Lifetime=str_extract(Scenario,"Reference|Short|Long"),
         Scenario_Sales=str_extract(Scenario,"Momentum|Ambitious"),
         Scenario=NULL) %>% 
  filter(Scenario_Sales!="Baseline")


# 1 oil barrel = 42 gasoline gallons
gas_gallons %>% group_by(Scenario_Sales,Scenario_Lifetime,Scenario_mpg) %>% reframe(x=sum(total_gallons)/42/1e6) # million oil barrels
# 33.7 kWh per gallon, 3.6 MJ in a kwh
gas_gallons %>% group_by(Scenario_Sales,Scenario_Lifetime,Scenario_mpg) %>% reframe(x=sum(total_gallons)*33.7*3.6/1e9) # trillion MJ

# including upstream and combustion
gas <- read.csv("Parameters/Operation/LCI_petrol.csv")

# from impacts per kg of petrol to per gallon, assuming 0.74 kg/L
gas <- gas %>% 
  mutate(Stage="Driving") %>% 
  dplyr::select(-sheet,-Name,-Region,-fu) %>% 
  mutate(across(-c(Stage), ~ .x * 0.74 * 3.78541))
    
# estimate impacts
ice_usage_total <- gas_gallons %>% 
  cross_join(gas) %>% 
  mutate(across(-c(Year,Scenario_Sales,Scenario_Lifetime,Scenario_mpg,Stage,
                   State,vehSize,total_gallons), ~ .x * total_gallons)) %>% 
  mutate(vehicle_type="ICE")

# agg to national level
ice_usage_total <- ice_usage_total %>% 
  group_by(Scenario_Sales,Scenario_Lifetime,Scenario_mpg,Year,vehSize,Stage,vehicle_type) %>% 
  reframe(across(-c(State),~ sum(.x))) %>% ungroup()

ice_usage_total$total_gallons <- NULL

ice_usage_total %>% 
  group_by(Scenario_Sales,Scenario_Lifetime,Scenario_mpg) %>% 
  reframe(x=sum(kgCO2eq)/1e9) # million tons

# Save all -----
names(veh_prod_total) # by Sales Scenario
names(veh_maintenance_total) # by Sales and Lifetime Scenario
names(lib_prod_total) # by Sales and Capacity Scenario
names(LIB_replacement_total) # by Sales, Capacity and Lifetime Scenario
names(ev_usage_total) # by Sales, Lifetime and Grid Scenario
names(ice_usage_total) # by Sales and Lifetime Scenario and MPG
names(LIB_recycling_total) # by Sales, Capacity and Lifetime Scenario
names(veh_recycling_total) # by sales, lifetime scenario

write.csv(veh_prod_total,"Results/veh_prod.csv",row.names = F) 
write.csv(veh_maintenance_total,"Results/veh_maintenance.csv",row.names = F)  
write.csv(lib_prod_total,"Results/lib_prod.csv",row.names = F)  
write.csv(LIB_replacement_total,"Results/LIB_replacement.csv",row.names = F)
write.csv(ev_usage_total,"Results/ev_usage.csv",row.names = F)
write.csv(ice_usage_total,"Results/ice_usage.csv",row.names = F)  
write.csv(LIB_recycling_total,"Results/LIB_recycling.csv",row.names = F) 
write.csv(veh_recycling_total,"Results/veh_recycling.csv",row.names = F) 


# amortization
write.csv(amort_total,"Results/amort_veh.csv",row.names = F) 
write.csv(amort_lib_total,"Results/amort_LIB.csv",row.names = F) 

# kWh
write.csv(kwh_total,"Results/kwh.csv",row.names = F) 
write.csv(kwh_recycling_total,"Results/kwh_recyc.csv",row.names = F) 


# EoF 


# Grid analysis ----------------
# Based on EIA Electricity scenarios

df_grid <- rbind(veh_prod_total,
            mutate(filter(lib_prod_total,scen_recyc=="Recycling 5%"),scen_recyc=NULL),
            LIB_replacement_total,
            ice_usage_total,
            mutate(filter(Li_recycled_total,scen_recyc=="Recycling 5%"),scen_recyc=NULL))


grid_levels <- unique(ev_usage_total$EIA_GridScenario)
aux <- c()
for(i in grid_levels){
  aux <- rbind(aux,mutate(df_grid,EIA_GridScenario=i))
}
df_grid <- rbind(aux,ev_usage_total);rm(i)

write.csv(df_grid,"Results/EIAGridScenarios_tonsGHG.csv",row.names = F)
scens <- read.csv("Inputs/Join_EIAScenarios.csv")

df_grid <- df_grid %>% 
  left_join(scens,by=c("EIA_GridScenario"="scenario"))

# GHG intensity (gr CO2 per mile)
df_grid %>% 
  group_by(Scenario,scenarioDescription,vehicle_type) %>% 
  reframe(CI=sum(tonsCO2e)*1e6) %>% ungroup() %>% # to grams 
  left_join(vmt_total) %>% 
  mutate(CI=CI/vmt,vmt=NULL) %>% 
  pivot_wider(names_from = vehicle_type, values_from = CI)


total_df_grid <- df_grid %>% 
  group_by(Scenario,EIA_GridScenario,vehicle_type,Stage) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)/1e6) %>% ungroup() %>% 
  mutate(Stage=factor(Stage,levels=stage_lvl))

total_type_p2 <- df_grid %>% 
  group_by(Scenario,EIA_GridScenario,vehicle_type) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)/1e6) %>% ungroup() %>% 
  mutate(lab_total=paste0(round(tonsCO2e,0)," Mtons"))


ggplot(total_df_grid,aes(vehicle_type,tonsCO2e))+
  geom_col(aes(fill=Stage),position = position_stack(),
           col="black",linewidth=0.1)+
  geom_text(data=total_type_p2,aes(label=lab_total),size=8*5/14 * 0.8,
            nudge_y = 1000)+
  # facet_grid(EIA_GridScenario~Scenario)+
  facet_wrap(Scenario~EIA_GridScenario)+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_fill_viridis_d(direction = -1,option = "E")+
  labs(x="",y="",title = paste0("Million tons CO2e emissions, ",start_year,"-2050"))+
  theme_bw(8)+
  theme(panel.grid = element_blank())

# More concise figure
data_fig <- df_grid %>% 
  group_by(Scenario,EIA_GridScenario,scenarioDescription,vehicle_type) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)/1e6) %>% ungroup() %>% 
  filter(vehicle_type=="EV"|EIA_GridScenario=="ref2025") %>% 
  mutate(scenarioDescription=if_else(vehicle_type=="ICE",
                                     paste0("ICE - ",scenarioDescription),
                                     scenarioDescription))

scens1 <- (unique(data_fig$scenarioDescription))

data_fig <- data_fig %>% 
  mutate(Scenario=factor(Scenario,levels=c("Baseline","Momentum","Ambitious")),
         scenarioDescription=factor(scenarioDescription,levels=scens1))

ggplot(data_fig,aes(scenarioDescription,tonsCO2e,fill=Scenario))+
  geom_col(position = "dodge")+
  coord_flip()+
  facet_grid(Scenario~.)+
  labs(x="",y="",title = paste0("Million tons CO2e emissions, ",start_year,"-2050"))+
  theme_bw(8)+
  theme(panel.grid = element_blank())


# Recycling Analysis --------------


df2 <- rbind(veh_prod_total,
            LIB_replacement_total,
            ev_usage_total,
            ice_usage_total)
rec_levels <- unique(Li_recycled_total$scen_recyc)
aux <- c()
for(i in unique(Li_recycled_total$scen_recyc)){
  aux <- rbind(aux,mutate(df2,scen_recyc=i))
}
df2 <- rbind(aux,Li_recycled_total,lib_prod_total)

write.csv(df2,"Results/recycScenarios_tonsGHG.csv",row.names = F)

df2 <- df2 %>% mutate(scen_recyc=factor(scen_recyc,levels=rec_levels))

# GHG intensity (gr CO2 per mile)
df2 %>% 
  group_by(Scenario,scen_recyc,vehicle_type) %>% 
  reframe(CI=sum(tonsCO2e)*1e6) %>% ungroup() %>% # to grams 
  left_join(vmt_total) %>% 
  mutate(CI=CI/vmt,vmt=NULL) %>% 
  pivot_wider(names_from = vehicle_type, values_from = CI)


total_df2 <- df2 %>% 
  group_by(Scenario,scen_recyc,vehicle_type,Stage) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)/1e6) %>% ungroup() %>% 
  mutate(Stage=factor(Stage,levels=stage_lvl),
         scen_recyc=factor(scen_recyc,levels = rec_levels))


total_type_p2 <- df2 %>% 
  group_by(Scenario,scen_recyc,vehicle_type) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)/1e6) %>% ungroup() %>% 
  mutate(lab_total=paste0(round(tonsCO2e,0)," Mtons"))


ggplot(total_df2,aes(vehicle_type,tonsCO2e))+
  geom_col(aes(fill=Stage),position = position_stack(),
           col="black",linewidth=0.1)+
  geom_text(data=total_type_p2,aes(label=lab_total),size=8*5/14 * 0.8,
            nudge_y = 1000)+
  facet_grid(scen_recyc~Scenario)+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_fill_viridis_d(direction = -1,option = "E")+
  labs(x="",y="",title = paste0("Million tons CO2e emissions, ",start_year,"-2050"))+
  theme_bw(8)+
  theme(panel.grid = element_blank())

# EoF