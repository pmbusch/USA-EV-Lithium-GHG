# Master code to estimate GHG impacts
# PBH and YC Nov 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# UPSTREAM EMISSIONS -------

# Vehicle production - no Battery
veh_prod <- read.csv("Inputs/Prod_Veh.csv")
veh_prod <- veh_prod %>% group_by(vehicle_type) %>% 
  reframe(vehProd_kgCO2=sum(GWP_component,na.rm=T)/1e3) # kg CO2e per vehicle

# Battery
lib_materials <- read.csv("Inputs/upstream_libmaterial.csv")
lib_assembly <- read.csv("Inputs/LIB_Assembly.csv")
lib_assembly <- sum(lib_assembly$GWP_component,na.rm=T)/1e3 # kg CO2 per kWh
lib_materials$kgco2e_kwh <- lib_materials$kgco2e_kwh+lib_assembly

# Battery size USA
# Data: EV Volumes 2022
# Only LDV, BEV, for USA, baseline scenario 2022
bat <- read.csv("Inputs/USA_bat_size.csv") %>% 
  rename(LIB_Chem=chemistry) %>% 
  mutate(LIB_Chem=str_remove(LIB_Chem," "))

# 721 missing, assume 811
bat <- bat %>% 
  mutate(LIB_Chem=if_else(LIB_Chem=="NMC721","NMC811",LIB_Chem)) %>% 
  group_by(LIB_Chem) %>% reframe(kwh_veh=sum(kwh_veh)) %>% ungroup()

lib_upstream <- lib_materials %>%
  left_join(bat) %>% 
  mutate(LIB_kgco2=kgco2e_kwh*kwh_veh) %>% 
  group_by(Type) %>% 
  reframe(LIB_kgco2=sum(LIB_kgco2)) %>% ungroup()

## Fleet, Sales and LIB -----

fleet <- read.csv("Parameters/USA_fleet.csv")
head(fleet)

# new sales
sales <- read.csv("Parameters/salesEV.csv")
# dissagregation into state can be done later, but upstream does not depend on state...

# as if sales were either EV or ICE (comparative scenario)
veh_prod_total <- sales %>% 
  mutate(x=1) %>% 
  left_join(mutate(veh_prod,x=1)) %>% # assume all car for now
  mutate(tonsCO2e=Sales*vehProd_kgCO2/1e3) %>% 
  group_by(Year,vehicle_type) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup() %>% 
  mutate(Stage="Vehicle production")

# LIB requirements
lib_prod_total <- sales %>% 
  mutate(x=1) %>% 
  left_join(mutate(filter(lib_upstream,Type=="CAR"),x=1)) %>%  # assume all car for now
  mutate(tonsCO2e=Sales*LIB_kgco2/1e3) %>% 
  group_by(Year) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup() %>% 
  mutate(Stage="LIB production") %>% 
  mutate(vehicle_type="EV")

# LIB replacement needs
LIB_replacement <- read.csv("Parameters/LIB_replacement.csv") # units of LIB
# add kWh
LIB_replacement_total <- LIB_replacement %>% 
  group_by(Year) %>% reframe(LIB=sum(LIB)) %>% ungroup() %>% 
  mutate(x=1) %>% 
  left_join(mutate(filter(lib_upstream,Type=="CAR"),x=1)) %>%  # assume all car for now
  mutate(tonsCO2e=LIB*LIB_kgco2/1e3) %>% 
  group_by(Year) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup() %>% 
  mutate(Stage="LIB Replacement") %>% 
  mutate(vehicle_type="EV")

# Battery recycling Impact -----



# PENDING - impacts



# USAGE EMISSIONS ---------

## kWh consumed by EVs -------
# based on fleet operation
ev_kwh <- read.csv("Parameters/EV_kwh_consumption.csv")
head(ev_kwh)

## Electricity impacts ----
electricity <- read.csv("Parameters/countyElectricityCarbon.csv")
# group it by state
electricity <- electricity %>% group_by(period,State) %>% 
  reframe(kg_co2e=weighted.mean(kg_co2e,pop)) %>% ungroup()
head(electricity) # kg CO2e per kWh

ev_usage_total <- ev_kwh %>% 
  left_join(rename(electricity,Year=period)) %>% 
  mutate(tonsCO2e=total_kwh*kg_co2e/1e3, # tons CO2e
         Stage="Driving")

# agg to national level
ev_usage_total <- ev_usage_total %>% 
  group_by(Year,Stage) %>% reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup() %>% 
  mutate(vehicle_type="EV")

sum(ev_usage_total$tonsCO2e)/1e6

## Gasoline emissions ----
# based on fleet operation
gas_gallons <- read.csv("Parameters/ICE_gasGallons_consumption.csv")

gas <- read.csv("Inputs/Gas_upstream.csv")
gas <- sum(gas$GWP_component,na.rm=T)/1e3 # kg CO2e per gallon

ice_usage_total <- gas_gallons %>% 
  group_by(Year) %>% reframe(tonsCO2e=sum(total_gallons)) %>% ungroup() %>% 
  mutate(tonsCO2e=tonsCO2e*gas/1e3) %>% 
  mutate(vehicle_type="ICE",Stage="Driving")

sum(ice_usage_total$tonsCO2e)/1e6

# JOIN ALL -----------


df <- rbind(veh_prod_total,
            lib_prod_total,
            LIB_replacement_total,
            ev_usage_total,
            ice_usage_total)

write.csv(df,"Results/tonsGHG.csv",row.names = F)

# Common sense checks

# PENDING


# Exploratory figures -----

# by lithium
total_type <- df %>% 
  group_by(vehicle_type) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup()  

# US Lithium Demand
lithium_demand <- read.csv("Parameters/MineralDemand_FewScenarios.csv")
lithium_demand <- lithium_demand %>% 
  filter(Mineral=="Lithium") %>%
  filter(Year<2051) %>% 
  filter(scen_all=="Ambitious-Baseline-Baseline-Baseline-Baseline") %>% 
  filter(Region=="United States",Vehicle %in% c("Car","Additional LIB"),
         Powertrain=="BEV")
lithium_demand <- lithium_demand %>% 
  group_by(Year) %>% reframe(tons_mineral=sum(tons_mineral)) %>% ungroup()

# reduction in ton CO2e per ton of lithium 
(total_type[2,2]-total_type[1,2])/sum(lithium_demand$tons_mineral)

# 1.7 tons CO2e avoided per kg of lithium

total_df <- df %>% 
  group_by(vehicle_type,Stage) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup()  


ggplot(total_df,aes(vehicle_type,tonsCO2e,fill=Stage))+
  geom_col(position = position_stack())+
  labs(x="",y="")+
  theme_bw(8)+
  theme(panel.grid = element_blank())



df %>% 
  group_by(Year,vehicle_type) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup() %>% 
  ggplot(aes(Year,tonsCO2e,col=vehicle_type))+
  geom_line()


# EoF