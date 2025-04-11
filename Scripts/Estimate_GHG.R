# Master code to estimate GHG impacts
# PBH and YC Nov 2024


source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Usage emissions ---------

## kWh consumed by EVs -------

ev_kwh <- read.csv("EV_kwh_consumption.csv")
head(ev_kwh)

## electricity data ----
electricity <- read.csv("Parameters/countyElectricityCarbon.csv")
# group it by state
electricity <- electricity %>% group_by(period,State) %>% 
  reframe(kg_co2e=weighted.mean(kg_co2e,pop)) %>% ungroup()
head(electricity)

df <- ev_kwh %>% 
  left_join(rename(electricity,Year=period)) %>% 
  mutate(tGHG=total_kwh*kg_co2e/1e3, # tons CO2e
         Stage="Driving")
sum(df$tGHG)/1e6


## Fleet, Sales and LIB -----

fleet <- read.csv("Results/GHG/USA_fleet.csv")
head(fleet)


## LIB Size and Chemistry Share ------
# THIS GOES INTO UPSTREAM!!

bat_ldv <- read.csv("Parameters/Demand Intermediate Results/bat_size_chem_ldv.csv")
bat_rest <- read.csv("Parameters/Demand Intermediate Results/bat_size_chem_rest.csv")

# Filter USA
bat_ldv <- bat_ldv %>% filter(Region=="United States")
bat_rest <- bat_rest %>% filter(Region=="United States")

# Reference scenario FOR NOW
bat_ldv <- bat_ldv %>% 
  filter(chem_scenario=="Baseline",capacity_scenario=="Baseline")

head(bat_ldv)
head(bat_rest)

## Energy consumption forecast -----



# Electricity Grid Upstream -----

# TO DO - Do processing in another script, here we just load


# Gasoline production Upstream Impact -----

# TO DO - Do processing in another script, here we just load


# Battery recycling Impact -----

# TO DO - Do processing in another script, here we just load

# CAN BE DONE IN A 2ND ROUND MODEL DEVELOPMENT








# EoF