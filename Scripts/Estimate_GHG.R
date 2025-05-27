# Master code to estimate GHG impacts
# PBH and YC Nov 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# UPSTREAM EMISSIONS -------

# Vehicle production - no Battery
veh_prod <- read.csv("Inputs/Prod_Veh.csv")
veh_prod <- veh_prod %>% group_by(vehicle_type,vehicle_class) %>% 
  reframe(vehProd_kgCO2=sum(GWP_component,na.rm=T)/1e3) # kg CO2e per vehicle
veh_prod <- veh_prod %>% rename(vehSize=vehicle_class) %>% 
  mutate(vehSize=if_else(vehSize=="CAR","Car","Light Truck")) %>% 
  group_by(vehicle_type,vehSize) %>% 
  reframe(vehProd_kgCO2=mean(vehProd_kgCO2)) # average SUV and PUT

# Battery
lib_materials <- read.csv("Inputs/upstream_libmaterial.csv")
lib_assembly <- read.csv("Inputs/LIB_Assembly.csv")
lib_assembly <- sum(lib_assembly$GWP_component,na.rm=T)/1e3 # kg CO2 per kWh
lib_materials$kgco2e_kwh <- lib_materials$kgco2e_kwh+lib_assembly
lib_extraction <- read.csv("Inputs/Li_extract.csv")
lib_extraction <- sum(lib_extraction$co2e_g, na.rm = TRUE) / 1e3 # kg CO2 per ton Li2CO3

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

lib_upstream <- lib_upstream %>% rename(vehSize=Type) %>% 
  mutate(vehSize=if_else(vehSize=="CAR","Car","Light Truck"))

## Fleet, Sales and LIB -----

fleet <- read.csv("Parameters/USA_fleet_type.csv")
range(fleet$Year)
head(fleet)
unique(fleet$Scenario)

# new sales
sales <- read.csv("Parameters/salesEV_type.csv")
range(sales$Year)
head(sales)
unique(sales$Scenario)
# dissagregation into state can be done later, but upstream does not depend on state...

# as if sales were either EV or ICE (comparative scenario)
veh_prod_total <- sales %>%
  left_join(veh_prod) %>%
  mutate(tonsCO2e=Sales*vehProd_kgCO2/1e3) %>% 
  group_by(Scenario,Year,vehSize,vehicle_type) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup() %>% 
  mutate(Stage="Vehicle production")

# LIB requirements
lib_prod_total <- sales %>% 
  left_join(lib_upstream) %>% 
  mutate(tonsCO2e=Sales*LIB_kgco2/1e3) %>% 
  group_by(Scenario,Year,vehSize) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup() %>% 
  mutate(Stage="LIB production") %>% 
  mutate(vehicle_type="EV")

# LIB replacement needs
LIB_replacement <- read.csv("Parameters/LIB_replacement_type.csv") # units of LIB

# add kWh
LIB_replacement_total <- LIB_replacement %>% 
  group_by(Scenario,Year,vehSize) %>% reframe(LIB=sum(LIB)) %>% ungroup() %>% 
  left_join(lib_upstream) %>%  # assume all car for now
  mutate(tonsCO2e=LIB*LIB_kgco2/1e3) %>% 
  group_by(Scenario,Year,vehSize) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup() %>% 
  mutate(Stage="LIB Replacement") %>% 
  mutate(vehicle_type="EV")

# Recycling Impact -----

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

# tons LI recovered
Li_recycled <- read.csv("Results/MineralRecycled.csv")
Li_recycled <- Li_recycled %>% filter(Mineral=="Lithium") %>% 
  mutate(Mineral=NULL)

Li_recycled_total <- Li_recycled %>% 
  mutate(tonsCO2e=Min_recycled_tons*lib_recyc_upstream*5.323) %>%  # Li to LCE conversion
  mutate(Stage="Lithium recycling") %>% 
  mutate(vehicle_type="EV",Min_recycled_tons=NULL)

# divide half-half to car and light truck for now
Li_recycled_total <- rbind(mutate(Li_recycled_total,tonsCO2e=tonsCO2e/2,vehSize="Car"),
                           mutate(Li_recycled_total,tonsCO2e=tonsCO2e/2,vehSize="Light Truck"))

## Subtract -----
# Remove Lithium upstream impact from battery production

# Lithium recycling avoids new lithium extraction
Li_avoided <- Li_recycled %>% 
  mutate(tonsCO2e=-Min_recycled_tons*lib_extraction*5.323/1e3) %>%  # Li to LCE conversion
  mutate(Stage="LIB production") %>% 
  mutate(vehicle_type="EV",Min_recycled_tons=NULL)

# divide half-half to car and light truck for now
Li_avoided <- rbind(mutate(Li_avoided,tonsCO2e=tonsCO2e/2,vehSize="Car"),
                           mutate(Li_avoided,tonsCO2e=tonsCO2e/2,vehSize="Light Truck"))
# Add to LIB production
aux <- c()
for (i in unique(Li_avoided$scen_recyc)){
  aux <- rbind(aux,
               mutate(lib_prod_total,scen_recyc=i))
}
rm(i)

lib_prod_total <- rbind(aux,Li_avoided) %>% 
  group_by(Scenario,scen_recyc,Year,vehSize,Stage,vehicle_type) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup()

# USAGE EMISSIONS ---------

## kWh consumed by EVs -------
# based on fleet operation
ev_kwh <- read.csv("Parameters/EV_kwh_consumption.csv")
head(ev_kwh)
ev_kwh %>% group_by(Scenario) %>% reframe(x=sum(total_kwh)/1e9) # TWh

# upstream fossil fuel consumtpion
electricity_fossil <- read.csv("Parameters/countyElectricityFossilFuel.csv")
electricity_fossil <- electricity_fossil %>% group_by(period,State,fossilFuel,unit) %>% 
  reframe(value=weighted.mean(value,pop)) %>% ungroup() # group it by state
# total
# 1 barrel: 136 kg of oil
ev_kwh %>% 
  left_join(rename(electricity_fossil,Year=period),
            relationship = "many-to-many") %>% 
  mutate(div=case_when(
    fossilFuel=="Coal"~1e3, # to tons
    fossilFuel=="Crude oil"~136, # to barrels
    T ~ 1)) %>% 
  mutate(x=total_kwh*value/div) %>%
  group_by(Scenario,fossilFuel) %>% reframe(x=sum(x)/1e6) # to million

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
  group_by(Scenario,Year,vehSize,Stage) %>% reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup() %>% 
  mutate(vehicle_type="EV")

ev_usage_total %>% group_by(Scenario) %>% reframe(x=sum(tonsCO2e)/1e6)

## Gasoline emissions ----
# based on fleet operation
gas_gallons <- read.csv("Parameters/ICE_gasGallons_consumption.csv")
# 1 oil barrel = 42 gasoline gallons
gas_gallons %>% group_by(Scenario) %>% reframe(x=sum(total_gallons)/42/1e6) # million oil barrels

gas <- read.csv("Inputs/Gas_upstream.csv")
gas <- sum(gas$GWP_component,na.rm=T)/1e3 # kg CO2e per gallon

ice_usage_total <- gas_gallons %>% 
  group_by(Scenario,Year,vehSize) %>% 
  reframe(tonsCO2e=sum(total_gallons)) %>% ungroup() %>% 
  mutate(tonsCO2e=tonsCO2e*gas/1e3) %>% 
  mutate(vehicle_type="ICE",Stage="Driving")

ice_usage_total %>% group_by(Scenario) %>% reframe(x=sum(tonsCO2e)/1e6)

# JOIN ALL -----------

# 5% recycling scenario, other scenario comes later
df <- rbind(veh_prod_total,
            mutate(filter(lib_prod_total,scen_recyc=="Recycling 5%"),scen_recyc=NULL),
            LIB_replacement_total,
            ev_usage_total,
            ice_usage_total,
            mutate(filter(Li_recycled_total,scen_recyc=="Recycling 5%"),scen_recyc=NULL))

write.csv(df,"Results/tonsGHG.csv",row.names = F)

stage_lvl <- c("Lithium recycling","Driving","LIB Replacement",
               "LIB production","Vehicle production")
df <- df %>% mutate(Stage=factor(Stage,levels=stage_lvl))

# Common sense checks

# total emissions by year
df %>% 
  filter(Year %in% c(2030,2040,2050)) %>% 
  filter(Scenario=="Ambitious") %>% 
  group_by(vehicle_type,Year) %>% 
  reframe(mtons=sum(tonsCO2e)/1e6) %>% 
  pivot_wider(names_from = Year, values_from = mtons)

# For reference, US entire transport sector emissions in 2022: 1800 Mtons CO2e
# source: https://cfpub.epa.gov/ghgdata/inventoryexplorer/#transportation/entiresector/allgas/category/current

# by vmt
vmt <- read.csv("Results/total_VMT.csv")
vmt_size <- vmt %>% group_by(Scenario,vehSize) %>% reframe(vmt=sum(total_vmt))
vmt <- vmt %>% group_by(Scenario,Year) %>% reframe(vmt=sum(total_vmt)/1e6) %>% ungroup()

vmt[2,3]/1e6 # trillion
vmt[22,3]/1e6 # trillion

# 2023, 3.19 trillion miles in USA
# https://afdc.energy.gov/data/10315

vmt_total <- vmt %>% group_by(Scenario) %>% reframe(vmt=sum(vmt)*1e6) # total miles 

# GHG intensity (gr CO2 per mile)
df %>% 
  group_by(Scenario,vehicle_type) %>% 
  reframe(CI=sum(tonsCO2e)*1e6) %>% ungroup() %>% # to grams 
  left_join(vmt_total) %>% 
  mutate(CI=CI/vmt,
         CI_km=CI/1.609)
# 163 for EV, 310 for ICE , same ballpark as https://www.nature.com/articles/s41598-024-51697-1

# by size
df %>% 
  group_by(Scenario,vehicle_type,vehSize) %>%
  reframe(CI=sum(tonsCO2e)*1e6) %>% ungroup() %>% # to grams 
  left_join(vmt_size) %>% 
  mutate(CI=CI/vmt,
         CI_km=CI/1.609)

# Exploratory figures -----

# by lithium
total_type <- df %>% 
  group_by(Scenario,vehicle_type) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)/1e6) %>% ungroup() %>% 
  mutate(lab_total=paste0(round(tonsCO2e,0)," Mtons"))

# load lithium
mineral_demand <- read.csv("Results/MineralDemand.csv")

lithium_demand <- mineral_demand %>% filter(Mineral=="Lithium")

li_demand_amb <- lithium_demand %>% filter(Scenario=="Ambitious",
                                           scen_recyc=="Recycling 5%")

# reduction in ton CO2e per ton of lithium 
(li_red <- (total_type[2,3]-total_type[1,3])/(sum(li_demand_amb$Mineral_tons)/1e6))

# 2.1 tons CO2e avoided per kg of lithium
# why allocate everything to lithium???? 

# based on value by content
(li_value <- read.csv("Results/MineralValue.csv"))
li_red*li_value[4,4] # 72 kg CO2e per kg Lithium extracted

# Reduction by kWh of battery pack - units are in MWh
total_kwh <- mineral_demand %>% 
  filter(Mineral=="kWh",
         scen_recyc=="Recycling 5%") %>% 
  group_by(Scenario) %>% 
  reframe(kWh=sum(Mineral_tons)*1e3) %>% ungroup()

kwh_red <- total_type %>% 
  dplyr::select(-lab_total) %>% 
  pivot_wider(names_from = vehicle_type, values_from = tonsCO2e) %>% 
  mutate(red=ICE-EV) %>% 
  left_join(total_kwh) %>% 
  mutate(red_pkWh=red*1e9/kWh) #from mtons to kg
kwh_red
# ~370 kg CO2 per kWh

# Reduction per kg of critical Mineral - Li,Ni,Co,Graphite,Cu
total_mineral <- mineral_demand %>% 
  filter(Mineral!="kWh",
         scen_recyc=="Recycling 5%") %>% 
  group_by(Scenario) %>% reframe(mtons=sum(Mineral_tons)/1e6)

min_red <- total_type %>% 
  dplyr::select(-lab_total) %>% 
  pivot_wider(names_from = vehicle_type, values_from = tonsCO2e) %>% 
  mutate(red=ICE-EV) %>% 
  left_join(total_mineral) %>% 
  mutate(red_kg_per_kg=red/mtons)
min_red
  

# million tons
total_df <- df %>% 
  filter(Scenario=="Ambitious") %>% 
  group_by(Scenario,vehicle_type,Stage) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)/1e6) %>% ungroup() %>% 
  mutate(Stage=factor(Stage,levels=stage_lvl))

total_type_p <- total_type %>% filter(Scenario=="Ambitious")

ggplot(total_df,aes(vehicle_type,tonsCO2e))+
  geom_col(aes(fill=Stage),position = position_stack(),
           col="black",linewidth=0.1)+
  geom_text(data=total_type_p,aes(label=lab_total),size=8*5/14 * 0.8,
            nudge_y = 1000)+
  # facet_wrap(~Scenario)+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_fill_viridis_d(direction = -1,option = "E")+
  labs(x="",y="",title = "Million tons CO2e emissions, 2022-2050")+
  theme_bw(8)+
  theme(panel.grid = element_blank())

ggsave("Figures/GHG_fleet.png", ggplot2::last_plot(),units="cm",dpi=600,width=8.7,height=8.7)

# over time
df %>% 
  group_by(Scenario,Year,vehicle_type,Stage) %>%
  reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup() %>%
  mutate(tonsCO2e=tonsCO2e/1e6) %>% 
  ggplot(aes(Year,tonsCO2e,fill=Stage))+
  geom_area()+
  facet_grid(vehicle_type~Scenario)+
  coord_cartesian(expand = F)+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_fill_viridis_d(direction = -1,option = "E")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  labs(x="",y="",title = "Million tons CO2e emissions")+
  theme_bw(8)+
  theme(panel.grid = element_blank(),
        panel.spacing = unit(1, "cm"))

ggsave("Figures/GHG_fleet_time.png", ggplot2::last_plot(),units="cm",
       dpi=600,width=8.7*2,height=8.7)

# By stage and vehicle size
stage_lvl2 <- expand.grid(stage_lvl,c("Car","Light Truck")) %>% 
  mutate(x=paste0(Var2,"-",Var1)) %>% pull(x)

# million tons
total_df <- df %>% 
  filter(Scenario=="Ambitious") %>% 
  group_by(Scenario,vehicle_type,Stage,vehSize) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)/1e6) %>% ungroup() %>% 
  mutate(Stage=paste0(vehSize,"-",Stage),
         Stage=factor(Stage,levels=stage_lvl2))

ggplot(total_df,aes(vehicle_type,tonsCO2e))+
  geom_col(aes(fill=Stage),position = position_stack(),
           col="black",linewidth=0.1)+
  geom_text(data=total_type_p,aes(label=lab_total),size=8*5/14 * 0.8,
            nudge_y = 1000)+
  # facet_wrap(~Scenario)+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_fill_viridis_d(direction = -1,option = "E")+
  labs(x="",y="",title = "Million tons CO2e emissions, 2022-2050")+
  theme_bw(8)+
  theme(panel.grid = element_blank())

ggsave("Figures/GHG_fleet_type.png", ggplot2::last_plot(),units="cm",dpi=600,width=8.7,height=8.7)


# Recycling Analysis --------------


# PENDING
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
  labs(x="",y="",title = "Million tons CO2e emissions, 2022-2050")+
  theme_bw(8)+
  theme(panel.grid = element_blank())

# EoF