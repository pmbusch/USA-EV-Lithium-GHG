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

# total emissions by year
df %>% 
  filter(Year %in% c(2030,2040,2050)) %>% 
  group_by(vehicle_type,Year) %>% 
  reframe(mtons=sum(tonsCO2e)/1e6) %>% 
  pivot_wider(names_from = Year, values_from = mtons)

# For reference, US entire transport sector emissions in 2022: 1800 Mtons CO2e
# source: https://cfpub.epa.gov/ghgdata/inventoryexplorer/#transportation/entiresector/allgas/category/current

# by vmt
vmt <- read.csv("Results/total_VMT.csv")
vmt <- vmt %>% group_by(Year) %>% reframe(vmt=sum(total_vmt)/1e6) %>% ungroup()

vmt[2,2]/1e6 # trillion
vmt[22,2]/1e6 # trillion

# 2023, 3.19 trillion miles in USA
# https://afdc.energy.gov/data/10315

vmt_total <- sum(vmt$vmt)*1e6 # total miles

# GHG intensity (gr CO2 per mile)
df %>% 
  group_by(vehicle_type) %>% 
  reframe(CI=sum(tonsCO2e)*1e6) %>% ungroup() %>% # to grams 
  mutate(CI=CI/vmt_total,
         CI_km=CI/1.609)
# 156 for EV, 310 for ICE , same ballpark as https://www.nature.com/articles/s41598-024-51697-1



# Exploratory figures -----

# by lithium
total_type <- df %>% 
  group_by(vehicle_type) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)/1e6) %>% ungroup() %>% 
  mutate(lab_total=paste0(round(tonsCO2e,0)," Mtons"))

# load lithium
lithium_demand <- read.csv("Results/LiDemand.csv")

# reduction in ton CO2e per ton of lithium 
(li_red <- (total_type[2,2]-total_type[1,2])/(sum(lithium_demand$Lithium_tons)/1e6))

# 2.1 tons CO2e avoided per kg of lithium
# why allocate everything to lithium???? 

# based on value by content
(li_value <- read.csv("Results/LiValue.csv"))
li_red*li_value[4,4] # 73 kg CO2e per kg Lithium extracted


# million tons
total_df <- df %>% 
  group_by(vehicle_type,Stage) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)/1e6) %>% ungroup()

ggplot(total_df,aes(vehicle_type,tonsCO2e))+
  geom_col(aes(fill=Stage),position = position_stack(),
           col="black",linewidth=0.1)+
  geom_text(data=total_type,aes(label=lab_total),size=8*5/14 * 0.8,
            nudge_y = 1000)+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_fill_viridis_d(direction = -1,option = "E")+
  labs(x="",y="",title = "Million tons CO2e emissions, 2022-2050")+
  theme_bw(8)+
  theme(panel.grid = element_blank())

ggsave("Figures/GHG_fleet.png", ggplot2::last_plot(),units="cm",dpi=600,width=8.7,height=8.7)


# over time
df %>% 
  # group_by(Year,vehicle_type) %>% 
  # reframe(tonsCO2e=sum(tonsCO2e)) %>% ungroup() %>% 
  mutate(tonsCO2e=tonsCO2e/1e6) %>% 
  ggplot(aes(Year,tonsCO2e,fill=Stage))+
  geom_area()+
  facet_wrap(~vehicle_type)+
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


# EoF