# Based on Fleet size (with age), fuel consumption and VMT, calculate 
# yearly fuel or electricity consumption
# PBH March 2025

library(tidyverse)

# Fuel Consumption ------
# get Fuel Consumption from EIA data
fe <- read.csv("Inputs/EIA_Table40_Energy Consumption.csv",skip = 4)
names(fe)
fe[,1]

# get only rows of 
# 3: Gasoline ICE Vehicles
# 7 to 9 : EVs car
# 23: Gasoline ICE light trucks
# 28 to 29: EVs light trucks

fe <- fe[c(3,7:9,23,28:29),]
names(fe)
fe <- fe %>% dplyr::select(-X,-api.key,-units,-Growth..2022.2050.,-X2021) %>% 
  pivot_longer(c(-full.name), names_to = "period", values_to = "mpge") %>% 
  mutate(period=as.integer(str_remove(period,"X")))
unique(fe$full.name)
  
## EV Range Share range --------
# get share of sales of 100-200-300 mile EV, to average them
share_eia <- read.csv("Inputs/EIA_Table38.csv")
names(share_eia)
unique(share_eia$scenario);unique(share_eia$tableName);unique(share_eia$unit)
unique(share_eia$seriesName)
unique(share_eia$regionName)

share_eia_ev <- share_eia %>% 
  filter(str_detect(seriesName,"Electric Vehicle")) %>% 
  filter(regionName!="United States") %>% 
  group_by(period,seriesName) %>% 
  reframe(units=sum(value)) %>% ungroup() %>% 
  filter(units>1) %>% 
  group_by(period) %>% 
  mutate(share=units/sum(units)) %>% 
  mutate(seriesName=str_remove_all(seriesName,"Light-Duty Vehicle Sales : |Alternative-Fuel |Conventional ")) %>% 
  mutate(vehSize=str_extract(seriesName,"Car|Light Truck")) %>% 
  group_by(period,vehSize) %>% # share by size
  mutate(share_size=units/sum(units))
unique(share_eia_ev$seriesName)


# Almost all vehicles are 300-mile, and the regional variation is similar
ggplot(share_eia_ev,aes(period,share_size,fill=seriesName))+
  geom_area()+
  # facet_wrap(~regionName)+
  coord_cartesian(expand = F)+
  scale_y_continuous(labels=scales::percent)+
  labs(x="",y="",fill="",title="Share of new sales USA")+
  theme_bw(8)

## Car or Truck Share ------------

# As EIA mainly forecast gasoline adoption, the comparisons needs the national assumption
# on vehicle type (car or light truck) adoption
unique(share_eia$seriesName)
share_eia_size <- share_eia %>% 
  filter(regionName!="United States") %>% 
  filter(str_detect(seriesName,"Total New")) %>% 
  group_by(period,regionName,seriesName) %>% 
  reframe(units=sum(value)) %>% ungroup() %>% 
  filter(units>1) %>% 
  group_by(period,regionName) %>% 
  mutate(share=units/sum(units)) %>% 
  mutate(seriesName=str_remove_all(seriesName,"Light-Duty Vehicle Sales : |Total New ")) %>% 
  mutate(vehSize=if_else(seriesName=="Truck","Light Truck","Car"))
unique(share_eia_size$seriesName)  

# tiny variations by region, still worth including
ggplot(share_eia_size,aes(period,share,fill=vehSize))+
  geom_area()+
  facet_wrap(~regionName)+
  coord_cartesian(expand = F)+
  scale_y_continuous(labels=scales::percent)+
  labs(x="",y="",fill="",title="Share of new sales USA")+
  theme_bw(8)

ggsave("Figures/Fleet/EIA_types.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)


## Join to data to get FE by year of model
unique(fe$full.name)
unique(share_eia_ev$seriesName)

fe <- fe %>% 
  mutate(vehSize=str_extract(full.name,"Car|Light Truck"),
         vehType=str_extract(full.name,"Gasoline|Electric"))
  
fe_ice <- fe %>% filter(vehType=="Gasoline")

fe_ev <- fe %>% 
  filter(vehType=="Electric") %>% 
  mutate(seriesName=str_remove_all(full.name,"Light-Duty Fuel Economy: Alternative-Fuel |: Reference case") %>% 
           str_replace("Cars:","Cars :") %>% str_replace("Trucks:","Trucks :")) %>% 
  left_join(share_eia_ev)

# get weighted avg of mpge for different EV ranges
fe_ev <- fe_ev %>% 
  mutate(mpge=mpge*share_size) %>% 
  group_by(period,vehSize,vehType) %>% 
  reframe(mpge=sum(mpge)) %>% ungroup()

# convert to kWh based on EPA asumption of 33.7 kWh per gallon https://www.epa.gov/greenvehicles/fuel-economy-and-ev-range-testing
fe_ev$mpge <- fe_ev$mpge/33.7 # miles per kWh, really high efficiency

# join them
fe <- fe_ice %>% mutate(full.name=NULL,unit="gallons") %>% 
  rbind(mutate(fe_ev,unit="kWh"))

write.csv(fe,"Parameters/mpg.csv",row.names = F)

# VMT ------
vmt <- readxl::read_excel("Inputs/VMT.xlsx",range="C14:G45")
vmt[,c(2,4)] <- NULL
names(vmt) <- c("age","Car","Light Truck")
vmt <- vmt %>% 
  pivot_longer(c(-age), names_to = "vehSize", values_to = "vmt")
vmt %>% group_by(vehSize) %>% reframe(vmt=sum(vmt)/1e3)

# Fleet ----
fleet <- read.csv("Parameters/USA_fleet.csv") %>% 
  mutate(modelYear=Year-age)
range(fleet$modelYear)
sum(fleet$fleet)/1e9 # 3.24, total fleet over time

# divide EV fleet into US States, based on Dissagregate EV script
ev_state_share <- read.csv("Parameters/EV_share_state.csv")
head(ev_state_share)
ev_state_share %>% group_by(period) %>% reframe(x=sum(ev_share))

# divide fleet into states based on modelYear 
# Key assumption: No Trade between states across the fleet

consumption <- fleet %>% 
  left_join(ev_state_share,by=c("modelYear"="period")) %>% 
  mutate(fleet=fleet*ev_share)
sum(consumption$fleet)/1e9 # 3.24


# add VMT, but first divide fleet into cars-light trucks at each modelyear based on national EIA projections
dict_reg <- read.csv("Inputs/Join_TransportEIA_State.csv")
join_size <- share_eia_size %>% 
  dplyr::select(period,regionName,vehSize,share) %>% 
  left_join(dict_reg,by=c("regionName"="Region_Transport")) %>% 
  rename(State=NAME) %>% dplyr::select(period,vehSize,State,share)
range(join_size$period) # use 2022 for past model years
aux <- join_size %>% filter(period==2022)
for (i in 2015:2021){
  aux$period <- i
  join_size <- rbind(join_size,aux)
}
rm(i,aux)

consumption <- consumption %>% 
  mutate(ev_share=NULL) %>% 
  left_join(join_size,by=c("State","modelYear"="period")) %>% 
  mutate(fleet=fleet*share,share=NULL)
sum(consumption$fleet)/1e9 # 3.24

# add VMT
head(vmt)
consumption <- consumption %>% 
  left_join(vmt) %>% 
  mutate(total_vmt=vmt*fleet)
sum(consumption$fleet)/1e9 # 3.24


# get total gallons of gasoline or kWh consumed

# get data for 2015-2021
range(fe$period)
aux <- fe %>% filter(period==2022)
for (i in 2015:2021){
  aux$period <- i
  fe <- rbind(fe,aux) } 
rm(i,aux)


# Electric Vehicles only
fe_bev <- fe %>% filter(vehType=="Electric")

consumption_ev <- consumption %>% 
  left_join(fe_bev,by=c("vehSize","modelYear"="period")) %>% 
  mutate(total_kwh=total_vmt/mpge)
sum(consumption_ev$fleet)/1e9 # 3.24
sum(consumption_ev$total_kwh)/1e9 # 138000 Twh 
sum(consumption_ev$total_vmt)/sum(consumption_ev$fleet)
sum(consumption_ev$total_kwh)/sum(consumption_ev$fleet) # 4266
12000/3

# aggregate at state level
head(consumption_ev)
cons <- consumption_ev %>% 
  group_by(Year,State) %>% 
  reframe(total_kwh=sum(total_kwh))

write.csv(cons,"EV_kwh_consumption.csv",row.names = F)

# Same but as the whole fleet was gasoline
fe_ice <- fe %>% filter(vehType=="Gasoline")

consumption_ice <- consumption %>% 
  left_join(fe_ice,by=c("vehSize","modelYear"="period")) %>% 
  mutate(total_gallons=total_vmt/mpge)
sum(consumption_ice$fleet)/1e9 # 3.24
sum(consumption_ice$total_gallons)/1e9 
sum(consumption_ice$total_vmt)/sum(consumption_ice$fleet)

# aggregate at state level
head(consumption_ice)
cons <- consumption_ice %>% 
  group_by(Year,State) %>% 
  reframe(total_gallons=sum(total_gallons))

write.csv(cons,"ICE_gasGallons_consumption.csv",row.names = F)


# EoF