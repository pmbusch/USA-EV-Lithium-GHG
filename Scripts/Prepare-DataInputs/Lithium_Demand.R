# Estimates lithium demand for USA based on sales and LIB replacement
# PBH May 2025


source("Scripts/00-Libraries.R", encoding = "UTF-8")


# new sales
sales <- read.csv("Parameters/salesEV.csv")
LIB_replacement <- read.csv("Parameters/LIB_replacement.csv") # units of LIB
LIB_replacement <- LIB_replacement %>% 
  group_by(Year) %>% reframe(LIB=sum(LIB))
sum(sales$Sales)/1e6 # 312 M
sum(LIB_replacement$LIB)/1e6 # 63 M

# join them
sales <- sales %>% 
  left_join(LIB_replacement) %>% 
  mutate(Sales=Sales+LIB,
         LIB=NULL)

# Lithium required per battery

bat <- read.csv("Inputs/USA_bat_size.csv")

intensity <- read_excel("Inputs/Mineral_Intensity.xlsx",sheet="Li")
intensity <- intensity %>% 
  filter(Anode=="Graphite",str_detect(Electrolyte,"liquid"))

unique(intensity$Mineral)


bat <- bat %>% 
  left_join(intensity) %>% 
  mutate(kg_veh=kwh_veh*kg_per_kwh)

li_veh <- bat %>% group_by(Mineral) %>% 
  reframe(kg_veh=sum(kg_veh)) %>% ungroup() # in kg

# Lithium demand, in tons
cathode_scrap <- 0.04 # manufacturing scrap
sales <- sales %>% 
  mutate(x=1) %>% left_join(mutate(li_veh,x=1)) %>% 
  mutate(Mineral_tons=Sales*kg_veh/1e3/(1-cathode_scrap), x=NULL) %>% # tons or MWh
  mutate(Sales=NULL,kg_veh=NULL)

sales %>% group_by(Mineral) %>% reframe(x=sum(Mineral_tons)/1e6) # 3 M tons for Li, 32 TWh

# Account for Recycling flows
us_recycling <- 0.05

LIB_failure <- read.csv("Parameters/LIB_failure.csv")
LIB_available <- read.csv("Parameters/LIB_available.csv")
LIB_available$LIB <- LIB_available$LIB*0.5 # Other half goes to SSPS

Li_recycled <- rbind(LIB_failure,LIB_available) %>% 
  group_by(Year) %>% 
  reframe(LIB=sum(LIB)) %>% ungroup() %>% 
  mutate(x=1) %>% left_join(mutate(li_veh,x=1)) %>% 
  mutate(Min_recycled_tons=LIB*kg_veh*us_recycling/1e3, # tons or MWh
         LIB=NULL,x=NULL,kg_veh=NULL)

# add scrap to recycling
Li_recycled <- Li_recycled %>% 
  left_join(sales) %>% 
  mutate(Min_recycled_tons=Min_recycled_tons+
           Mineral_tons*cathode_scrap*us_recycling,
         Mineral_tons=NULL)

# delay one year
Li_recycled <- Li_recycled %>% 
  mutate(Year=Year+1) %>% 
  filter(Year<2051) %>% 
  rbind(tibble(Year=2022,Min_recycled_tons=0,
               Mineral=unique(Li_recycled$Mineral))) %>% arrange(Year)

# reduce primary Mineral extraction
sales <- sales %>% 
  left_join(Li_recycled) %>% 
  mutate(Mineral_tons=Mineral_tons-Min_recycled_tons,
         Min_recycled_tons=NULL)
  
write.csv(sales,"Results/MineralDemand.csv",row.names = F)
write.csv(Li_recycled,"Results/MineralRecycled.csv",row.names = F)


# Share of value in the battery, by Mineral price
bat <- read.csv("Inputs/USA_bat_size.csv")
price <- read_excel("Inputs/Mineral_Price.xlsx")


bat <- bat %>% 
  left_join(dplyr::select(intensity,Mineral,chemistry,kg_per_kwh)) %>%
  left_join(dplyr::select(price,Mineral,USD_ton)) %>% 
  filter(Mineral!="kWh")

bat <- bat %>% mutate(kg=kwh_veh*kg_per_kwh,
                      value=kwh_veh*kg_per_kwh*USD_ton/1e3)
  
value <- bat %>% group_by(Mineral) %>% 
  reframe(kg=sum(kg),
          value=sum(value)) %>% ungroup() %>% 
  mutate(share_value=value/sum(value))
value

write.csv(value,"Results/MineralValue.csv",row.names = F)


# EoF