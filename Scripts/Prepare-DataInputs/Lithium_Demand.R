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

li_intensity <- intensity %>% filter(Mineral=="Lithium")

bat <- bat %>% 
  left_join(li_intensity) %>% 
  mutate(kg_veh=kwh_veh*kg_per_kwh)

li_veh <- sum(bat$kg_veh) # in kg

# Lithium demand, in tons
sales <- sales %>% mutate(Lithium_tons=Sales*li_veh/1e3) %>% 
  mutate(Sales=NULL)

sum(sales$Lithium_tons)/1e6 # 3 M tons

write.csv(sales,"Results/LiDemand.csv",row.names = F)

# Share of value in the battery, by Mineral price

bat <- read.csv("Inputs/USA_bat_size.csv")
price <- read_excel("Inputs/Mineral_Price.xlsx")


bat <- bat %>% 
  left_join(dplyr::select(intensity,Mineral,chemistry,kg_per_kwh)) %>%
  left_join(dplyr::select(price,Mineral,USD_ton))

bat <- bat %>% mutate(kg=kwh_veh*kg_per_kwh,
                      value=kwh_veh*kg_per_kwh*USD_ton/1e3)
  
value <- bat %>% group_by(Mineral) %>% 
  reframe(kg=sum(kg),
          value=sum(value)) %>% ungroup() %>% 
  mutate(share_value=value/sum(value))
value

write.csv(value,"Results/LiValue.csv",row.names = F)

# EoF