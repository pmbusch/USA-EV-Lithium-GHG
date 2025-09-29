## Analysis of baseline results
##
## PBH Sept 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# LOAD Results -------

veh_prod <- read.csv("Results/veh_prod.csv") # by Sales Scenario
veh_maintenance <- read.csv("Results/veh_maintenance.csv")  # by Sales and Lifetime Scenario
lib_prod <- read.csv("Results/lib_prod.csv") # by Sales and Capacity Scenario
LIB_replacement <- read.csv("Results/LIB_replacement.csv") # by Sales, Capacity and Lifetime Scenario
ev_usage <- read.csv("Results/ev_usage.csv") # by Sales, Lifetime and Grid Scenario
ice_usage <- read.csv("Results/ice_usage.csv") # by Sales and Lifetime Scenario
LIB_recycling <- read.csv("Results/LIB_recycling.csv") # by Sales, Capacity and Lifetime Scenario

amort_total <- read.csv("Results/amort_veh.csv") 
amort_lib_total <- read.csv("Results/amort_LIB.csv") 
# kWh
kwh_total <- read.csv("Results/kwh.csv") 
kwh_recycling_total <- read.csv("Results/kwh_recyc.csv") 



# Change names
(names(veh_prod) <- names(veh_prod) %>% str_remove("vehProd_"))
(names(veh_maintenance) <- names(veh_maintenance) %>% str_remove("vehMain_"))
(names(lib_prod) <- names(lib_prod) %>% str_remove("LIB_"))
(names(LIB_replacement) <- names(LIB_replacement) %>% str_remove("LIB_"))
# names(ev_usage) <- names(ev_usage) %>% str_remove("") 
# names(ice_usage) <- names(ice_usage) %>% str_remove("") 
(names(LIB_recycling) <- names(LIB_recycling) %>% str_remove("LIBRecyc_"))
(names(amort_total) <- names(amort_total) %>% str_remove("vehProd_"))
(names(amort_lib_total) <- names(amort_lib_total) %>% str_remove("LIB_"))


# Flat them and join them
f.flat <- function(df){
  
  names_scen <- paste0(names(df),collapse = ",") 
  
  # add missing scen as "Reference"
  if(!str_detect(names_scen,"Scenario_Lifetime")){
    df$Scenario_Lifetime <- "Reference"
  }
  
  if(!str_detect(names_scen,"Scenario_Capacity")){
    df$Scenario_Capacity <- "Reference"
  }
  
  if(!str_detect(names_scen,"Scenario_Grid")){
    df$Scenario_Grid <- "ref2025"
  }
  
  if(!str_detect(names_scen,"Scenario_Recycling")){
    df$Scenario_Recycling <- "Recycling 45%"
  }
  
  
  df <- df %>% 
    pivot_longer(-c(Scenario_Sales,Scenario_Lifetime,Scenario_Capacity,Scenario_Recycling,
                    Scenario_Grid,Year,vehSize,vehicle_type,Stage), 
                 names_to = "impact", values_to = "value")
  return(df)
}

df_all <- rbind(f.flat(veh_prod),
            f.flat(veh_maintenance),
            f.flat(lib_prod),
            f.flat(LIB_replacement),
            f.flat(ev_usage),
            f.flat(ice_usage),
            f.flat(amort_total),
            f.flat(amort_lib_total),
            f.flat(LIB_recycling))
nrow(df_all)/1e6 # below 1M, good
unique(df_all$Stage)

stage_lvl <- c("LIB Recycling","Driving",
               "LIB Replacement","LIB production",
               "Vehicle Maintenance","Vehicle production")
df_all <- df_all %>% mutate(Stage=factor(Stage,levels=stage_lvl))

# baseline scenario
df <- df_all %>% 
  filter(Scenario_Sales=="Ambitious",
         Scenario_Lifetime=="Reference",
         Scenario_Capacity=="Reference",
         Scenario_Recycling=="Recycling 45%",
         Scenario_Grid=="ref2025")

# Figure -----
unique(df$impact)
dict <- read_excel("Inputs/Dict_Impacts.xlsx")

total_df <- df %>% 
  left_join(dict) %>%
  filter(!is.na(Category)) %>% 
  group_by(Category,Impact_Name,vehicle_type,Stage) %>% 
  reframe(value=sum(value)/1e6) %>% ungroup() %>% 
  mutate(Stage=factor(Stage,levels=stage_lvl)) %>% 
  mutate(labX=paste0(Impact_Name,"-",vehicle_type))


start_year <- 2024
ggplot(total_df,aes(labX,value))+
  geom_col(data=filter(total_df,vehicle_type=="EV"),
           aes(fill=Stage),position = position_stack(),
           col="black",linewidth=0.1,width = 0.3)+
  geom_col(data=filter(total_df,vehicle_type=="ICE"),
           aes(fill=Stage),position = position_stack(),
           col="black",linewidth=0.1,width = 0.3)+
  # geom_text(data=total_type_p,aes(label=lab_total),size=8*5/14 * 0.8,
  #           nudge_y = 1000)+
  facet_wrap(~Category,scales="free")+
  coord_flip()+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_fill_viridis_d(direction = -1,option = "E")+
  labs(x="",y="",title = paste0("All in tons or TJ",start_year,"-2050"))+
  theme_bw(8)+
  theme(panel.grid = element_blank())

ggsave("Figures/fleet_impacts.png", ggplot2::last_plot(),units="cm",dpi=600,width=8.7,height=8.7)


# Common sense checks ----

unique(df$impact)

## Total emissions by year -----
df %>% 
  filter(Year %in% c(2030,2040,2050)) %>% 
  filter(impact=="kgCO2eq") %>% 
  group_by(vehicle_type,Year) %>% 
  reframe(mtons=sum(value)/1e9) %>% 
  pivot_wider(names_from = Year, values_from = mtons)

# For reference, US entire transport sector emissions in 2022: 1800 Mtons CO2e
# source: https://cfpub.epa.gov/ghgdata/inventoryexplorer/#transportation/entiresector/allgas/category/current

# by vmt
vmt <- read.csv("Results/total_VMT.csv")
vmt <- vmt %>% 
  mutate(Scenario_Lifetime=str_extract(Scenario,"Reference|Short|Long"),
         Scenario_Sales=str_extract(Scenario,"Momentum|Ambitious"),
         Scenario=NULL) %>% 
  filter(Scenario_Sales!="Baseline")
vmt_size <- vmt %>% 
  group_by(Scenario_Sales,Scenario_Lifetime,vehSize) %>% reframe(vmt=sum(total_vmt))

vmt <- vmt %>% group_by(Scenario_Sales,Scenario_Lifetime,Year) %>% reframe(vmt=sum(total_vmt)/1e6) %>% ungroup()
  

vmt[2,4]/1e6 # trillion
vmt[22,4]/1e6 # trillion

# 2023, 3.19 trillion miles in USA
# https://afdc.energy.gov/data/10315
vmt_total <- vmt %>% group_by(Scenario_Sales,Scenario_Lifetime) %>% reframe(vmt=sum(vmt)*1e6) # total miles 
vmt_total <- vmt_total %>% 
  filter(Scenario_Sales=="Ambitious",Scenario_Lifetime=="Reference")
  
  

## GHG intensity (gr CO2 per mile) ----
df %>% 
  filter(impact=="kgCO2eq") %>% 
  group_by(Scenario_Sales,Scenario_Lifetime,vehicle_type) %>% 
  reframe(CI=sum(value)*1e3) %>% ungroup() %>% # to grams 
  left_join(vmt_total) %>% 
  mutate(CI=CI/vmt,
         CI_km=CI/1.609)
# 163 g/mile for EV, 310 for ICE , same ballpark as https://www.nature.com/articles/s41598-024-51697-1

# by size
df %>% 
  group_by(Scenario_Sales,Scenario_Lifetime,vehicle_type,vehSize) %>%
  reframe(CI=sum(tonsCO2e)*1e3) %>% ungroup() %>% # to grams 
  left_join(vmt_size) %>% 
  mutate(CI=CI/vmt,
         CI_km=CI/1.609)


##  By kg lithium ------
total_type <- df %>% 
  group_by(Scenario,vehicle_type) %>% 
  reframe(tonsCO2e=sum(tonsCO2e)/1e6) %>% ungroup() %>% 
  mutate(lab_total=paste0(round(tonsCO2e,0)," Mtons"))

# load lithium
mineral_demand <- read.csv("Results/MineralDemand.csv")
mineral_demand <- mineral_demand %>% filter(Year>=start_year)

lithium_demand <- mineral_demand %>% filter(Mineral=="Lithium",
                                            scen_recyc=="Recycling 5%") %>% 
  group_by(Scenario) %>% 
  reframe(mtons=sum(Mineral_tons)/1e6) %>% ungroup()

# reduction in ton CO2e per ton of lithium 
li_red <- total_type %>% 
  dplyr::select(-lab_total) %>% 
  pivot_wider(names_from = vehicle_type, values_from = tonsCO2e) %>% 
  mutate(red=ICE-EV) %>% 
  left_join(lithium_demand) %>% 
  mutate(red_pkgLithium=red/mtons) 
li_red
# ~4.1 tons CO2e avoided per kg of lithium
# why allocate everything to lithium???? 
ggplot(li_red,aes(red,mtons))+geom_point()

# based on value by content
(li_value <- read.csv("Results/MineralValue.csv"))
li_red %>% mutate(red_pkgLithium=red_pkgLithium*li_value[4,4]) 
# ~145 kg CO2e per kg Lithium extracted

## By kWh battery pack -----
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
# ~400 kg CO2 per kWh

## by kg Mineral ------
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
# ~200 kg per kg of critical mineral (Li,Ni,Cu,Co,Graphite)

## FIGURE million tons -----
total_df <- df %>% 
  filter(impact=="kgCO2eq") %>% 
  group_by(vehicle_type,Stage) %>% 
  reframe(tonsCO2e=sum(value)/1e9) %>% ungroup() %>% 
  mutate(Stage=factor(Stage,levels=stage_lvl))

total_type_p <- total_type %>% filter(Scenario %in% c("Ambitious","Ambitious-Reference"))

start_year <- 2024
ggplot(total_df,aes(vehicle_type,tonsCO2e))+
  geom_col(aes(fill=Stage),position = position_stack(),
           col="black",linewidth=0.1)+
  # geom_text(data=total_type_p,aes(label=lab_total),size=8*5/14 * 0.8,
  #           nudge_y = 1000)+
  # facet_wrap(~Scenario)+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_fill_viridis_d(direction = -1,option = "E")+
  labs(x="",y="",title = paste0("Million tons CO2e emissions, ",start_year,"-2050"))+
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
  scale_x_continuous(breaks = c(2024, 2030, 2040, 2050))+
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
  labs(x="",y="",title = paste0("Million tons CO2e emissions, ",start_year,"-2050"))+
  theme_bw(8)+
  theme(panel.grid = element_blank())

ggsave("Figures/GHG_fleet_type.png", ggplot2::last_plot(),units="cm",dpi=600,width=8.7,height=8.7)
