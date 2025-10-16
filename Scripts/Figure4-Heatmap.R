# Heatmap for key sensitivity
# PBH OCt 2025


source("Scripts/00-Libraries.R", encoding = "UTF-8")
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")

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


# scenario of grid to include
scens_grid <- c("ref2025","Cambium","highogs","lowogs")

# Flat them and join them - expanding missing scenarios
f.flat.exp <- function(df){
  
  names_scen <- paste0(names(df),collapse = ",") 
  
  # EXPAND DF to include all other scenario, no grid...
  if(!str_detect(names_scen,"Scenario_Lifetime")){
    df_aux <- c()
    for (i in unique(LIB_replacement$Scenario_Lifetime)){
      df$Scenario_Lifetime <- i
      df_aux <- rbind(df_aux,df)
    }
    df <- df_aux
  }
  
  if(!str_detect(names_scen,"Scenario_Capacity")){
    df_aux <- c()
    for (i in unique(LIB_replacement$Scenario_Capacity)){
      df$Scenario_Capacity <- i
      df_aux <- rbind(df_aux,df)
    }
    df <- df_aux
  }
  
  if(!str_detect(names_scen,"Scenario_Grid")){
    df_aux <- c()
    for (i in scens_grid){
      df$Scenario_Grid <- i
      df_aux <- rbind(df_aux,df)
    }
    df <- df_aux
  }
  
  if(!str_detect(names_scen,"Scenario_mpg")){
    df_aux <- c()
    for (i in unique(ice_usage$Scenario_mpg)){
      df$Scenario_mpg <- i
      df_aux <- rbind(df_aux,df)
    }
    df <- df_aux
  }
  
  if(!str_detect(names_scen,"Scenario_Recycling")){
    df_aux <- c()
    for (i in unique(LIB_recycling$Scenario_Recycling)){
      df$Scenario_Recycling <- i
      df_aux <- rbind(df_aux,df)
    }
    df <- df_aux
  }
  
  
  df <- df %>% 
    pivot_longer(-c(Scenario_Sales,Scenario_Lifetime,Scenario_Capacity,Scenario_Recycling,
                    Scenario_Grid,Year,vehSize,vehicle_type,Stage), 
                 names_to = "impact", values_to = "value")
  return(df)
}

df_all <- rbind(f.flat.exp(veh_prod),
                f.flat.exp(veh_maintenance),
                f.flat.exp(lib_prod),
                f.flat.exp(LIB_replacement),
                f.flat.exp(ev_usage),
                f.flat.exp(ice_usage),
                # f.flat.exp(amort_total),
                # f.flat.exp(amort_lib_total),
                f.flat.exp(LIB_recycling))

nrow(df_all)/1e6 # 7m

stage_lvl <- c("LIB Recycling","Driving",
               "LIB Replacement","LIB production",
               "Vehicle Maintenance","Vehicle production")
df_all <- df_all %>% mutate(Stage=factor(Stage,levels=stage_lvl))


head(df_all)

unique(df_all$Scenario_Grid)

# Figure -----
dict_grid <- read.csv("Inputs/Join_EIAScenarios.csv")

df <- df_all %>% 
  filter(impact %in% c("kgCO2eq","kgLithium")) %>% 
  filter(Scenario_Grid %in% scens_grid) %>% 
  filter(Scenario_Sales=="Ambitious") %>% 
  group_by(Scenario_Grid,Scenario_Sales,Scenario_Lifetime,Scenario_Capacity,Scenario_Recycling,vehicle_type,impact) %>% 
  reframe(value=sum(value)) %>% 
  left_join(dict_grid,by=c("Scenario_Grid"="scenario")) %>% 
  mutate(Scenario_Grid=scenarioDescription,scenarioDescription=NULL)

# get delta
head(df)
df <- df %>% 
  pivot_wider(names_from = c(vehicle_type,impact), values_from = value) %>% 
  mutate(delta_co2=(EV_kgCO2eq-ICE_kgCO2eq)/1e3, # to tons
         delta_li=EV_kgLithium-ICE_kgLithium) %>% 
  mutate(metric=-delta_co2/delta_li)

df <- df %>% 
  mutate(Scenario_Recycling=str_remove(Scenario_Recycling,"Recycling ") %>% 
           factor(levels = c("5%","25%","45%","65%","85%"))) %>%
  mutate(Scenario_Capacity=Scenario_Capacity %>% str_replace("_capacity"," Capacity") %>% 
           str_to_title() %>% factor(levels=c("Low Capacity","Reference","High Capacity"))) %>% 
  mutate(Scenario_Lifetime=Scenario_Lifetime %>% str_replace("Long","Long Duration") %>% 
           str_replace("Short","Short Duration") %>% factor(levels=c("Short Duration","Reference","Long Duration"))) %>% 
  mutate(Scenario_Grid=factor(Scenario_Grid))


range_metric <- range(df$metric)

ggplot(df,aes(Scenario_Recycling,Scenario_Grid,fill=metric))+
  geom_tile(color = "grey80") +  
  facet_grid(Scenario_Capacity~Scenario_Lifetime)+
  scico::scale_fill_scico(palette = "vik",direction = -1)+
  labs(x="Recycling rate",y="",col="",fill="tons CO2e avoided per kg Lithium")+
  theme_minimal(base_size = 9)+
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

ggsave("Figures/Fig4.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7*1.5)

# per kWh, in text
df_kwh <- df_all %>% 
  filter(impact %in% c("kgCO2eq")) %>% 
  filter(Scenario_Grid %in% scens_grid) %>% 
  filter(Scenario_Sales=="Ambitious") %>% 
  filter(Scenario_Recycling=="Recycling 5%") %>% 
  group_by(Scenario_Grid,Scenario_Lifetime,Scenario_Capacity,vehicle_type,impact) %>% 
  reframe(value=sum(value)) %>% 
  left_join(dict_grid,by=c("Scenario_Grid"="scenario")) %>% 
  mutate(Scenario_Grid=scenarioDescription,scenarioDescription=NULL)


kwh <- kwh_total %>% 
  filter(Scenario_Sales=="Ambitious") %>% 
  group_by(Scenario_Lifetime,Scenario_Capacity,vehicle_type) %>% 
  reframe(kwh=sum(kwh)) %>% ungroup() 

data_fig2 <- df_kwh %>% 
  pivot_wider(names_from = c(vehicle_type), values_from = value) %>%
  left_join(kwh) %>%
  mutate(delta_co2=(ICE-EV)/1e3) %>%  # to tons
  mutate(metric=delta_co2/kwh) # tons per kWh

data_fig2 <- data_fig2 %>% 
  mutate(Scenario_Capacity=Scenario_Capacity %>% str_replace("_capacity"," Capacity") %>% 
           str_to_title() %>% factor(levels=c("Low Capacity","Reference","High Capacity"))) %>% 
  mutate(Scenario_Lifetime=Scenario_Lifetime %>% str_replace("Long","Long Duration") %>% 
           str_replace("Short","Short Duration") %>% factor(levels=c("Short Duration","Reference","Long Duration"))) %>% 
  mutate(Scenario_Grid=factor(Scenario_Grid))

(range_metric <- range(data_fig2$metric))

ggplot(data_fig2,aes(Scenario_Capacity,Scenario_Grid,fill=metric))+
  geom_tile(color = "grey80") +  
  facet_grid(~Scenario_Lifetime)+
  scico::scale_fill_scico(palette = "vik",direction = -1)+
  labs(x="",y="",col="",fill="tons CO2e avoided per kWh of Lithium-ion battery produced")+
  theme_minimal(base_size = 9)+
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

ggsave("Figures/Fig4_LIB.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7*1)
