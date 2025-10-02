# Common scripts to load results into a df, for all scenarios

## PBH Sept 2025

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


# EoF