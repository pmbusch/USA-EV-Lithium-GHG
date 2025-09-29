# Combine endpoits based on characterization factors
# Master table to translate all flows into relevant impact categories

library(tidyverse)
library(readxl)

url_file <- "Inputs"


# GWP values AR6, excl biogenic CO2 ------
gwp <- read_excel(paste0(url_file,"/LCI_ecoinvent311.xlsx"),
                  sheet="GWP100_AR6",range="A22:G325")
# in kg CO2e


## Human Health PM2.5 ------
pm <- read_excel(paste0(url_file,"/LCI_ecoinvent311.xlsx"),
                 sheet="TRACI2.2_PM2.5",range="A19:G67")
# in kg PM2.5 eq


## Ozone depletion ----
o3 <- read_excel(paste0(url_file,"/LCI_ecoinvent311.xlsx"),
                 sheet="TRACI2.2_Ozone",range="A19:G78")
# in kg CFC11 eq

## Smog formation ----
smog <- read_excel(paste0(url_file,"/LCI_ecoinvent311.xlsx"),
                   sheet="TRACI2.2_Smog",range="A19:G525")
# in kg O3 eq

## Acidification
ad <- read_excel(paste0(url_file,"/LCI_ecoinvent311.xlsx"),
                 sheet="TRACI2.2_Acidification",range="A19:G54")
# in kg SO2 eq

## Primary Net Energy consumption ------
en <- read_excel(paste0(url_file,"/LCI_ecoinvent311.xlsx"),
                 sheet="net_energy",range="A16:G247")
# in MJ


# Combine all ------

# get unique combination of flows

df <- tibble(Flow=unique(
  c(gwp$Flow,en$Flow,ad$Flow,o3$Flow,pm$Flow,smog$Flow))) %>% 
  left_join(dplyr::select(gwp,Flow,`1 [Flow] = * kg CO2 eq.`)) %>% 
  left_join(dplyr::select(en,Flow,`1 [Flow] = * MJ`)) %>% 
  left_join(dplyr::select(ad,Flow,`1 [Flow] = * kg SO2 eq.`)) %>% 
  left_join(dplyr::select(o3,Flow,`1 [Flow] = * kg CFC 11 eq.`)) %>% 
  left_join(dplyr::select(pm,Flow,`1 [Flow] = * kg PM2.5 eq.`)) %>% 
  left_join(dplyr::select(smog,Flow,`1 [Flow] = * kg O3 eq.`))

names(df) <- str_remove(names(df),"1 \\[Flow\\] = \\* ") %>% 
  str_remove_all(" ") %>% str_replace("eq.","eq")

# Do NA as zero
df <- df %>% mutate(across(where(is.numeric), ~replace_na(., 0)))


## Categories ------

# Separate energy as renewable
df <- df %>% 
  mutate(renewable=if_else(str_detect(Flow,"Renewable"),1,0),
         MJ_nonRenewable=if_else(renewable==1,0,MJ))


write.csv(df,"Parameters/endpoints.csv",row.names = F)

# EoF