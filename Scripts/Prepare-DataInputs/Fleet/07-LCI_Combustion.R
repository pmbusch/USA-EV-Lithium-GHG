# PBH September 2025
# based on combustion. 
# u-so process - does not include upstream, just this 
# we add upstream petrol production
# ecoinvent 3.11

library(tidyverse)
library(readxl)
theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()))

url_file <- "Inputs"

# propietary data - ecoinvent 3.11

# Combustion -----

# read entire sheet
df <- read_excel(paste0(url_file,"/LCI_ecoinvent311.xlsx"),sheet="u-so car",
                 .name_repair = "unique_quiet")
# get metadata
name <- df[2,3][[1]] # large size EURO 5
cat(name,"\n")
region <- df[2,2][[1]] # RER

ref_year <- df[min(which(df[,1]=="Reference year")),2][[1]]

# read inputs and outputs
pos_inputs <- which(df[,1]=="Inputs")
pos_outputs <- which(df[,1]=="Outputs")

inputs <- df[(pos_inputs+2):(pos_outputs-1),]
colnames(inputs) <- df[(pos_inputs+1),]

pos_end <- which(df[,1]=="VF type")
outputs <- df[(pos_outputs+2):(pos_end-1),]
colnames(outputs) <- df[(pos_outputs+1),]

#join them
inputs$Type <- "Inputs"
outputs$Type <- "Outputs"

df <- rbind(inputs,outputs) %>% 
  mutate(Name=name,Region=region,Year=ref_year,sheet="u-so car")

# Functional UNIT
FU <- df %>% filter(Type=="Outputs") %>% slice(1) %>% 
  mutate(FU=paste0(Amount," ",Units)) %>% 
  pull(FU)

df$fu <- FU # 1km

# convert flows to per kg of fuel petrol, low sulfur (combustion)
(mass <- df %>% filter(Type=="Inputs") %>% 
  filter(str_detect(Flows,"petrol, low"))) 
mass <- sum(as.numeric(mass$Amount))

df <- df %>% 
  mutate(fu="1 kg") %>% 
  mutate(Amount=as.numeric(Amount)/mass)

# 3.18 kg CO2 per kg of petrol
  
# petrol density ~ 0.74 kg/L
3.18*0.74 # kg CO2 per liter
3.18*0.74*3.78541 # kg CO2 per gallon; 1 gallon = 3.78 liters

# Embedded fuel consumption - mpg
(1/1.6)/(mass/0.74/3.78541) # 23.7 mpg

# Endpoints 

end <- read.csv("Parameters/endpoints.csv")

# Add endpoints and estimate impacts for flows
df <- df %>% rename(Flow=Flows) %>% 
  left_join(end,by="Flow")
# refill NA as 0
df <- df %>% mutate(across(c("kgCO2eq","MJ","kgSO2eq","kgCFC11eq","kgPM2.5eq","kgO3eq","MJ_nonRenewable"), ~replace_na(., 0)))

# NOTE: No energy consumption at u-so level

# estimate impact per kg
df <- df %>% 
  mutate(Amount=as.numeric(Amount)) %>% 
  group_by(sheet,Name,Region,fu) %>% 
  reframe(kgCO2eq=sum(Amount*kgCO2eq),
          MJ=sum(Amount*MJ),
          MJ_nonRenewable=sum(Amount*MJ_nonRenewable),
          kgSO2eq=sum(Amount*kgSO2eq),
          kgCFC11eq=sum(Amount*kgCFC11eq),
          kgPM2.5eq=sum(Amount*kgPM2.5eq),
          kgO3eq=sum(Amount*kgO3eq)) %>% 
  ungroup()


# Petrol production ----
## add upstream emissions petrol

upstream <- read.csv("Parameters/Manufacturing/ecoinvent_upstream.csv")
up_mat <- read.csv("Parameters/Manufacturing/ecoinvent_upstream_material.csv")

upstream <- upstream %>% left_join(up_mat)

petrol <- upstream %>% 
  filter(str_detect(Name,"petrol production")) 

## add combustion to petrol
names(df)
petrol$kgCO2eq <- petrol$kgCO2eq+df$kgCO2eq
petrol$MJ <- petrol$MJ+df$MJ
petrol$MJ_nonRenewable <- petrol$MJ_nonRenewable+df$MJ_nonRenewable
petrol$kgSO2eq <- petrol$kgSO2eq+df$kgSO2eq
petrol$kgCFC11eq <- petrol$kgCFC11eq+df$kgCFC11eq
petrol$kgPM2.5eq <- petrol$kgPM2.5eq+df$kgPM2.5eq
petrol$kgO3eq <- petrol$kgO3eq+df$kgO3eq

write.csv(petrol,"Parameters/Operation/LCI_petrol.csv",row.names = F)

# EoF