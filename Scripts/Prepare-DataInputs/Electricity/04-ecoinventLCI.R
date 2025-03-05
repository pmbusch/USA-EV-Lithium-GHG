# Work with LCI ecoinvent 3.9 datasets
# Extract GHG and local air pollutant emissions data
# PBH Feb 2025

library(tidyverse)
library(readxl)

url_file <- "Inputs"

# propietary data
(sheets <- excel_sheets(paste0(url_file,"/LCI_ecoinvent.xlsx")))

sheets <- sheets[-1:-3]


ecoinvent <- c()
for(s in sheets){
  
  # read entire sheet
  df <- read_excel(paste0(url_file,"/LCI_ecoinvent.xlsx"),sheet=s,
                   .name_repair = "unique_quiet")
  # get metadata
  name <- df[2,3][[1]]
  cat(name,"\n")
  region <- df[2,2][[1]]
  
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
  
  df_aux <- rbind(inputs,outputs) %>% 
    mutate(Name=name,Region=region,Year=ref_year,sheet=s)
  
  # join to master
  ecoinvent <- rbind(ecoinvent,df_aux)
  
  rm(df,df_aux,inputs,outputs,name,region,ref_year,pos_end,
     pos_inputs,pos_outputs)
}
nrow(ecoinvent)/1e6 #0.3M

unique(ecoinvent$Name)
unique(ecoinvent$Region)

# GWP values
gwp <- read_excel(paste0(url_file,"/LCI_ecoinvent.xlsx"),
                  sheet="GWP100_AR6",range="A22:G329")

# Functional Unit
ecoinvent %>% filter(Type=="Outputs") %>%
  group_by(sheet,Name) %>% slice(1)

# Remove heat as output
ecoinvent <- ecoinvent %>% filter(str_detect(Name,"electricity"))


# Add GWP and estimate emissions
df_gwp <- ecoinvent %>% rename(Flow=Flows) %>% 
  left_join(gwp,by="Flow")
names(df_gwp)
df_gwp <- df_gwp %>% filter(!is.na(`1 [Flow] = * kg CO2 eq.`))
unique(df_gwp$Units) # all in kg

# estimate kg and aggregate by process
df_gwp <- df_gwp %>% 
  mutate(Amount=as.numeric(Amount)) %>% 
  mutate(co2e=Amount*`1 [Flow] = * kg CO2 eq.`) %>%  # kg CO2e
  group_by(sheet,Name,Region) %>% 
  reframe(kg_co2e=sum(co2e)) %>% ungroup()

# save
write.csv(df_gwp,"Parameters/ecoinvent_GHG_electricity.csv",row.names = F)

# Figure  - could load results if faster
# df_gwp <- read.csv("Parameters/ecoinvent_GHG_electricity.csv")
data_fig <- df_gwp %>% 
  mutate(Name=str_remove(Name,"electricity production, ") %>% 
           str_remove("electricity, high voltage, ")) %>% 
  filter(!str_detect(Name,"import|voltage"))

order_name <- data_fig %>% group_by(Name) %>% 
  reframe(x=mean(kg_co2e)) %>% arrange((x)) %>% pull(Name)
data_fig <- data_fig %>% mutate(Name=factor(Name,levels=order_name))
  
ggplot(data_fig,aes(Name,kg_co2e))+
  geom_col(aes(fill=Region),position = "dodge")+
  coord_flip(expand = F)+
  guides(fill=guide_legend(ncol=2,reverse = T))+
  labs(y="kg CO2e per kWh Electricity",x="")+
  theme_bw(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = c(0.7,0.3))

ggsave("Figures/ecoinvent.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=8.7*2,height=8.7)
        
        
# GET local air pollutants

pollutants <- c("Dust \\(PM2\\.5\\)","Dust \\(> PM10\\)",
                "Sulphur dioxide","Nitrogen oxides","Carbon monoxide",
                "VOC","Ammonia")

head(ecoinvent)
local <- ecoinvent %>% 
  filter(str_detect(Flows,"emissions to air|Particles to air")) %>%
  filter(str_detect(Flows,paste(pollutants,collapse = "|"))) %>% 
  mutate(Pollutant=str_extract(Flows,paste(pollutants,collapse = "|")))
unique(local$Type)
unique(local$Pollutant)

# sum different local air pollutans
unique(local$Units) # all kg
local <- local %>% 
  mutate(Amount=as.numeric(Amount)) %>% 
  group_by(sheet,Name,Region,Pollutant) %>% 
  reframe(kg_per_kwh=sum(Amount)) %>% ungroup()

# save
write.csv(local,"Parameters/ecoinvent_LocalPollutants_electricity.csv",row.names = F)

# Figure  - could load results if faster
# local <- read.csv("Parameters/ecoinvent_LocalPollutants_electricity.csv")
data_fig <- local %>% 
  mutate(Name=str_remove(Name,"electricity production, ") %>% 
           str_remove("electricity, high voltage, ")) %>% 
  filter(!str_detect(Name,"import|voltage"))

order_name <- data_fig %>% group_by(Name) %>% 
  reframe(x=mean(kg_per_kwh)) %>% arrange((x)) %>% pull(Name)
data_fig <- data_fig %>% mutate(Name=factor(Name,levels=order_name))

ggplot(data_fig,aes(Name,kg_per_kwh))+
  geom_col(aes(fill=Region),position = "dodge")+
  facet_wrap(~Pollutant,scales="free_x")+
  coord_flip(expand = F)+
  guides(fill=guide_legend(ncol=2,reverse = T))+
  labs(y="kg per kWh Electricity",x="")+
  theme_bw(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = c(0.7,0.2))

ggsave("Figures/ecoinventLocalPollutants.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=8.7*3,height=8.7*2)

# EoF