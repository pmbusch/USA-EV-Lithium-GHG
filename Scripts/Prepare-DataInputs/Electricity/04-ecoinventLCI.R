# Work with LCI ecoinvent 3.9 datasets
# Extract GHG and local air pollutant emissions data
# PBH Feb 2025

library(tidyverse)
library(readxl)

url_file <- "Inputs"

# propietary data
(sheets <- excel_sheets(paste0(url_file,"/LCI_ecoinvent.xlsx")))

sheets <- sheets[-1:-7]

# Loop LCI ecoinvent ------------
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

# GWP values AR6 ------
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
        
# Other Impact Categories - Traci 2.1 ----------

## Human Health PM2.5 ------
pm <- read_excel(paste0(url_file,"/LCI_ecoinvent.xlsx"),
                  sheet="TRACI2.1_PM2.5",range="A19:G67")
df_pm <- ecoinvent %>% rename(Flow=Flows) %>% 
  left_join(pm,by="Flow")
names(df_pm)
df_pm <- df_pm %>% filter(!is.na(`1 [Flow] = * kg PM2.5 eq.`))
unique(df_pm$Units) # all in kg

# estimate kg and aggregate by process
df_pm <- df_pm %>% 
  mutate(Amount=as.numeric(Amount)) %>% 
  mutate(qty=Amount*`1 [Flow] = * kg PM2.5 eq.`) %>% 
  group_by(sheet,Name,Region) %>% 
  reframe(value=sum(qty)) %>% ungroup() %>% 
  mutate(unit="kg PM2.5eq/kWh") %>% 
  mutate(impact="Human Health Particulate Air")

## Ozone depletion -----
o3 <- read_excel(paste0(url_file,"/LCI_ecoinvent.xlsx"),
                 sheet="TRACI2.1_Ozone",range="A19:G74")
df_o3 <- ecoinvent %>% rename(Flow=Flows) %>% 
  left_join(o3,by="Flow")
names(df_o3)
df_o3 <- df_o3 %>% filter(!is.na(`1 [Flow] = * kg CFC 11 eq.`))
unique(df_o3$Units) # all in kg

# estimate kg and aggregate by process
df_o3 <- df_o3 %>% 
  mutate(Amount=as.numeric(Amount)) %>% 
  mutate(qty=Amount*`1 [Flow] = * kg CFC 11 eq.`) %>% 
  group_by(sheet,Name,Region) %>% 
  reframe(value=sum(qty)) %>% ungroup() %>% 
  mutate(unit="kg CFC11eq/kWh") %>% 
  mutate(impact="Ozone depletion")

## Smog formation -----
smog <- read_excel(paste0(url_file,"/LCI_ecoinvent.xlsx"),
                 sheet="TRACI2.1_Smog",range="A19:G523")
df_smog <- ecoinvent %>% rename(Flow=Flows) %>% 
  left_join(smog,by="Flow")
names(df_smog)
df_smog <- df_smog %>% filter(!is.na(`1 [Flow] = * kg O3 eq.`))
unique(df_smog$Units) # all in kg

# estimate kg and aggregate by process
df_smog <- df_smog %>% 
  mutate(Amount=as.numeric(Amount)) %>% 
  mutate(qty=Amount*`1 [Flow] = * kg O3 eq.`) %>% 
  group_by(sheet,Name,Region) %>% 
  reframe(value=sum(qty)) %>% ungroup() %>% 
  mutate(unit="kg O3eq/kWh") %>% 
  mutate(impact="Smog formation")

## Acidification ------
ad <- read_excel(paste0(url_file,"/LCI_ecoinvent.xlsx"),
                 sheet="TRACI2.1_Acidification",range="A19:G79")
df_ad <- ecoinvent %>% rename(Flow=Flows) %>% 
  left_join(ad,by="Flow")
names(df_ad)
df_ad <- df_ad %>% filter(!is.na(`1 [Flow] = * kg SO2 eq.`))
unique(df_ad$Units) # all in kg

# estimate kg and aggregate by process
df_ad <- df_ad %>% 
  mutate(Amount=as.numeric(Amount)) %>% 
  mutate(qty=Amount*`1 [Flow] = * kg SO2 eq.`) %>% 
  group_by(sheet,Name,Region) %>% 
  reframe(value=sum(qty)) %>% ungroup() %>% 
  mutate(unit="kg SO2eq/kWh") %>% 
  mutate(impact="Acidification")

# Join all others impacts
df_others <- rbind(df_pm,df_o3,df_smog,df_ad)

# save
write.csv(df_others,
          "Parameters/ecoinvent_OtherImpacts_electricity.csv",row.names = F)


# Figure  - could load results if faster
# df_others <- read.csv("Parameters/ecoinvent_OtherImpacts_electricity.csv")
data_fig <- df_others %>% 
  mutate(Name=str_remove(Name,"electricity production, ") %>% 
           str_remove("electricity, high voltage, ")) %>% 
  filter(!str_detect(Name,"import|voltage")) %>% 
  mutate(impact=paste0(impact," ",unit))

order_name <- data_fig %>% group_by(Name) %>% 
  reframe(x=mean(value)) %>% arrange((x)) %>% pull(Name)
data_fig <- data_fig %>% mutate(Name=factor(Name,levels=order_name))

ggplot(data_fig,aes(Name,value))+
  geom_col(aes(fill=Region),position = "dodge")+
  facet_wrap(~impact,scales="free_x")+
  coord_flip(expand = F)+
  guides(fill=guide_legend(ncol=2,reverse = T))+
  labs(y="kg per kWh Electricity",x="")+
  theme_bw(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = c(0.7,0.2))

ggsave("Figures/ecoinventOtherImpacts.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=8.7*3,height=8.7*2)

# EoF