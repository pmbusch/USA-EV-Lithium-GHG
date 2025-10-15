# Work with LCI ecoinvent 3.11 datasets
# Extract GHG and local air pollutant emissions data
# Electricity impacts
# PBH Feb 2025

library(tidyverse)
library(readxl)

url_file <- "Inputs"

# propietary data - ecoinvent 3.11
(sheets <- excel_sheets(paste0(url_file,"/LCI_ecoinvent311.xlsx")))

# pick only electricity producttion
sheets <- str_subset(sheets,"Sheet")

# Loop LCI ecoinvent ------------
ecoinvent <- c()
for(s in sheets){
  
  # read entire sheet
  df <- read_excel(paste0(url_file,"/LCI_ecoinvent311.xlsx"),sheet=s,
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
nrow(ecoinvent)/1e6 #0.2M

unique(ecoinvent$Name)
unique(ecoinvent$Region)


# Endpoints -----

end <- read.csv("Parameters/endpoints.csv")
head(end)

# Functional Unit - per kWh (or 3.6 MJ)
ecoinvent %>% filter(Type=="Outputs") %>%
  group_by(sheet,Name) %>% slice(1) %>% 
  pull(Units) %>% unique()
  # pull(Amount) %>% unique()


# Add endpoints and estimate impacts for flows
df_all <- ecoinvent %>% rename(Flow=Flows) %>% 
  left_join(end,by="Flow")
names(df_all)

# refill NA as 0
df_all <- df_all %>% mutate(across(c("kgCO2eq","MJ","kgSO2eq","kgCFC11eq","kgPM2.5eq","kgO3eq","MJ_nonRenewable"), ~replace_na(., 0)))

# get primary energy consumption detail
energy <- df_all %>% 
  mutate(Amount=as.numeric(Amount)) %>% 
  mutate(MJ=Amount*MJ) %>% 
  mutate(prim_energy=case_when(
    str_detect(Flow,"oil") ~ "MJ_Crudeoil",
    str_detect(Flow,"Natural gas") ~ "MJ_Naturalgas",
    str_detect(Flow,"Coal") ~ "MJ_Coal",
    str_detect(Flow,"wind") ~ "MJ_Wind",
    str_detect(Flow,"solar") ~ "MJ_Solar",
    str_detect(Flow,"Biomass") ~ "MJ_Biomass",
    str_detect(Flow,"hydro") ~ "MJ_Hydro",
    str_detect(Flow,"geothermal") ~ "MJ_Geothermal",
    str_detect(Flow,"Uranium") ~ "MJ_Uranium",
    T ~ "MJ_Other"))
energy %>% group_by(prim_energy) %>% reframe(x=sum(MJ)) %>% arrange(desc(x))
energy <- energy %>% 
  group_by(sheet,Name,Region,prim_energy) %>% 
  reframe(MJ=sum(MJ)) %>% 
  pivot_wider(names_from = prim_energy, values_from = MJ)


# get GHG emissions detail for discounting
dict_ghg <- read_excel("Inputs/TAWP_AR6.xlsx",sheet="Match_ecoinvent")  
ghg <- df_all %>% 
  mutate(Amount=as.numeric(Amount)) %>% 
  left_join(dict_ghg) %>% 
  filter(!is.na(Sign)) %>% 
  mutate(Amount=Amount*Sign) %>% 
  group_by(sheet,Name,Region,Match) %>% 
  reframe(Amount=sum(Amount)) %>% 
  mutate(Match=paste0("kg_",Match)) %>% 
  pivot_wider(names_from = Match, values_from = Amount)


# estimate impact per kWh
df <- df_all %>% 
  mutate(Amount=as.numeric(Amount)) %>% 
  group_by(sheet,Name,Region) %>% 
  reframe(kgCO2eq=sum(Amount*kgCO2eq),
          MJ=sum(Amount*MJ),
          MJ_nonRenewable=sum(Amount*MJ_nonRenewable),
          kgSO2eq=sum(Amount*kgSO2eq),
          kgCFC11eq=sum(Amount*kgCFC11eq),
          kgPM2.5eq=sum(Amount*kgPM2.5eq),
          kgO3eq=sum(Amount*kgO3eq)) %>% 
  ungroup()

df <- df %>% left_join(energy) %>% left_join(ghg)

# save
write.csv(df,"Parameters/Electricity/ecoinvent_electricity.csv",row.names = F)

# Figure -----  
# - could load results if faster
# df <- read.csv("Parameters/Electricity/ecoinvent_electricity.csv")

## GWP ----

data_fig <- df %>% 
  mutate(Name=str_remove(Name,"electricity production, ") %>% 
           str_remove("electricity, high voltage, ") %>% 
           str_remove(" to generic market for electricity, medium voltage") %>% 
           str_remove("electricity, from ")) %>% 
  filter(!str_detect(Name,"import|voltage"))

order_name <- data_fig %>% group_by(Name) %>% 
  reframe(x=mean(kgCO2eq)) %>% arrange((x)) %>% pull(Name)
data_fig <- data_fig %>% mutate(Name=factor(Name,levels=order_name))
  
ggplot(data_fig,aes(Name,kgCO2eq))+
  geom_col(aes(fill=Region),position = "dodge")+
  coord_flip(expand = F)+
  guides(fill=guide_legend(ncol=2,reverse = T))+
  labs(y="kg CO2e per kWh Electricity",x="")+
  theme_bw(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = c(0.7,0.3))

ggsave("Figures/Electricity/ecoinvent.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=8.7*2,height=8.7)

## Others ----        

data_fig <- df %>% 
  mutate(Name=str_remove(Name,"electricity production, ") %>% 
           str_remove("electricity, high voltage, ") %>% 
           str_remove(" to generic market for electricity, medium voltage") %>% 
           str_remove("electricity, from ")) %>% 
  filter(!str_detect(Name,"import|voltage"))

data_fig <- data_fig %>% 
  dplyr::select(Name,Region,kgSO2eq,kgCFC11eq,kgPM2.5eq,kgO3eq) %>% 
  pivot_longer(c(-Name,-Region), names_to = "impact", values_to = "value") %>% 
  mutate(impact=case_when(
    impact=="kgPM2.5eq" ~ paste0("Human Health Particulate Air"," ",impact,"/kWh"),
    impact=="kgCFC11eq" ~ paste0("Ozone depletion"," ",impact,"/kWh"),
    impact=="kgSO2eq" ~ paste0("Acidification"," ",impact,"/kWh"),
    impact=="kgO3eq" ~ paste0("Smog formation"," ",impact,"/kWh"),
    T ~ "NA"))
  
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

ggsave("Figures/Electricity/ecoinventOtherImpacts.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=8.7*3,height=8.7*2)



## Primary Net Energy consumption ------
# Show it by primary energy source

data_fig <- df_all %>% 
  mutate(Name=str_remove(Name,"electricity production, ") %>% 
           str_remove("electricity, high voltage, ") %>% 
           str_remove(" to generic market for electricity, medium voltage") %>% 
           str_remove("electricity, from ")) %>% 
  filter(!str_detect(Name,"import|voltage")) %>% 
  filter(MJ>0) %>% 
  mutate(Amount=as.numeric(Amount)) %>%
  mutate(MJ=Amount*MJ,MJ_nonRenewable=Amount*MJ_nonRenewable) %>% 
  group_by(Name,Flow,Region) %>% 
  reframe(MJ=sum(MJ),MJ_nonRenewable=sum(MJ_nonRenewable)) %>% ungroup()
  
# average by region
data_fig <- data_fig %>% 
  group_by(Name,Flow) %>% 
  reframe(MJ=mean(MJ),MJ_nonRenewable=mean(MJ_nonRenewable)) %>% ungroup()

data_fig <- data_fig %>% 
  pivot_longer(c(-Name,-Flow), names_to = "impact", values_to = "value") %>% 
  mutate(impact=case_when(
    impact=="MJ" ~  "All primary energy",
    impact=="MJ_nonRenewable" ~ "Non Renewable Primary Energy",
    T ~ "NA"))

data_fig <- data_fig %>% 
  mutate(Flow=str_replace(Flow, "\\[.*", "") %>% str_trim())

order_name <- data_fig %>% group_by(Name) %>% 
  reframe(x=mean(value)) %>% arrange((x)) %>% pull(Name)
data_fig <- data_fig %>% mutate(Name=factor(Name,levels=order_name))

ggplot(data_fig,aes(Name,value))+
  geom_col(aes(fill=Flow))+
  facet_wrap(~impact)+
  coord_flip(expand = F)+
  scale_y_continuous(sec.axis = sec_axis(~ . /3.6, 
                                          name = "Net Primary Energy [kWh per kWh Electricity]"))+
  labs(y="Net Primary Energy [MJ per kWh Electricity]",x="",fill="")+
  guides(fill=guide_legend(nrow=4,reverse = T))+
  theme_bw(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = "bottom")

ggsave("Figures/Electricity/ecoinventNetEnergy.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=8.7*3,height=8.7*2)

# Material usage detail ------

mat <- ecoinvent %>% 
  filter(Type=="Inputs") %>% 
  filter(str_detect(Flows,"Non renewable elements")) %>% 
  mutate(Material=str_remove(Flows," \\[Non renewable elements\\]")) %>% 
  mutate(Amount=as.numeric(Amount))
unique(mat$Units)

mat <- mat %>% 
  group_by(sheet,Name,Region,Material) %>% 
  reframe(kgMat=sum(Amount)) %>% ungroup()

# save endpoints: total mass, total metal mass, and key minerals
mat_interest <- c("Lithium","Nickel","Cobalt","Copper","Zinc","REE","Aluminium")
metals <- read_excel("Inputs/Elements_Metals.xlsx")

# top ones based on content
top_mats <- metals %>% filter(Highlight=="Yes") %>% pull(Material)
top_mats <- c(top_mats,"REE")

mat <- mat %>% 
  left_join(metals) %>% 
  mutate(Material=if_else(Metal=="Rare earth metal","REE",Material)) %>% 
  mutate(addMetal=if_else(Metal %in% c("REE","Metal"),1,0)) %>% 
  mutate(topMat=if_else(Material %in% c("REE",top_mats),1,0))


mat_agg1 <- mat %>% 
  group_by(sheet,Name,Region) %>% 
  reframe(kgMetal=sum(kgMat*addMetal),
          kgMat=sum(kgMat)) %>% ungroup()

mat_agg2 <- mat %>% 
  # filter(Material %in% mat_interest) %>%
  filter(Material %in% top_mats) %>%
  group_by(sheet,Name,Region,Material) %>% 
  reframe(kgMat=sum(kgMat)) %>% ungroup() %>% 
  mutate(Material=paste0("kg",Material)) %>% 
  pivot_wider(names_from = Material, values_from = kgMat)

mat_agg <- left_join(mat_agg1,mat_agg2)
          
# save
write.csv(mat_agg,"Parameters/Electricity/ecoinvent_electricity_material.csv",row.names = F)


## Figure ----

data_fig <- mat_agg %>% 
  mutate(Name=str_remove(Name,"electricity production, ") %>% 
           str_remove("electricity, high voltage, ") %>% 
           str_remove(" to generic market for electricity, medium voltage") %>% 
           str_remove("electricity, from ")) %>% 
  filter(!str_detect(Name,"import|voltage"))

data_fig <- data_fig %>% 
  mutate(kgMat=kgMat-kgMetal) %>% 
  pivot_longer(c(-sheet,-Name,-Region), names_to = "key", values_to = "kg") %>% 
  mutate(cat=if_else(key %in% c("kgMat","kgMetal"),"All Materials","Highlight Materials")) %>% 
  mutate(key=str_remove(key,"kg") %>% str_replace("Mat","Non-Metal"))
  
# average by region
data_fig <- data_fig %>% 
  group_by(Name,cat,key) %>% 
  reframe(kg=mean(kg)) %>% ungroup()


order_name <- data_fig %>% group_by(Name) %>% 
  reframe(x=mean(kg)) %>% arrange((x)) %>% pull(Name)
data_fig <- data_fig %>% mutate(Name=factor(Name,levels=order_name))

data_fig$kg <- data_fig$kg*1e3

#COLOR
mats <- MetBrewer::met.brewer("Signac", n = 9)
names(mats) <- unique(data_fig$key)


p1 <- ggplot(filter(data_fig,cat=="All Materials"),aes(Name,kg))+
  geom_col(aes(fill=key))+
  facet_wrap(~cat)+
  coord_flip(expand = F)+
  labs(y="Material requirements [kg per MWh]",x="",fill="")+
  guides(fill=guide_legend(nrow=2,reverse = T))+
  scale_fill_manual(values=mats)+
  theme_bw(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = c(0.8,0.2))
p1

p2 <- ggplot(filter(data_fig,cat!="All Materials"),aes(Name,kg))+
  geom_col(aes(fill=key))+
  facet_wrap(~cat)+
  coord_flip(expand = F)+
  labs(y="Material requirements [kg per MWh]",x="",fill="")+
  guides(fill=guide_legend(nrow=2,reverse = T))+
  scale_fill_manual(values=mats)+
  scale_x_discrete(labels=NULL)+
  theme_bw(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = c(0.6,0.2))
p2

cowplot::plot_grid(p1,p2,ncol = 2)

ggsave("Figures/Electricity/ecoinventMaterials.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=8.7*3,height=8.7*2)


# Fossil fuel consumption --------------

fossils <- end %>% filter(renewable==0,MJ>0) %>% pull(Flow) %>% unique()

# fossils <- c("Crude oil ecoinvent [Crude oil (resource)]",
#              "Coal, brown, in ground [Lignite (resource)]",
#              "Coal, hard, unspecified, in ground [Hard coal (resource)]",
#              "Gas, natural, in ground [Natural gas (resource)]",
#              "Gas, mine, off-gas, process, coal mining [Natural gas (resource)]")
             # "Shale [Non renewable resources]")


df_fossil <- ecoinvent %>% rename(Flow=Flows) %>% 
  mutate(fossilFuel=str_extract(Flow,"Coal|Crude oil|Natural gas|Uranium|Sulphur|Peat")) %>% 
  filter(Flow %in% fossils) %>% 
  filter(!is.na(Amount))
df_fossil %>% group_by(Flow,fossilFuel,Units) %>% tally() # all in kg, except gas in Nm3


# estimate kg and aggregate by process
df_fossil <- df_fossil %>% 
  mutate(qty=as.numeric(Amount)) %>% 
  group_by(sheet,Name,fossilFuel,Region) %>% 
  reframe(value=sum(qty)) %>% ungroup() %>% 
  # assume 0.8 kg per m3 for NG
  mutate(cf=if_else(fossilFuel=="Natural gas",0.8,1)) %>% 
  mutate(value=value*cf)

# save
write.csv(df_fossil,"Parameters/Electricity/ecoinvent_fossil_electricity.csv",row.names = F)

df_fossil <- df_fossil %>% 
  mutate(Name=str_remove(Name,"electricity production, ") %>% 
           str_remove("electricity, high voltage, ") %>% 
           str_remove(" to generic market for electricity, medium voltage") %>% 
           str_remove("electricity, from ")) %>% 
  filter(!str_detect(Name,"import|voltage"))

df_fossil <- df_fossil %>% 
  group_by(Name,fossilFuel) %>% 
  reframe(value=mean(value)) %>% ungroup()

order_name <- df_fossil %>% group_by(Name) %>% 
  reframe(x=mean(value)) %>% arrange((x)) %>% pull(Name)
df_fossil <- df_fossil %>% mutate(Name=factor(Name,levels=order_name))


ggplot(df_fossil,aes(Name,value))+
  geom_col(aes(fill=fossilFuel))+
  coord_flip(expand = F)+
  guides(fill=guide_legend(nrow=2,reverse = T))+
  labs(y="kg of fossil fuel per kWh Electricity",x="",fill="")+
  theme_bw(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position = c(0.7,0.3))

ggsave("Figures/Electricity/ecoinventFossilFuels.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=8.7*3,height=8.7*2)

# EoF