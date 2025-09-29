# Work with LCI ecoinvent 3.11 datasets
# Extract Material Usage as well
# PBH Feb 2025

library(tidyverse)
library(readxl)
theme_set(theme_bw(8)+ theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()))

url_file <- "Inputs"

# propietary data - ecoinvent 3.11
(sheets <- excel_sheets(paste0(url_file,"/LCI_ecoinvent311.xlsx")))

# pick only upstream impacts: 
# fuel prod
# vehicle manuf
# lib prod
# car maintenance
(sheets <- sheets[str_detect(sheets,"up|maintenance")])

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
  
  # Functional UNIT
  FU <- df_aux %>% filter(Type=="Outputs") %>% slice(1) %>% 
    mutate(FU=paste0(Amount," ",Units)) %>% 
    pull(FU)
  
  df_aux$fu <- FU
  
  # join to master
  ecoinvent <- rbind(ecoinvent,df_aux)
  
  rm(df,df_aux,inputs,outputs,name,region,ref_year,pos_end,
     pos_inputs,pos_outputs)
}
nrow(ecoinvent)/1e6 #0.03M

unique(ecoinvent$fu)

# Endpoints -----

end <- read.csv("Parameters/endpoints.csv")
head(end)

# Add endpoints and estimate impacts for flows
df <- ecoinvent %>% rename(Flow=Flows) %>% 
  left_join(end,by="Flow")
names(df)

# refill NA as 0
df <- df %>% mutate(across(c("kgCO2eq","MJ","kgSO2eq","kgCFC11eq","kgPM2.5eq","kgO3eq","MJ_nonRenewable"), ~replace_na(., 0)))

# get energy consumption detail
energy <- df %>% 
  mutate(Amount=as.numeric(Amount)) %>% 
  mutate(MJ=Amount*MJ) %>% 
  # filter(!near(MJ,0)) %>%
  # filter(str_detect(Flow,"resource")) %>%
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
  group_by(sheet,Name,Region,fu,prim_energy) %>% 
  reframe(MJ=sum(MJ)) %>% 
  pivot_wider(names_from = prim_energy, values_from = MJ)
    
# estimate impact per kWh
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

# add detail
names(energy)
df <- df %>% left_join(energy)

# save
write.csv(df,"Parameters/Manufacturing/ecoinvent_upstream.csv",row.names = F)


# Get material usage in manufacturing (from Inputs)
mat <- ecoinvent %>% 
  filter(Type=="Inputs") %>% 
  filter(str_detect(Flows,"Non renewable elements")) %>% 
  mutate(Material=str_remove(Flows," \\[Non renewable elements\\]")) %>% 
  mutate(Amount=as.numeric(Amount))
unique(mat$Units)

mat <- mat %>% 
  group_by(sheet,Name,Region,fu,Material) %>% 
  reframe(kgMat=sum(Amount)) %>% ungroup()

unique(mat$Material)
mat %>% 
  filter(str_detect(Name,"battery production|passenger car production")) %>% 
  group_by(Material) %>% 
  reframe(kgMat=mean(kgMat)) %>% 
  arrange(desc(kgMat))

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
  group_by(sheet,Name,Region,fu) %>% 
  reframe(kgMetal=sum(kgMat*addMetal),
          kgMat=sum(kgMat)) %>% ungroup()

mat_agg2 <- mat %>% 
  # filter(Material %in% mat_interest) %>% 
  filter(Material %in% top_mats) %>% 
  group_by(sheet,Name,Region,fu,Material) %>% 
  reframe(kgMat=sum(kgMat)) %>% ungroup() %>% 
  mutate(Material=paste0("kg",Material)) %>% 
  pivot_wider(names_from = Material, values_from = kgMat)
          
mat_agg <- left_join(mat_agg1,mat_agg2)

write.csv(mat_agg,"Parameters/Manufacturing/ecoinvent_upstream_material.csv",row.names = F)


mat <- mat %>% 
  mutate(mat_label=if_else(kgMat>0.5,Material,""))

mat %>% 
  filter(str_detect(fu,"kg")) %>% 
  filter(kgMat>0.001) %>% 
  # filter(Material %in% mat_interest) %>%
  ggplot(aes(Name,kgMat,fill=Material))+
  geom_col()+
  geom_text(aes(label=mat_label),position = position_stack(vjust=0.5),
            size=6*5/14 * 0.8)+
  coord_flip()+
  labs(x="",y="kg per kg")+
  # guides(fill=guide_legend(nrow=4,reverse = T))+
  guides(fill=guide_legend(ncol=3,reverse=T,byrow=T))+
  theme(legend.text = element_text(size=5),
        legend.position = "bottom")


# EoF