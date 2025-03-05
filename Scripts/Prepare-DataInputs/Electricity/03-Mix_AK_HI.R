# eGrid data for Hawai and Alaska
# States not present on EIA data
# Mix of electricity fuel
# Source: https://www.epa.gov/egrid/detailed-data
# PBH February 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")

url_file <- "Inputs"

# 2023 data ------

# data on combined vs conventional cycle, based on unit
unit <- read_excel(paste0(url_file,"/egrid2023_data_rev1.xlsx"),
                   sheet = "GEN23",skip = 1)
unit <- unit %>% filter(PSTATEABB %in% c("AK","HI")) %>% 
  filter(FUELG1=="NG") # Natural Gas Only
unique(unit$PRMVR)
# codes: https://www.epa.gov/egrid/code-lookup
unit <- unit %>% 
  mutate(type_ng=case_when(
    PRMVR %in% c("CA","") ~ "Combined Cycle",
    PRMVR %in% c("CT","GT","IC") ~ "Conventional"
  ))

unit <- unit %>% group_by(PSTATEABB,type_ng) %>% 
  reframe(value=sum(NAMEPCAP,na.rm = T)) %>%  # Generation capacity MW
  ungroup() %>% group_by(PSTATEABB) %>% 
  mutate(share=value/sum(value))


# eGrid region
egrid <- read_excel(paste0(url_file,"/egrid2023_data_rev1.xlsx"),
                    sheet = "SRL23",skip = 1)
names(egrid)

egrid <- egrid %>% 
  filter(str_detect(SRNAME,"ASCC|HICC")) %>% 
  dplyr::select(SRNAME,SRGENACL,SRGENAOL,SRGENAGS,SRGENANC,SRGENAHY,
                SRGENABM,SRGENAWI,SRGENASO,SRGENAGT,SRGENAOF) %>% 
  pivot_longer(c(-SRNAME), names_to = "key", values_to = "value") # all in MWh
  

egrid <- egrid %>% 
  mutate(fuel=case_when(
    key=="SRGENACL" ~ "Coal",
    key=="SRGENAOL" ~ "Petroleum",
    key=="SRGENAGS" ~ "Natural Gas Conventional", # assume all conventional for HI
    key=="SRGENANC" ~ "Nuclear",
    key=="SRGENAHY" ~ "Hydropower",
    key=="SRGENABM" ~ "Wood and Other Biomass",
    key=="SRGENAWI" ~ "Wind",
    key=="SRGENASO" ~ "Solar Photovoltaic",
    key=="SRGENAGT" ~ "Geothermal",
    key=="SRGENAOF" ~ "Petroleum")) # other fossil, assume oil


# Add natural gas detail
egrid_ng <- egrid %>% 
  filter(fuel=="Natural Gas Conventional",str_detect(SRNAME,"ASCC"))
# shares based on cap
unit$value <- unit$PSTATEABB <- NULL
egrid_ng <- egrid_ng %>% 
  mutate(dummy=1) %>% 
  left_join(mutate(unit,dummy=1)) %>% 
  mutate(fuel=paste0("Natural Gas ",type_ng),
         value=value*share)
egrid_ng$dummy <- egrid_ng$type_ng <- egrid_ng$share <- NULL

egrid <- egrid %>%
  filter(fuel!="Natural Gas Conventional"|str_detect(SRNAME,"HICC")) %>% 
  rbind(egrid_ng)


ggplot(egrid,aes(SRNAME,value,fill=fuel))+
  geom_col()+coord_flip()

# Forecast -----

# Hawaii: 100% renewable by 2045 
# https://www.eia.gov/state/print.php?sid=HI
# Do linear forecast
# Assumptions: goal is for each system

# current 
egrid_hi <- egrid %>% filter(str_detect(SRNAME,"HICC")) %>%
  # renewables according to EIA table 56
  mutate(renewable=fuel %in% c("Geothermal","Solar Photovoltaic","Wind",
                               "Wood and Other Biomass","Hydropower"))
# current share
egrid_hi %>% group_by(SRNAME,renewable) %>% reframe(x=sum(value)) %>%
  ungroup() %>% group_by(SRNAME) %>% mutate(perc=x/sum(x)) # 12.4% and 34.1% 

# 
linear_renewable <- expand.grid(period=2022:2050,
                                SRNAME=unique(egrid_hi$SRNAME)) %>%
  mutate(level2023=if_else(str_detect(SRNAME,"Oahu"),0.142,0.341)) %>% 
  # multiplicative factor for renewables
  mutate(goal=case_when(
    period<2024 ~ level2023,
    period<2045 ~ level2023+(period-2023)*(1-level2023)/(2045-2023),
    T ~ 1)) %>% 
  mutate(multFactor=goal/level2023) %>% 
  mutate(renewable=T,goal=NULL) 
linear_renewable2 <- linear_renewable %>% 
  # inverse case for non-renewable
  mutate(goal=case_when(
    period<2024 ~ 1-level2023,
    period<2045 ~ 1-level2023-(period-2023)*(1-level2023)/(2045-2023),
    T ~ 0)) %>% 
  mutate(multFactor=goal/(1-level2023)) %>% 
  mutate(renewable=F,goal=NULL)
linear_renewable <- rbind(linear_renewable,linear_renewable2)    
linear_renewable$dummy=1


# project Hawaii shares  
egrid_hi <- egrid_hi %>% 
  group_by(SRNAME) %>% mutate(share=value/sum(value)) %>% ungroup() %>% 
  mutate(dummy=1) %>% left_join(linear_renewable) %>% 
  mutate(share=share*multFactor) %>% 
  mutate(dummy=NULL,multFactor=NULL,level2023=NULL,renewable=NULL)

  
# Alaska: keep share of 2023
egrid_ak <- egrid %>% filter(str_detect(SRNAME,"ASCC")) %>%
  filter(value>0) %>% 
  group_by(SRNAME) %>% mutate(share=value/sum(value)) %>% ungroup() %>% 
  mutate(dummy=1) %>% left_join(tibble(period=2022:2050,dummy=1)) %>% 
  mutate(dummy=NULL)
  
egrid_shares <- rbind(egrid_hi,egrid_ak)

egrid_shares %>% group_by(SRNAME,period) %>% reframe(x=sum(share)) %>% arrange(desc(x))

ggplot(egrid_shares,aes(period,share,fill=fuel))+
  geom_col(position = "fill")+
  facet_wrap(~SRNAME)+
  coord_cartesian(expand = F)+
  labs(x="",y="",title="Electricity generation mix",fill="")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw(8)+
  theme(panel.grid = element_blank())

ggsave("Figures/AK_HI_Mix.png", 
       ggplot2::last_plot(),units="cm",dpi=600,width=8.7*2,height=8.7)


# save -----
write.csv(egrid_shares,"Parameters/HI_AK_mix.csv",row.names = F)

# EoF