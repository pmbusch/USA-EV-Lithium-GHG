# Download EIA Electricity Data
# GHG Estimation module
# Source: EIA Annual Energy Outlook
# https://www.eia.gov/outlooks/aeo/tables_ref.php
# PBH January 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Total Generation Data ------

## Load ----
df <- read.csv("Inputs/EIA_Table54.csv")
nrow(df) # 87464

#Dimensions
range(df$period) # 2022 to 2050
unique(df$regionName) # 25
unique(df$history);unique(df$scenario);unique(df$tableName)
unique(df$seriesId)

## Get Capacity data on Natural Gas -----
# Purpose: estimate shares of conventional vs combined cycle
ng <- df %>% 
  filter(str_detect(seriesName,"Capacity")) %>%
  filter(str_detect(seriesName,"Power")) %>% 
  filter(str_detect(seriesName,"Combined|Combustion|Natural Gas"))
unique(ng$seriesName)
unique(ng$unit)

# Get shares of NG conventional-combined by year and region
# Steam is treated as combined
ng_share <- ng %>%
  mutate(type_ng=if_else(str_detect(seriesName,"Combustion"),
                         "Conventional","Combined Cycle")) %>% 
  mutate(value=as.numeric(value)) %>% 
  group_by(regionId,regionName,period,type_ng) %>% 
  reframe(value=sum(value)) %>% ungroup()
ng_share <- ng_share %>%
  group_by(regionId,regionName,period) %>% 
  mutate(share=value/sum(value)) %>% ungroup()


## Filter data ---------
# filter by generation data
df <- df %>% filter(substr(seriesId,0,4)=="gen_") %>% 
  filter(str_detect(seriesName,"Electric Power")) # generation by fuel type for electric power
unique(df$seriesName)
unique(df$unit) # billion kWh

# remove duplicate results - or select only fuels of interest (no double counting)
df <- df %>% filter(str_detect(seriesName,
                               "Coal|Petroleum|Natural Gas|Nuclear|Renewable|Pumped|Distributed")) %>% 
  mutate(fuel=str_remove(seriesName,"Electricity : Electric Power Sector : Generation : "))

df <- df %>% mutate(value=as.numeric(value))

## Split natural gas into conventional and combined cycle -----
df_ng <- df %>% filter(fuel=="Natural Gas")
# shares based on cap
ng_share$value <- NULL
df_ng <- df_ng %>% left_join(ng_share) %>% 
  mutate(fuel=paste0(fuel," ",type_ng),
         value=value*share)
df_ng$type_ng <- df_ng$share <- NULL

df <- df %>%  filter(fuel!="Natural Gas") %>% 
  rbind(df_ng)

# Renewable generation data --------
df_ren <- read.csv("Inputs/EIA_Table56.csv")
nrow(df_ren) # 64090

#Dimensions
range(df_ren$period) # 2022 to 2050
unique(df_ren$regionName) # 25
unique(df_ren$history);unique(df_ren$scenario);unique(df_ren$tableName)
unique(df_ren$seriesId)

## Filter data ---------
# filter by generation data
df_ren <- df_ren %>% filter(substr(seriesId,0,4)=="gen_") %>% 
  filter(str_detect(seriesName,"Electric Power")) # generation by fuel type for electric power
unique(df_ren$seriesName)
unique(df_ren$unit) # billion kWh

# remove duplicate results - or select only fuels of interest (no double counting)
df_ren <- df_ren %>% 
  mutate(fuel=str_remove(seriesName,"Renewable Energy : Electric Power Sector : Electricity Generation : ")) %>% 
  filter(fuel!="Total")
df_ren <- df_ren %>% mutate(value=as.numeric(value))
unique(df_ren$fuel)

# Join them -------
# add detail of renewable energy (table 56) to table 54
unique(df$fuel)
# same value
df %>% filter(fuel=="Renewable Sources") %>% pull(value) %>% sum()/1e3
sum(df_ren$value)/1e3

df <- df %>% filter(fuel!="Renewable Sources")
df <- rbind(df,df_ren)

# region name simplification
df <- df %>%
  mutate(region=regionName %>% 
           str_replace("Western Electricity Coordinating Council","WECC") %>% 
           str_replace("Northeast Power Coordinating Council","NPCC") %>% 
           str_replace(" Reliability Corporation","") %>% 
           str_replace("Reliability Coordinating Council","RCC"))

# Save -----
df <- df %>% group_by(region,period) %>% mutate(share=value/sum(value)) %>% ungroup()
df %>% group_by(region,period) %>% reframe(x=sum(share)) %>% arrange(desc(x))

head(df)
write.csv(df,"Parameters/EIA_mix.csv",row.names = F)

# Figure -----
names(df)

# colors
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")
cat_colors <- names(fuel_colors)
df <- df %>% mutate(fuel=factor(fuel,levels=cat_colors))

# For United States
data_fig <- df %>% filter(str_detect(regionName,"United States"))
p1 <- ggplot(data_fig,aes(period,value))+
  geom_area(aes(fill=fuel))+
  coord_cartesian(expand = F)+
  scale_y_continuous(labels = scales::comma_format(big.mark = ' '))+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  labs(x="",y="",title="Generation [billion kWh]",fill="")+
  theme_bw(7)+ 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p1+scale_fill_manual(values=fuel_colors)


# For balancing region
# map: https://www.eia.gov/outlooks/aeo/pdf/nerc_map.pdf
data_fig2 <- df %>% 
  filter(!str_detect(regionName,"United States")) %>% 
  mutate(fuel=regionName) %>% 
  group_by(period,fuel) %>% 
  reframe(value=sum(value)) %>% ungroup()

p2 <- p1
p2$data <- data_fig2
p2+theme(legend.position ="none")

# Facet
p3 <- p1
p3$data <- df
p3+facet_wrap(~region,scales="free_y")+
  scale_fill_manual(values=fuel_colors)+
  guides(fill=guide_legend(nrow=4))+
  theme(legend.position=c(0.65,0.1),
        legend.text = element_text(size=6.5),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        legend.background = element_rect(fill = "transparent", color = NA))

ggsave("Figures/EIA_Mix.png", ggplot2::last_plot(),units="cm",
       dpi=600,width=18.5,height=9.7)


# EoF