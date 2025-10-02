# Time series of carbon footprint per mile

# PBH OCt 2025

source("Scripts/02-Load_Results.R")


## baseline scenario
df <- df_all %>% 
  filter(Scenario_Sales=="Ambitious",
         Scenario_Lifetime=="Reference",
         Scenario_Capacity=="Reference",
         Scenario_Recycling=="Recycling 45%",
         Scenario_Grid=="ref2025")

## VMT  ------
vmt <- read.csv("Results/total_VMT.csv")
vmt <- vmt %>% 
  mutate(Scenario_Lifetime=str_extract(Scenario,"Reference|Short|Long"),
         Scenario_Sales=str_extract(Scenario,"Momentum|Ambitious"),
         Scenario=NULL) %>% 
  filter(Scenario_Sales!="Baseline")
vmt_size <- vmt %>% 
  group_by(Scenario_Sales,Scenario_Lifetime,vehSize) %>% reframe(vmt=sum(total_vmt))

vmt <- vmt %>% group_by(Scenario_Sales,Scenario_Lifetime,Year) %>% reframe(vmt=sum(total_vmt)/1e6) %>% ungroup()

# 2023, 3.19 trillion miles in USA
# https://afdc.energy.gov/data/10315
vmt_total <- vmt %>% group_by(Scenario_Sales,Scenario_Lifetime) %>% reframe(vmt=sum(vmt)*1e6) # total miles 
vmt_total <- vmt_total %>% 
  filter(Scenario_Sales=="Ambitious",Scenario_Lifetime=="Reference")


# FIGURE ---------
head(df)
data_fig <- df %>% 
  filter(impact=="kgCO2eq") %>% 
  group_by(vehicle_type,Year) %>% 
  reframe(grams=sum(value)*1e3) %>% ungroup()

head(vmt)
vmt_fig <- vmt %>% 
  filter(Scenario_Sales=="Ambitious",Scenario_Lifetime=="Reference") %>% 
  dplyr::select(Year,vmt)

data_fig <- data_fig %>% 
  left_join(vmt_fig) %>% 
  mutate(metric=grams/(vmt*1.61*1e6))

p_zoom <- ggplot(data_fig,aes(Year,metric,col=vehicle_type))+
  geom_line(linewidth=0.5)+
  geom_text(data=filter(data_fig,Year==2049),aes(label=vehicle_type),
            size=6*5/14 * 0.8,nudge_y=40)+
  ylim(0,550)+
  coord_cartesian(expand = F)+
  labs(x="",y="",fill="",col="",title="grams CO2eq per km")+
  theme_bw(6.5)+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(hjust=1),
        legend.position = "none")
p_zoom

# same figure but with stacked chars
data_fig2 <- df %>% 
  filter(impact=="kgCO2eq") %>% 
  group_by(vehicle_type,Year,Stage) %>% 
  reframe(grams=sum(value)*1e3) %>% ungroup() %>% 
  left_join(vmt_fig) %>% 
  mutate(metric=grams/(vmt*1.61*1e6))

stage_colors <- viridis::viridis(6, option = "E",direction = -1)
names(stage_colors) <- stage_lvl

p <- ggplot(data_fig2,aes(Year,metric,fill=Stage))+
  geom_col(col="black",linewidth=0.1)+
  facet_wrap(~vehicle_type)+
  ylim(0,550)+
  coord_cartesian(expand = F)+
  scale_fill_manual(values = stage_colors)+
  labs(x="",y="",fill="",col="",title="grams CO2eq per km")+
  theme_bw(8)+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(hjust=1),
        panel.spacing.x= unit(0.5, "cm"),
        legend.position = "right")
p

library(cowplot)

ggdraw() +
  draw_plot(p) +
  draw_plot(p_zoom, x = 0.11, y = 0.6, width = 0.25, height = 0.25)


ggsave("Figures/Fig3.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)

# Lithium Figure ----------

# get diff EV minus ICE
data_fig <-  df %>% 
  filter(impact=="kgCO2eq") %>% 
  group_by(vehicle_type,Year) %>% 
  reframe(kg=sum(value)) %>% ungroup()

data_fig <- data_fig %>% 
  pivot_wider(names_from = vehicle_type, values_from = kg) %>% 
  mutate(delta=EV-ICE)

# kg lithium or per battetry
li <- df %>% 
  filter(impact %in% c("kgLithium")) %>% 
  group_by(impact,vehicle_type,Year) %>% 
  reframe(kg=sum(value)) %>% ungroup()

### add kWh requirements
kwh <- kwh_total %>% 
  filter(Scenario_Sales=="Ambitious",
         Scenario_Lifetime=="Reference",
         Scenario_Capacity=="Reference") %>% 
  group_by(Year,vehicle_type,Stage) %>% 
  reframe(kwh=sum(kwh)) %>% ungroup()
kwh_recyc <- kwh_recycling_total %>% 
  filter(Scenario_Sales=="Ambitious",
         Scenario_Lifetime=="Reference",
         Scenario_Recycling=="Recycling 45%",
         Scenario_Capacity=="Reference") %>% 
  group_by(Year,vehicle_type,Stage) %>% 
  reframe(kwh=sum(kwh)) %>% ungroup()
kwh <- rbind(kwh,kwh_recyc) %>% 
  group_by(Year,vehicle_type) %>% 
  reframe(kg=sum(kwh)) %>% ungroup() %>% 
  mutate(impact="kwh")


li <- li %>% 
  rbind(kwh) %>% 
  pivot_wider(names_from = vehicle_type, values_from = kg) %>% 
  mutate(ICE=if_else(is.na(ICE),0,ICE)) %>% # for kWh case
  mutate(delta_Li=EV-ICE) %>% dplyr::select(impact,Year,delta_Li)

data_fig <- data_fig %>% 
  left_join(li) %>% 
  mutate(impact=if_else(impact=="kgLithium","tons CO2e avoided per kg Lithium",
                        "tons CO2e avoided per kWh of battery"))
  mutate(metric=-delta/delta_Li/1e3) # tons avoided per kg of lithium

# over whole period
data_fig %>% 
  group_by(impact) %>% 
  reframe(x=sum(delta)/1e3,y=sum(delta_Li)) %>% 
  mutate(metric=-x/y) # 7.32 tons CO2 per kgLi, 470 kg CO2e per kWh



ggplot(data_fig,aes(Year,metric))+
  geom_line(linewidth=0.5)+
  facet_wrap(~impact,scales="free")+
  # ylim(0,16)+
  # coord_cartesian(expand = F)+
  labs(x="",y="",fill="",col="",title="tons CO2eq avoided per kg Lithium")+
  theme_bw(8)+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(hjust=1),
        legend.position = "none")

ggsave("Figures/Li_ts.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)



# EoF