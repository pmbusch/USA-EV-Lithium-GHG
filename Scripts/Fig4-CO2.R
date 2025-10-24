# Time series of total carbon emissions
# PBH OCt 2025

source("Scripts/02-Load_Results.R")

## baseline scenario
df <- df_all %>% 
  filter(Scenario_Sales=="Ambitious",
         Scenario_Lifetime=="Reference",
         Scenario_Capacity=="Reference",
         Scenario_Recycling=="Recycling 45%",
         Scenario_Grid=="ref2025")

# FIGURE ---------
head(df)
data_fig_zoom <- df %>% 
  filter(impact=="kgCO2eq") %>% 
  group_by(vehicle_type,Year) %>% 
  reframe(metric=sum(value)/1e3/1e6) %>% ungroup()

p_zoom <- ggplot(data_fig_zoom,aes(Year,metric,col=vehicle_type))+
  geom_line(linewidth=0.5)+
  geom_text(data=filter(data_fig_zoom,Year==2049),aes(label=vehicle_type),
            size=6*5/14 * 0.8,nudge_y=40)+
  ylim(0,1750)+
  coord_cartesian(expand = F)+
  labs(x="",y="",fill="",col="",title="")+
  theme_bw(6.5)+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(hjust=1),
        legend.position = "none")
p_zoom

# same figure but with stacked chars
data_fig <- df %>% 
  filter(impact=="kgCO2eq") %>% 
  group_by(vehicle_type,Year,Stage) %>% 
  reframe(metric=sum(value)/1e3/1e6) %>% ungroup()

stage_colors <- viridis::viridis(6, option = "E",direction = -1)
names(stage_colors) <- stage_lvl

p <- ggplot(data_fig,aes(Year,metric,fill=Stage))+
  geom_col(col="black",linewidth=0.1)+
  facet_wrap(~vehicle_type)+
  ylim(0,1750)+
  coord_cartesian(expand = F)+
  scale_fill_manual(values = stage_colors)+
  labs(x="",y="",fill="",col="",title="million tons CO2eq")+
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

# I like alone version more
p
  
ggsave("Figures/Fig3.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)


# Discounting --------------
vmt <- read.csv("Results/total_VMT.csv")
vmt <- vmt %>% 
  mutate(Scenario_Lifetime=str_extract(Scenario,"Reference|Short|Long"),
         Scenario_Sales=str_extract(Scenario,"Momentum|Ambitious"),
         Scenario=NULL) %>% 
  filter(Scenario_Sales!="Baseline")
vmt <- vmt %>% group_by(Scenario_Sales,Scenario_Lifetime,Year) %>% reframe(vmt=sum(total_vmt)) %>% ungroup()
vmt_total <- vmt %>% group_by(Scenario_Sales,Scenario_Lifetime) %>% reframe(vmt=sum(vmt))
vmt_total <- vmt_total %>% 
  filter(Scenario_Sales=="Ambitious",Scenario_Lifetime=="Reference")


unique(df$impact)

gwp <- tibble(impact=c("kg_CO2","kg_CH4","kg_N2O","kg_PFC116","kg_PFC14","kg_SF6"),
              gwp=c(1,29.8,273,12400,7380,24300))

data_fig2 <- df %>% 
  filter(impact %in% c("kg_CO2","kg_CH4","kg_N2O","kg_PFC116","kg_PFC14","kg_SF6")) %>% 
  group_by(impact,vehicle_type,Year,Stage) %>% 
  reframe(metric=sum(value)/1e3/1e6) %>% ungroup() # million tons

# compare to CO2eq - Perfect fit
data_fig2 %>% 
  left_join(gwp) %>% 
  mutate(metric=metric*gwp) %>% 
  group_by(vehicle_type,Year) %>% 
  reframe(gwp_calc=sum(metric)) %>% 
  left_join(data_fig_zoom) %>% 
  pivot_longer(c(gwp_calc,metric), names_to = "key", values_to = "value") %>% 
  ggplot(aes(Year,value,col=key))+
  geom_line()+
  facet_wrap(~vehicle_type)


# TAWP - discount based using AR6 and Kendall 2012 (https://link.springer.com/article/10.1007/s11367-012-0436-5)
tawp <- read_excel("Inputs/TAWP_AR6.xlsx",sheet="GWP_discount_100y")
tawp <- tawp %>% 
  dplyr::select(-year) %>% rename(Year=period) %>% 
  pivot_longer(c(-Year), names_to = "impact", values_to = "gwp") %>% 
  mutate(impact=paste0("kg_",impact))
  

data_fig2 <- data_fig2 %>% 
  left_join(tawp) %>% 
  mutate(metric=metric*gwp)

# majority is CO2
data_fig2 %>% 
  group_by(vehicle_type,impact,Year) %>% 
  reframe(metric=sum(metric)) %>% ungroup() %>% 
  ggplot(aes(Year,metric,fill=impact))+
  geom_col()+
  facet_wrap(~vehicle_type)

# by stage
data_fig2 %>% 
  group_by(vehicle_type,Stage,Year) %>% 
  reframe(metric=sum(metric)) %>% ungroup() %>% 
  ggplot(aes(Year,metric,fill=Stage))+
  geom_col(col="black",linewidth=0.1)+
  facet_wrap(~vehicle_type)+
  # ylim(0,1750)+
  coord_cartesian(expand = F)+
  scale_fill_manual(values = stage_colors)+
  labs(x="",y="",fill="",col="",title="million tons CO2eq - Equivalent to 2025")+
  theme_bw(8)+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(hjust=1),
        panel.spacing.x= unit(0.5, "cm"),
        legend.position = "right")


# Bar plot comparison
df1 <- data_fig %>%   
  group_by(vehicle_type,Stage) %>% 
  reframe(metric=sum(metric)) %>% ungroup() %>% 
  mutate(vehicle_type=factor(vehicle_type)) %>% 
  mutate(key="Standard")

df2 <- data_fig2 %>%   
  group_by(vehicle_type,Stage) %>% 
  reframe(metric=sum(metric)) %>% ungroup() %>% 
  mutate(vehicle_type=factor(vehicle_type)) %>% 
  mutate(key="Discounting")

# quantify difference for EV
df1 %>% group_by(vehicle_type) %>% reframe(metric=sum(metric)) %>% 
  pivot_wider(names_from = vehicle_type, values_from = metric) %>% 
  mutate(red=(ICE-EV)/ICE,delta=ICE-EV,
         red_label=paste0(round(red*100,0),"%"))

df2 %>% group_by(vehicle_type) %>% reframe(metric=sum(metric)) %>% 
  pivot_wider(names_from = vehicle_type, values_from = metric) %>% 
  mutate(red=(ICE-EV)/ICE,delta=ICE-EV,
         red_label=paste0(round(red*100,0),"%"))

# difference in delta emissions
(15514-13263)/15514 # 14% reduction in total emissions reduction

# difference for EV (GWP vs TAWP)
(12912-11347)/12912
# For ICE (GWP vs TAW[])
(26627-23167)/26627

ggplot(df1,aes(as.numeric(vehicle_type)+0.2,metric,fill=Stage))+
  geom_col(col="black",linewidth=0.1,width=0.4)+
  geom_col(data=df2,aes(x=as.numeric(vehicle_type)-0.2),col="black",linewidth=0.1,width=0.4,alpha=.5)+
  annotate(geom = "text",x = 1.2,y=16e3,label="GWP-100  (standard)",size=9*5/14 * 0.8,fontface="italic")+
  annotate(geom = "text",x = 0.8,y=15e3,label="Discounted GWP-100, -12%",size=9*5/14 * 0.8,fontface="italic")+
    annotate(geom = "text",x = 1.8,y=24e3,label="-13%",size=9*5/14 * 0.8,fontface="italic")+
  scale_fill_manual(values = stage_colors)+
  scale_x_continuous(breaks=1:2,labels=c("EV","ICEV"))+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE),
                     expand = expansion(mult = c(0, 0.05)),
                     sec.axis = sec_axis(~ . * 1e6*1e6 / vmt_total$vmt*1.61, name = "grams CO2e per km (avg.)"))+
  coord_flip()+
  labs(x="",fill="",col="",y="million tons CO2eq")+
  theme_bw(8)+
  guides(fill= guide_legend(reverse = TRUE,nrow = 1))+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(hjust=1),
        panel.spacing.x= unit(0.5, "cm"),
        legend.position = "bottom")

ggsave("Figures/Fig3_discount.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=8.7)



# old, not relevant
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