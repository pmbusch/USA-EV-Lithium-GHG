# Grid emissions at county level, with forecast to 2050
# PBH March 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load inputs or parameters ---------
# county data
census_join <- read.csv("Parameters/census_joins.csv")
head(census_join)
# Note that HI and AK do not have a EIA region, but do have detail on ecoinvent region


# Electricity mixes
mix <- read.csv("Parameters/EIA_mix.csv")
mix_HI_AK <- read.csv("Parameters/HI_AK_mix.csv")

# emission factors
ghg <- read.csv("Parameters/ecoinvent_GHG_electricity.csv")
otherImpacts <- read.csv("Parameters/ecoinvent_OtherImpacts_electricity.csv")


# Electricity Mixes -----------
# Need to add HI and AK to EIA

# share mix of EIA (average emissions)
names(mix)
mix %>% group_by(period,regionName) %>% reframe(x=sum(share)) %>% arrange(desc(x))
mix <- mix %>% dplyr::select(scenario,period,regionName,fuel,share)

# HI and AK
names(mix_HI_AK)
mix_HI_AK <- mix_HI_AK %>% rename(regionName=SRNAME)
mix_HI_AK %>% group_by(period,regionName) %>% reframe(x=sum(share)) %>% arrange(desc(x))
mix_HI_AK <- mix_HI_AK %>% dplyr::select(period,regionName,fuel,share)

# same EIA scenario for all HI and AK
aux <- c()
for (i in unique(mix$scenario)){
  aux <- rbind(aux,mutate(mix_HI_AK,scenario=i))  
}
rm(i)
mix_HI_AK <- aux
mix <- rbind(mix,mix_HI_AK)

# census data ---

# correct issue with census - add HI and AK from ecoinvent region map
census_join <- census_join %>% 
  mutate(Region_Electricity_EIA=if_else(
    State %in% c("Hawaii","Alaska"),
    Region_ecoinvent_detail,
    Region_Electricity_EIA))

df <- census_join %>% 
  left_join(mix,by=c("Region_Electricity_EIA"="regionName"))
nrow(df)/1e6 # 1.3M
names(df)
df %>% filter(is.na(fuel)) # perfect join


# Ecoinvent (emissions) ----


# Match fuels
ghg <- ghg %>% filter(str_detect(Name,"electricity production|waste incineration")) %>% 
  mutate(Name=str_remove(Name,"electricity production, "))
unique(ghg$Name)
unique(df$fuel)
join_fuel <- read.csv("Inputs/join_fuels.csv")
ghg <- ghg %>% rename(ecoinvent_fuel=Name) %>% 
  left_join(join_fuel) %>% 
  rename(fuel=EIA_Fuel)

# filter fuels not used
ghg <- ghg %>% filter(!is.na(fuel))

# national avg
ghg_nat <- ghg %>% group_by(fuel) %>% reframe(kg_co2e=mean(kg_co2e)) %>% ungroup()

# Join to census
unique(census_join$Region_ecoinvent)
unique(ghg$Region)

df <- df %>% 
  left_join(ghg,by=c("Region_ecoinvent"="Region","fuel"))
names(df)
df %>% filter(is.na(kg_co2e)) %>% group_by(scenario,Region_ecoinvent,fuel) %>% tally()

# unmatch could be due to regional part, so add national averages
df$ecoinvent_fuel <- NULL
df_na <- df %>% filter(is.na(kg_co2e)) %>% dplyr::select(-sheet,-kg_co2e)
df_na <- df_na %>% left_join(ghg_nat,by="fuel") %>% 
  mutate(sheet="National")

# ALL assigned
df_na %>% filter(is.na(kg_co2e))

# add to main
df <- df %>% filter(!is.na(kg_co2e)) %>% rbind(df_na)
rm(df_na)

# CO2e by year and county --------
# Aggregate
head(df)
names(df)
electricity <- df %>% 
  filter(!is.na(kg_co2e)) %>% 
  mutate(kg_co2e=kg_co2e*share) %>% 
  group_by(scenario,State,STATEFP,COUNTYFP,NAME,period) %>% 
  reframe(pop=mean(pop),
          kg_co2e=sum(kg_co2e)) %>% ungroup()

# T&D losses - 8%, towards goal of 4% in 2030  
td_loss <- tibble(period=2022:2050) %>% 
  mutate(losses=case_when(
    period<2030 ~ 0.08-0.04/8*(period-2022),
    T ~ 0.04))

electricity <- electricity %>% 
  left_join(td_loss) %>% 
  mutate(kg_co2e=kg_co2e/(1-losses),
         losses=NULL)

write.csv(electricity,"Parameters/countyElectricityCarbon.csv",row.names = F)
# electricity <- read.csv("Parameters/countyElectricityCarbon.csv")


# at state
electricity_state <- electricity %>% 
  filter(scenario=="ref2025") %>% 
  group_by(scenario,State,period) %>% 
  reframe(kg_co2e=weighted.mean(kg_co2e,pop)) %>% ungroup()


state_highlight <- c("California","Texas","Florida","Michigan","Massachusetts",
                     "New York","Minnesota")

state_colors <- c("#FF5733","#8B4513","#FFA500","#4682B4","#800080",
                  "#00008B","#228B22","#808080")
names(state_colors) <- c(state_highlight,"Others")

electricity_state <- electricity_state %>% 
  mutate(state_color=if_else(State %in% state_highlight,State,"Others"))

# figure
ggplot(electricity_state,aes(period,kg_co2e,group=State,col=state_color))+
  geom_line(alpha=.7)+
  geom_text_repel(data=filter(electricity_state,period==2050),
                  size=5*5/14 * 0.8,alpha=.5,
                  max.overlaps = 30,
                  aes(label=State),nudge_x = 3)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_color_manual(values = state_colors) +
  coord_cartesian(expand = F,ylim = c(0,0.9))+
  labs(x="",y="",title="Average kg CO2e per kWh",fill="")+
  theme_bw(7)+ 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
    legend.position="none")

ggsave("Figures/Electricity/Electricity_CO2.png", ggplot2::last_plot(),units="cm",
       dpi=600,width=8.7*2.2,height=12)

## map figure ------

# shp_state <-  vect(paste0(url_file,"/Shapefiles/tl_2024_us_state/tl_2024_us_state.shp"))
# terra::plot(shp_state)
# as.data.frame(shp_state) %>% head()
# shp_state$NAME %>% unique()

data_fig <- electricity %>% filter(period %in% c(2024,2050)) %>% 
  filter(scenario=="ref2025") %>% 
  mutate(kg_co2e=kg_co2e*1000) %>%  # kg per MWh
  mutate(STATEFP=paste0(if_else(str_length(STATEFP)==1,"0",""),STATEFP)) %>% 
  mutate(COUNTYFP=paste0(case_when(
    str_length(COUNTYFP)==1 ~ "00",
    str_length(COUNTYFP)==2 ~ "0",
    T ~ ""),COUNTYFP))

# county map from tigris library
library(tigris) 
tigcounties <- counties()
tigcounties <- tigcounties[as.numeric(tigcounties$STATEFP) < 60, ]

# add emissions data
# unique(shp$STATEFP)
# unique(shp$COUNTYFP)
tigcounties <- merge(tigcounties, data_fig, 
                     by = c("STATEFP","COUNTYFP"), all.x = TRUE)  
tigcounties <- subset(tigcounties,!is.na(tigcounties$kg_co2e))

# put HI and AK at the bottom
# https://stackoverflow.com/questions/13757771/relocating-alaska-and-hawaii-on-thematic-map-of-the-usa-with-ggplot2
geo_shifted <- shift_geometry(tigcounties,position = "below",preserve_area = FALSE)

map <- ggplot(geo_shifted) +
  geom_sf(aes(fill = kg_co2e), color = "black",linewidth=0.05) +
  facet_wrap(~period)+
  scale_fill_gradientn(colors = c("tan", "darkred"))+
  # coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +  # Continental US bounds
  labs(fill=expression("Grid Intensity [kg CO"["2"]*"e per MWh]"))+
  theme_void()+
  theme(legend.position = "none",
        plot.margin = margin(0, 0, 0, 0))+
  guides(fill = guide_colorbar(barwidth = 50))  
map

# histogram to show legend
hist <- ggplot(geo_shifted,aes(kg_co2e,fill=after_stat(x)))+
  geom_histogram(col="white",binwidth = 50,boundary=0)+
  facet_wrap(~period,ncol=2,scales="free_x")+
  scale_x_continuous(limits = c(0,1.05e3))+
  scale_fill_gradientn(colors = c("tan", "darkred"))+
  coord_cartesian(expand = F)+
  labs(x=expression("Grid Intensity [kg CO"["2"]*"e per MWh]"),y="")+
  theme_bw(10)+
  theme(panel.grid = element_blank(),
        legend.position = "none",
        # strip.text = element_text(face = "bold"),
        strip.text = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "black"),
        strip.background = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        panel.spacing = unit(3, "cm"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
hist

library(cowplot)
# combine them
ggdraw() +
  draw_plot(hist, 0, 0.1, 0.9, 0.2)+ 
  draw_plot(map, 0, 0.05, 1, 0.95)

ggsave("Figures/Electricity/Electricity_CO2_map.png", ggplot2::last_plot(),units="cm",
       dpi=600,width=8.7*2.2,height=12)

# For all scenarios - TAKES A LOT OF TIME
scens <- read.csv("Inputs/Join_EIAScenarios.csv")
for (i in unique(df$scenario)){
  
  aux_co2 <-electricity %>% filter(period %in% c(2024,2050)) %>% 
    filter(scenario==i) %>% 
    mutate(kg_co2e=kg_co2e*1000) %>% 
    mutate(STATEFP=paste0(if_else(str_length(STATEFP)==1,"0",""),STATEFP)) %>% 
    mutate(COUNTYFP=paste0(case_when(
      str_length(COUNTYFP)==1 ~ "00",
      str_length(COUNTYFP)==2 ~ "0",
      T ~ ""),COUNTYFP))
  
  new_data <- geo_shifted %>% mutate(kg_co2e=NULL) %>% 
    left_join(aux_co2,by=c("State","COUNTYFP","period"))
  
  aux <- scens %>% filter(scenario==i) %>% pull(scenarioDescription)
  
  p3 <- map+ggtitle(aux)
  p3$data <- new_data
  p_hist <- hist
  p_hist$data <- new_data
  
  ggdraw() +
    draw_plot(p_hist, 0, 0.1, 0.9, 0.2)+ 
    draw_plot(p3, 0, 0.05, 1, 0.95)
  
  ggsave(paste0("Figures/Electricity/EIA_Scenarios/",i,".png"), 
                ggplot2::last_plot(),units="cm",
         dpi=600,width=8.7*2.2,height=12)

}
rm(i)


# Figure of MIX by time -----

head(mix)
# colors
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")
cat_colors <- names(fuel_colors)
mix <- mix %>% mutate(fuel=factor(fuel,levels=cat_colors))

mix %>% 
  filter(scenario=="ref2025") %>% 
  group_by(regionName,period) %>% 
  filter(share>0) %>% 
  reframe(x=sum(share)) %>% arrange(desc(x))

mix %>% 
  filter(regionName!="United States") %>% 
  filter(scenario=="ref2025") %>% 
  mutate(regionName=regionName %>% 
           str_replace("Northeast Power Coordinating Council","NPCC") %>% 
           str_replace("Western Electricity Coordinating Council","WECC") %>% 
           str_replace(" Reliability Corporation","") %>% 
           str_replace("Southwest Power Pool","SPP") %>% 
           str_replace("Reliability Coordinating Council","RCC") %>% 
           str_replace("Power Pool Area","") %>% 
           str_replace("and Long","& Long")) %>% 
  complete(regionName, period, fuel, fill = list(share = 0)) %>% 
  ggplot(aes(period,share))+
  # geom_area(aes(fill=fuel,group=fuel))+ # weird spikes for some reasons
  geom_col(aes(fill=fuel,group=fuel),width=1)+
  facet_wrap(~regionName)+
  coord_cartesian(expand = F)+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(breaks = c(2030, 2040, 2050))+
  scale_fill_manual(values=fuel_colors)+
  guides(fill=guide_legend(nrow=2))+
  labs(x="",y="",title="Share of Electricity Generation [%]",fill="")+
  theme_bw(7)+ 
  theme(panel.grid = element_blank(),
        strip.text = element_text(size=5),
        legend.position="bottom",
        legend.text = element_text(size=6),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        legend.background = element_rect(fill = "transparent", color = NA))

ggsave("Figures/Electricity/Mix.png", ggplot2::last_plot(),units="cm",
       dpi=600,width=18.5,height=9.7)

# Fossil Fuel per MWh -----------
fossil <- read.csv("Parameters/ecoinvent_fossil_electricity.csv")
fossil <- fossil %>% filter(str_detect(Name,"electricity production|waste incineration")) %>% 
  mutate(Name=str_remove(Name,"electricity production, "))
unique(fossil$Name)

df2 <- census_join %>% 
  left_join(mix,by=c("Region_Electricity_EIA"="regionName"))
unique(df2$fuel)
join_fuel <- read.csv("Inputs/join_fuels.csv")
fossil <- fossil %>% rename(ecoinvent_fuel=Name) %>% 
  left_join(join_fuel) %>% 
  rename(fuel=EIA_Fuel)

# filter fuels not used
fossil <- fossil %>% filter(!is.na(fuel))
unique(fossil$fuel)

# national avg
fossil_nat <- fossil %>% group_by(fuel,fossilFuel,unit) %>% 
  reframe(value=mean(value)) %>% ungroup()

# Join to census
unique(census_join$Region_ecoinvent)
unique(fossil$Region)

df2 <- df2 %>% 
  left_join(fossil,by=c("Region_ecoinvent"="Region","fuel"))
names(df2)
df2 %>% filter(is.na(value)) %>% group_by(scenario,Region_ecoinvent,fuel) %>% tally()

# unmatch could be due to regional part, so add national averages
df2$ecoinvent_fuel <- NULL
df_na <- df2 %>% filter(is.na(value)) %>% dplyr::select(-sheet,-value,-fossilFuel,-unit)
df_na <- df_na %>% left_join(fossil_nat) %>% 
  mutate(sheet="National")
unique(df_na$fuel)

# ALL assigned
df_na %>% filter(is.na(value))

# add to main
df2 <- df2 %>% filter(!is.na(value)) %>% rbind(df_na)
rm(df_na)

#  by year and county 
# Aggregate
head(df2)
names(df2)
electricity2 <- df2 %>% 
  filter(!is.na(value)) %>% 
  mutate(value=value*share) %>% 
  group_by(scenario,State,STATEFP,COUNTYFP,NAME,period,fossilFuel,unit) %>% 
  reframe(pop=mean(pop),
          value=sum(value)) %>% ungroup()

# T&D losses - 8%, towards goal of 4% in 2030  
td_loss <- tibble(period=2022:2050) %>% 
  mutate(losses=case_when(
    period<2030 ~ 0.08-0.04/8*(period-2022),
    T ~ 0.04))

electricity2 <- electricity2 %>% 
  left_join(td_loss) %>% 
  mutate(value=value/(1-losses),
         losses=NULL)

write.csv(electricity2,"Parameters/countyElectricityFossilFuel.csv",row.names = F)
# electricity2 <- read.csv("Parameters/countyElectricityCarbon.csv")


# at state
electricity_state <- electricity2 %>%
  filter(scenario=="ref2025") %>% 
  mutate(fossilFuel=paste0(fossilFuel," (",unit,")")) %>% 
  group_by(State,period,fossilFuel) %>% 
  reframe(value=weighted.mean(value,pop)) %>% ungroup()


state_highlight <- c("California","Texas","Florida","Michigan","Massachusetts",
                     "New York","Minnesota")

state_colors <- c("#FF5733","#8B4513","#FFA500","#4682B4","#800080",
                  "#00008B","#228B22","#808080")
names(state_colors) <- c(state_highlight,"Others")

electricity_state <- electricity_state %>% 
  mutate(state_color=if_else(State %in% state_highlight,State,"Others"))

# figure
ggplot(electricity_state,aes(period,value,group=State,col=state_color))+
  geom_line(alpha=.7)+
  geom_text_repel(data=filter(electricity_state,period==2050),
                  size=5*5/14 * 0.8,alpha=.5,
                  max.overlaps = 30,
                  aes(label=State),nudge_x = 3)+
  facet_wrap(~fossilFuel,scales="free_y")+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_color_manual(values = state_colors) +
  # coord_cartesian(expand = F,ylim = c(0,0.9))+
  labs(x="",y="",title="Average Fossil Fuel Resource consumption per kWh",fill="")+
  theme_bw(7)+ 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        legend.position="none")

ggsave("Figures/Electricity/Electricity_fossil.png", ggplot2::last_plot(),units="cm",
       dpi=600,width=8.7*2.2,height=12)

# EoF