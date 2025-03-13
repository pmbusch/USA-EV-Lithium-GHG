# Dissagregate EVs based on 2023 registrations and population
# PBH March 2025
# https://afdc.energy.gov/data/10962

library(tidyverse)

# only BEVs included, for 2023
ev <- readxl::read_excel("Inputs/10962-ev-registration-counts-by-state_9-06-24.xlsx",
                         sheet = "EV Registration Counts in 2023",
                         range = "B3:C54") %>% 
  rename(regs=`Registration Count`)
sum(ev$regs)/1e6 # 3.55M

# get joins for region transport from EIA data
dict_reg <- read.csv("Inputs/Join_TransportEIA_State.csv")
ev <- ev %>% left_join(dict_reg,by=c("State"="NAME"))

# get population by state, from census data (need to generate census file beforehand)
census <- read.csv("Parameters/census_joins.csv")
census <- census %>% group_by(State,Region_Transport) %>% 
  reframe(pop=sum(pop))
sum(census$pop)/1e6 # 334M

# add pop
ev <- ev %>% left_join(census)
sum(ev$pop)/1e6

# calculate share of EV regs and share of pop by state within regions
ev_share <- ev %>% group_by(Region_Transport) %>% 
  mutate(pop=pop/sum(pop),
         ev=regs/sum(regs)) %>% ungroup() %>% 
  mutate(regs=NULL)
  
# State code
st_code <- read.csv("Inputs/Join_StateCode.csv")

# Barplot
ev_share %>% 
  left_join(st_code) %>% 
  rename(Population=pop,`EV Registration`=ev) %>% 
  pivot_longer(c(Population,`EV Registration`), 
               names_to = "key", values_to = "value") %>% 
  mutate(State_code=if_else(key=="Population",State_code,"")) %>% 
  ggplot(aes(key,value,fill=State))+
  geom_col(col="black",linewidth=0.05)+
  geom_text(aes(label=State_code),position = position_stack(vjust = .5),
            size=6*5/14 * 0.8)+
  coord_flip(expand = F)+
  ggforce::facet_col(facets = vars(Region_Transport),
                     scales = "free_y",
                     space = "free")+
  scale_y_continuous(labels=scales::percent)+
  scale_fill_viridis_d()+
  labs(x="",y="State share within EIA Transport Region")+
  theme_bw(8)+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        strip.text = element_text(size=6))

ggsave("Figures/EV_reg_share.png", ggplot2::last_plot(),
       units="cm",dpi=600,width=8.7*2,height=11)

# Save share
write.csv(ev_share,"Parameters/EV_share_state.csv",row.names = F)

