# GHG Calculation
# EV Fleet Model
# PBH Nov 2024

# Results are from previous stock model
# https://github.com/pmbusch/Lithium-Supply/blob/main/Scripts/Demand%20Model/Prepare_Data/09-SurvivalCurve_Simulation.R

# Need to load, filter and adjust the data to the desired format

# Load Data
source("Scripts/00-Libraries.R", encoding = "UTF-8")

## Reuse statistics from Survival Model -----
reuse <- read.csv("Inputs/region_outflows_LIB.csv",
                  stringsAsFactors = FALSE)
# Convert strings to vectors by year - Process the list column back into a list
reuse <- reuse %>%
  mutate(LIB_recycling_vector = str_split(LIB_recycling_vector,"\\|") %>% lapply(as.numeric),
         LIB_Available_vector = str_split(LIB_Available_vector,"\\|") %>% lapply(as.numeric),
         add_LIB_vector = str_split(add_LIB_vector,"\\|") %>% lapply(as.numeric),
         EV_Stock_vector = str_split(EV_Stock_vector,"\\|") %>% lapply(as.numeric))
names(reuse)

# Only for USA and BEV Cars
reuse <- reuse %>% 
  filter(Region=="United States") %>% 
  filter(Vehicle=="Car") %>% 
  filter(Powertrain=="BEV") %>%
  filter(scen_lifetime=="Baseline") %>% 
  filter(Year<2051)

# sales are direct
sales <- reuse %>% dplyr::select(Year,Sales)
write.csv(sales,"Parameters/salesEV.csv",row.names = F)

  
# Fleet at 2050 ------------
(x <- reuse %>% filter(Year==2050) %>% 
    pull(EV_Stock)/1e6)
x/335 # car ownership 0.70


# Get EV stock by calendar year, model year and vehicle age, for US
fleet <- reuse %>%
  mutate(age = map(EV_Stock_vector, seq_along),
         fleet = map(EV_Stock_vector, as.numeric)) %>%
  unnest_longer(c(EV_Stock_vector, age, fleet)) %>%
  mutate(age=age-1) %>% 
  dplyr::select(Year,age,fleet) %>% 
  filter(fleet>0)

# check totals
range(fleet$age)
fleet %>% group_by(Year) %>% reframe(fleet=sum(fleet)/1e6)
reuse$EV_Stock/1e6

# Save
write.csv(fleet,"Parameters/USA_fleet.csv",row.names = F)

# Additional Batteries ---------
addLIB <- reuse %>%
  mutate(age = map(add_LIB_vector, seq_along),
         LIB = map(add_LIB_vector, as.numeric)) %>%
  unnest_longer(c(add_LIB_vector, age, LIB)) %>%
  mutate(age=age-1) %>% 
  mutate(modelYear=Year-age) %>% 
  dplyr::select(Year,age,modelYear,LIB) %>% 
  filter(LIB>0)
range(addLIB$age)
range(addLIB$modelYear)
write.csv(addLIB,"Parameters/LIB_replacement.csv",row.names = F)

# Recyling may need to come from full model results, to account for SSPS


# Figures ------
names(reuse)
url_fig <- "Figures/Fleet/%s.png"

# Sales
sales %>% 
  mutate(Sales=Sales/1e6) %>% 
  ggplot(aes(Year,Sales))+
  geom_line()+
  coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  labs(x="",y="",title="USA BEV Sales [million units]")+
  theme(legend.position = "none",
        axis.text.x = element_text(hjust = 1))

ggsave(sprintf(url_fig,"sales"),dpi=600,units = "cm",
       width = 12,height=8.7)

# Fleet
fleet %>% 
  mutate(fleet=fleet/1e6) %>% 
  ggplot(aes(Year,fleet,fill=age))+
  geom_col(col="black",width = 1,linewidth=0.05)+
  coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_fill_gradientn(colors = c("#006837", "#66BD63",  # Green (1-10)
                                  "#1C9099", "#67A9CF",  # Blue (11-20)
                                  "#D73027", "#A50026"), # Red (21-30)
                       values = scales::rescale(c(0, 10, 11, 20, 21, 30)),
                       breaks = c(0,10,20,30),name = "Vehicle\nAge")+
  labs(x="",y="",title="USA BEV Fleet [million units]")

ggsave(sprintf(url_fig,"fleet"),dpi=600,units = "cm",
       width = 12,height=8.7)



addLIB %>% 
  mutate(LIB=LIB/1e6) %>% 
  ggplot(aes(Year,LIB,fill=age))+
  geom_col(col="black",width = 1,linewidth=0.05)+
  coord_cartesian(expand = F)+
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050))+
  scale_fill_gradientn(colors = c("#006837", "#66BD63",  # Green (1-5)
                                  "#1C9099", "#67A9CF",  # Blue (6-10)
                                  "#E69F00", "#FDD835",  # Orange (11-15)
                                  "#D73027", "#A50026"), # Red (16-19)
                       values = scales::rescale(c(0, 5, 6, 10, 11, 15, 16, 19)),
                       breaks = c(0,5,10,15,19),name = "Vehicle\nAge")+
  labs(x="",y="",title="USA LIB Replacement [million units]")

ggsave(sprintf(url_fig,"addLIB"),dpi=600,units = "cm",
       width = 12,height=8.7)

# EoF