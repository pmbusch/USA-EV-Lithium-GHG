# Load and filter sales

library(tidyverse)

sales <- readxl::read_excel("Inputs/2025_sales_data_updated_roadmapv2.6.xlsx")
sales <- sales %>% 
  filter(Country=="United States",
         Powertrain=="BEV",
         Vehicle=="Cars")
unique(sales$Scenario)
sales <- sales %>% 
  filter(Scenario %in% c("Baseline 2023","Momentum","Ambitious")) %>% 
  mutate(Scenario=str_remove(Scenario," 2023"))

sales <- sales %>% rename(Year=CY) %>% 
  dplyr::select(Scenario,Year,Sales) %>% 
  arrange(Scenario)

write.csv(sales,"Parameters/SalesEV.csv",row.names = F)

# EoF