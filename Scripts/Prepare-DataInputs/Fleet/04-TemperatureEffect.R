# Temperature effect on driving efficiency
# Low and high temperature affect efficiency, especially for EVs
# Data on SI of Woody et al: Woody, M., Vaishnav, P., Keoleian, G. A., De Kleine, R., Kim, H. C., Anderson, J. E., & Wallington, T. J. (2022). The role of pickup truck electrification in the decarbonization of light-duty vehicles. Environmental Research Letters, 17(3), 034031.


library(tidyverse)

# Empirical data on efficiency/fuel consumption and temperature: https://pubs.acs.org/doi/10.1021/acs.est.9b00648

# Study in China that used this equations: https://pubs.acs.org/doi/10.1021/acs.est.0c08217
# Slope factors for piecewise linear
alpha <- data.frame(Powertrain = c("ICEV","HEV","PHEV","BEV"),
                alpha_low = c(0.0129,0.0171,0.0183,0.0210), # low temp
                alpha_high = c(0.0064,0.0123,0.0154,0.0242))
  
  
# Load Temperature adjustments USA from: https://iopscience.iop.org/article/10.1088/1748-9326/ac5142/meta
tempAdj <- read.csv("Inputs/Temperature_Adjustments.csv")

# Pro: already calculated by county level using Temp in USA
# Cons: do not have PHEVs

# Key idea: Use 3 results to estimate delta T1 and delta 2
# Result is not exact, but close
library(stats)
solve_eq <- function(R1,R2,R3,
                     a_low1,a_low2,a_low3,
                     a_high1,a_high2,a_high3, 
                     T1_start = 25, T2_start = 12) {
  # get as close as obtained value - Min Sum of Squares
  eq_func <- function(x) { 
    (a_low1*(x[1]-23.9)+a_high1*(15.5-x[2])-R1)^2+
      (a_low2*(x[1]-23.9)+a_high2*(15.5-x[2])-R2)^2+
      (a_low3*(x[1]-23.9)+a_high3*(15.5-x[2])-R3)^2}
  res <- optim(c(T1_start, T2_start), eq_func)
  list(T1 = res$par[1], T2 = res$par[2], objective = res$value)
}

# Example usage: 
solve_eq(0.07222506776181,0.11499898398357,0.18712323449692,
         alpha[1,2],alpha[2,2],alpha[4,2],
         alpha[1,3],alpha[2,3],alpha[4,3])
# solves nicely

# rowwise for every county
tempAdj <-   tempAdj %>%
  rowwise() %>%
  mutate(res = list(solve_eq(ICEVs-1,HEVs-1, BEVs-1,
                             alpha[1,2],alpha[2,2],alpha[4,2],
                             alpha[1,3],alpha[2,3],alpha[4,3],
                             25,12))) %>%
  # mutate(T1 = res$par[1], T2 = res$par[2], objective = res$value) %>%
  # select(-res) %>%
  unnest_wider(res) %>% 
  ungroup()

range(tempAdj$objective) # close to zero
range(tempAdj$T1) # >23.9
range(tempAdj$T2) # <15.5

# With T1 and T2 estimates we can calculate Temp adjustment for PHEV
tempAdj <- tempAdj %>% 
  mutate(PHEVs=1+alpha[3,2]*(T1-23.9)+alpha[3,3]*(15.5-T2))

# Load census data to do weighted average
census <- read.csv("Parameters/census_joins.csv") %>% 
  # 0 before code
  mutate(state0=if_else(str_length(STATEFP)==1,"0","")) %>% 
  mutate(county0=case_when(
    str_length(COUNTYFP)==1 ~ "00",
    str_length(COUNTYFP)==2 ~ "0",
    T ~ "")) %>% 
  mutate(FIPS=as.character(paste0(state0,STATEFP,county0,COUNTYFP)))

# Connecticut is weird, manual join by looking at the map
# "Capitol" ~ "Hartford County" 
# "Greater Bridgeport" ~ Fairfiled County
# "Lower Connecticut River Valley" ~ Middlesex County
# "Naugatuck Valley" ~ New Haven County
# "Northeastern Connecticut" ~ Windham County
# "Northwest Hills" ~ Litchfield County
# "South Central Connecticut" ~ New Haven County
# "Southeastern Connecticut" ~ New London County
# "Western Connecticut" ~ Fairfield County
# add correct FIPS
census <- census %>% 
  mutate(FIPS=case_when(
    NAME=="Capitol" ~ "09003" , 
    NAME=="Greater Bridgeport" ~ "09001",
    NAME=="Lower Connecticut River Valley" ~ "09007",
    NAME=="Naugatuck Valley" ~ "09009",
    NAME=="Northeastern Connecticut" ~ "09015",
    NAME=="Northwest Hills" ~ "09005",
    NAME=="South Central Connecticut" ~ "09009",
    NAME=="Southeastern Connecticut" ~ "09011",
    NAME=="Western Connecticut" ~ "09001",
    T ~ FIPS))

# Some NA still
df <- tempAdj %>% 
  mutate(County=str_remove_all(County,"'")) %>% 
  mutate(FIPS=paste0(if_else(str_length(FIPS)==4,"0",""),FIPS)) %>% 
  left_join(census,by="FIPS")

df %>% filter(is.na(State)) # just Tolland
df <- df %>% filter(!is.na(State))

write.csv(df,"Parameters/Operation/TempAdjFactors.csv",row.names = F)  

# weighted meand by state
df_state <- df %>% group_by(State) %>% 
  reframe(ICEVs=weighted.mean(ICEVs,pop),
          HEVs=weighted.mean(HEVs,pop),
          PHEVs=weighted.mean(PHEVs,pop),
          BEVs=weighted.mean(BEVs,pop))
  
write.csv(df_state,"Parameters/Operation/TempAdjFactorsState.csv",row.names = F)  

# EoF