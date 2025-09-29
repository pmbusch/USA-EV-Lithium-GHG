# Based on lithium supply project
# Original script: https://github.com/pmbusch/Lithium-Supply/blob/main/Scripts/Demand%20Model/Prepare_Data/08-SurvivalCurve_Simulation.R 
# PBH May 2025

library(tidyverse)


# Function to get flows (numbers of cars,EV,LIB) depending on the 
# vehicle and battery starting age
# Discretized by year using Normal Distribution
# n vehicles: vehicles currently on stock, 
f.getOutflows <- function(n_veh=1,EV_age,LIB_age, maxEV_age=30, maxLIB_age=30,
                          dist.Age="Logistic"){
  
  # get fraction year to year of survival, based on CDF ratios
  # represent proportion that survives year to year
  
  if(dist.Age=="Normal"){
    y1 = (1-pnorm(EV_age+1, mean = mean_ev, sd = sd_ev))/
      (1-pnorm(EV_age, mean = mean_ev, sd = sd_ev))
    y2 = (1-pnorm(LIB_age+1, mean = mean_lib, sd = sd_lib))/
      (1-pnorm(LIB_age, mean = mean_lib, sd = sd_lib))
  } else{ # Logistic
    y1 = (1-plogis(EV_age+1, mean_ev, sd_ev*sqrt(3)/pi))/ # CONVERT SCALE TO Stand Dev.
      (1-plogis(EV_age, mean_ev, sd_ev*sqrt(3)/pi))
    y2 = (1-plogis(LIB_age+1, mean_lib, sd_lib*sqrt(3)/pi))/
      (1-plogis(LIB_age, mean_lib, sd_lib*sqrt(3)/pi))
  }
  
  # max age
  if(EV_age>=maxEV_age) {y1 = 0}
  if(LIB_age>=maxLIB_age) {y2 = 0}
  
  # independent events to get proportions into 4 cases
  ret <- tibble(
    both_fail=(1-y1)*(1-y2)*n_veh,
    ev_fail=(1-y1)*y2*n_veh,
    lib_fail=y1*(1-y2)*n_veh,
    none=y1*y2*n_veh)
  
  return(ret)
}


# parameters
# Other parameters
ev_age_newLib <- 8 # year were a new battery is needed, after that an old battery will be sufficient
# 8 years assuming a warranty over this period
max_reuse_lib <- 0.25
# Max age when an EV gets a battery, either 2-hand or new
max_ev_age <- 12
# Max age of LIB to be used in an EV
max_lib_age_ev <- 12

# life time parameters
mean_ev=17;sd_ev=4;mean_lib=15;sd_lib=4


# ICCT sales for USA
sales <- read.csv("Parameters/Operation/SalesEV.csv")


# Historical EV sales for stock
# According to EV Volumes, almost no historical sales of Vans, so no stock prior 
historical_ev <- read.csv("Parameters/historical_evSales.csv")
historical_ev$Vehicle="Cars"
historical_ev <- rbind(historical_ev,
                       tibble(period=2016:2023,Vehicle="Vans",sales=0))

# add historical sales
historical_ev <- historical_ev %>% rename(Year=period,Sales=sales) 
aux <- c()
for (i in unique(sales$Scenario)){
  historical_ev$Scenario=i
  aux <- rbind(aux,historical_ev)
}
historical_ev <- aux
rm(i,aux)
sales <- sales %>% filter(Year>2023) %>% 
  rbind(historical_ev) %>% 
  arrange(Year) %>% arrange(Scenario)

## Loop ------
scenarios <- unique(sales$Scenario)
sales_orig <- sales
sales_new <- c()

lifetime_scen <- c("Reference","Short","Long")
mean_lib.orig=mean_lib


scen=scenarios[1] # debud
for (scen in scenarios){
  cat("Scenario ",scen,"\n")
  
  for (veh in unique(sales_orig$Vehicle)){
    
    # Filters
    sales <- sales_orig %>% 
      filter(Scenario==scen) %>% 
      filter(Vehicle==veh)
    
    for(lif in lifetime_scen){
      
      # LIB lifetime
      if(lif=="Short"){
        mean_lib= 10 #  years
      }
      else if(lif=="Long"){
        mean_lib= 20 # years
      } else {
        mean_lib= mean_lib.orig # back to original
      }
      
      sales$lifetime <- lif
      
      
      
      start_year <- 2016
      
      ## Loop by years 
      # Matrix update idea
      # Key: Update matrix of vehicle age and battery age stock accordingly
      matrix_data <- matrix(0, nrow = 31, ncol = 31)
      rownames(matrix_data) <-paste0("EV_",0:30) # ROWS are EV
      colnames(matrix_data) <- paste0("LIB_",0:30) # COLS are Battery
      
      # Loop through years
      sales$Year %>% range()
      sales$add_LIB <-sales$LIB_Available <- sales$LIB_recycling <- sales$LIB_reuse_EV <- sales$EV_Stock <- 0
      sales$add_LIB_vector <-sales$LIB_Available_vector <- sales$LIB_recycling_vector <- sales$EV_Stock_vector <- c()
      
      for (y in start_year:2050){
        
        # if (y==2043){break} # debug
        
        # Assign new sales to top left cuadrant (0,0)
        matrix_data[1, 1] <- sales$Sales[y-start_year+1]
        
        # clear stock of 10 or less batteries or EVs
        matrix_data[matrix_data < 10] <- 0
        
        # Get new matrix of EV stock with ages, LIBs in good use 
        new_matrix <- matrix_ev <- matrix_lib <- matrix_both <- matrix(0, nrow = 31, ncol = 31)
        rownames(new_matrix) <-paste0("EV_",0:30) # ROWS are EV
        colnames(new_matrix) <- paste0("LIB_",0:30) # COLS are Battery
        
        
        for (i in 1:31) { # EV
          for (j in 1:31) { # LIB
            if (matrix_data[i, j] != 0) {
              result <- f.getOutflows(matrix_data[i, j],i-1,j-1) # age is minus 1 for the index
              if (i!=31 & j!=31){ # to avoid border case
                new_matrix[i + 1, j + 1] <- result$none # move 1 age for both EV and LIB
                matrix_ev[i+1,j+1] <- result$lib_fail # EVs that need LIB
                matrix_lib[i+1,j+1] <- result$ev_fail # LIBs available to use
                matrix_both[i+1,j+1] <- result$both_fail # LIB failed for recycling
              } else if (j==31 & i!=31){ # BATTERIES TOO OLD
                matrix_ev[i+1,j] <- result$lib_fail # EVs that need LIB, no LIBs available as they died
                matrix_both[i+1,j] <- result$both_fail
              } else if (j!=31 & i==31){ # EV TOO OLD
                matrix_lib[i,j+1] <- result$ev_fail # LIBs available to use, no EV at border
                matrix_both[i,j+1] <- result$both_fail
              }
            }
          }
        }
        # get vector of outflows of EV and outflows of LIBs
        ev_need <- rowSums(matrix_ev)
        
        # Above certain age simply no LIB required, THEY DIED
        ev_need[(max_ev_age+1):31] <- 0
        
        # move to the left to allow for delay in other part of the code
        lib_failed <- colSums(matrix_ev)[-1] + colSums(matrix_both)[-1] # LIB ready for end life recycling, when the LIB failed
        lib_available <- colSums(matrix_lib)
        
        # assigning old batteries TO EVs
        lib_to_EV <- lib_available*max_reuse_lib
        # limit age of LIB for EV
        lib_to_EV[(max_lib_age_ev+1):31] <- 0
        
        lib_available <- lib_available-lib_to_EV
        
        # first match year to year with offset of years - 8 years
        ev_need <- c(ev_need,rep(0,ev_age_newLib))
        lib_to_EV <- c(rep(0,ev_age_newLib),lib_to_EV)
        allocation <- pmin(ev_need,lib_to_EV)
        
        ev_need <- ev_need - allocation
        lib_to_EV <- lib_to_EV - allocation
        
        # remove offsets
        ev_need <- ev_need[1:31]
        lib_to_EV <- lib_to_EV[-(1:ev_age_newLib)]
        allocation <- allocation[-(1:ev_age_newLib)]
        
        # update new_matrix with stock of EVs and old batteries
        for (i in 1:(31-ev_age_newLib)){
          new_matrix[i+ev_age_newLib,i] <- new_matrix[i+ev_age_newLib,i]+allocation[i]
        }
        
        allocation <- sum(allocation)
        
        # do rest of allocation with LOOP
        start_bat <- 1
        for (i in 31:1) { # start with old
          if (i<=ev_age_newLib){
            # new_matrix[i,0] <- ev_need[i] # new battery DUPLICATED
          } else {
            for (j in start_bat:31) {
              allocated <- min(ev_need[i], lib_to_EV[j])
              ev_need[i] <- ev_need[i] - allocated
              lib_to_EV[j] <- lib_to_EV[j] - allocated
              # update new_matrix with stock of EVs and old batteries
              new_matrix[i,j] <- new_matrix[i,j]+allocated
              allocation <- allocation+allocated
              start_bat <- j
              if (ev_need[i] == 0) { break }
            }
          }
        }
        
        # add remaining batteries back to pool
        lib_available <- lib_available+lib_to_EV
        
        # add EVs with new batteries to stock - note, no other battery with 0 age
        new_matrix[,1] <-  ev_need
        
        # assign numbers for Year - totals and vector
        sales$add_LIB[y-start_year+1] <- round(sum(ev_need),0) # additional new LIBs required
        sales$add_LIB_vector[y-start_year+1] <- list(round(ev_need[-1],0)) 
        # LIBs in good condition for SSPS or recycling
        sales$LIB_Available[y-start_year+1] <- round(sum(lib_available),0)  
        sales$LIB_Available_vector[y-start_year+1] <- list(round(lib_available[-1],0))  
        # LIBs that failed but available to recycle
        sales$LIB_recycling[y-start_year+1] <- round(sum(lib_failed),0)
        sales$LIB_recycling_vector[y-start_year+1] <- list(round(lib_failed,0))
        sales$LIB_reuse_EV[y-start_year+1] <- round(allocation,0)
        sales$EV_Stock[y-start_year+1] <- round(sum(new_matrix),0)
        sales$EV_Stock_vector[y-start_year+1] <- list(unname(round(rowSums(new_matrix)[-1],0)))
        
        
        # end for loop, next year
        matrix_data <- new_matrix
        
        # keep balance of removed EV Sales from stock
        
        rm(new_matrix,matrix_ev,matrix_lib,lib_to_EV,lib_available,allocated,allocation,start_bat)
        
      }
      rm(i,j)
      # save data
      sales_new <- rbind(sales_new,sales)
      
    }
  }  
}

sales <- sales_new

table(sales$Scenario,sales$lifetime)
# merge into scenario
sales <- sales %>% mutate(Scenario=paste0(Scenario,"-",lifetime))


## save stats as World or region-----
# all as percentage of that year sales
sales <- sales %>% 
  filter(Year>2021) %>% 
  mutate(perc_add_lib=if_else(Sales==0,0,add_LIB/Sales),
         perc_lib_reuse_ev=if_else(Sales==0,0,LIB_reuse_EV/Sales),
         perc_lib_available=if_else(Sales==0,0,LIB_Available/Sales),
         perc_lib_recycling=if_else(Sales==0,0,LIB_recycling/Sales))
sales

# Save vector variables as strings
sales <- sales %>%
  rowwise() %>%
  mutate_if(is.list, ~paste(unlist(.), collapse = '|')) 

write.csv(sales,"Parameters/outflows_LIB.csv",row.names = F)

# EoF