## Analysis of baseline results
# Barplots for materials
## PBH Sept 2025

# LOAD Results -------

source("Scripts/02-Load_Results.R")

# baseline scenario
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



# Figure -----

unique(df$impact)
dict <- read_excel("Inputs/Dict_Impacts.xlsx")
start_year <- 2025
range(df$Year)

total_df <- df %>% 
  left_join(dict) %>%
  filter(!is.na(Category)) %>% 
  group_by(Category,Impact_Name,Abbr,vehicle_type,Stage) %>% 
  reframe(value=sum(value)) %>% ungroup() %>% #   mutate(Stage=factor(Stage,levels=stage_lvl)) %>% 
  mutate(Impact_Name=Impact_Name %>% str_replace("Crudeoil","Crude oil") %>% 
           str_replace("Naturalgas","Natural gas") %>% str_replace("Non metal","Other Materials")) %>% 
  mutate(labX=paste0(Impact_Name,"-",vehicle_type))


stage_colors <- viridis::viridis(6, option = "E",direction = -1)
names(stage_colors) <- stage_lvl


## Materials ------
# order

mat_levels <- c("Aluminium","Cobalt","Copper","Lithium","Nickel",
                "REE","Zinc","Barium","Calcium","Chromium","Iron","Magnesium","Manganese",
                "Phosphorus","Silicon","Sulphur","Other Materials")
comb_lvl2 <- expand.grid(mat_levels,stage_lvl) %>% mutate(x=paste0(Var1,Var2)) %>% pull(x)

# fuel colors
mat_colors <- scico::scico(17, palette = "batlow", direction = 1)
names(mat_colors) <- mat_levels


data_fig4 <- total_df %>% 
  # filter(Category %in% c("Material consumption detail","ETM")|Impact_Name=="Other Materials") %>% 
  filter(Category %in% c("Material consumption detail","ETM")) %>%
  mutate(value=value/1e9) %>% 
  mutate(lvl=factor(paste0(Impact_Name,Stage),levels=comb_lvl2)) %>% 
  mutate(vehicle_type=factor(vehicle_type)) %>% 
  arrange(lvl) %>% 
  mutate(abb_lab=if_else(abs(value)>10,Abbr,""))


# minor effect
# substract from other materials
# subst <- data_fig4 %>% 
#   filter(Impact_Name!="Other Materials") %>% 
#   group_by(vehicle_type,Stage) %>% reframe(minus_mat=sum(value)) %>% ungroup()
# 
# data_fig4 <- data_fig4 %>% 
#   left_join(subst) %>% 
#   mutate(value=value-if_else(Impact_Name=="Other Materials",minus_mat,0))


data_fig4a <- data_fig4 %>% 
  group_by(vehicle_type,Stage) %>% 
  reframe(value=sum(value)) %>% ungroup()

total_lab4 <- total_df %>%
  filter(Category %in% c("Material consumption detail","ETM")) %>% 
  group_by(Category,vehicle_type) %>% 
  reframe(value=sum(value)/1e9) %>% ungroup() %>% 
  mutate(lab_total=paste0(round(value)," Mtons")) 

p4 <- ggplot(data_fig4,aes(as.numeric(vehicle_type)+0.15,value))+
  geom_col(data=data_fig4a,aes(fill=Stage),position = position_stack(),
           col="black",linewidth=0.1,width=0.3)+
  # geom_text(data=total_lab4,aes(label=lab_total),size=8*5/14 * 0.8,
  #           nudge_y = 0)+
  # by energy
  geom_col(aes(x=as.numeric(vehicle_type)-0.15,fill=Impact_Name,group = lvl),position = position_stack(),
           col="black",linewidth=0.1,width=0.3)+
  geom_text(position = position_stack(vjust = 0.5),size=6*5/14 * 0.8,
            aes(x=as.numeric(vehicle_type)-0.15,label = abb_lab,group=lvl))+
  coord_flip()+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE),
                     sec.axis = sec_axis(~ . * 1e9 / vmt_total$vmt*1.61, name = "kg per km"))+
  scale_fill_manual(values = c(stage_colors,mat_colors),
                    breaks = names(mat_colors))+
  scale_x_continuous(breaks=1:2,labels=c("EV","ICEV"))+
  labs(x="",y = "Million tons",fill="Material",tag="(c)",title="Material requirements")+
  guides(fill= guide_legend(reverse = TRUE,nrow = 2))+
  theme_bw(8)+
  theme(panel.grid = element_blank(),
        axis.title.y.right = element_text(face = "italic"),
        plot.tag = element_text(face = "bold"),
        legend.position = "bottom")
p4


## ETMs -----
data_fig5 <- data_fig4 %>% filter(Category %in% c("ETM")) %>% 
  mutate(abb_lab=if_else(abs(value)>2,Abbr,""))
data_fig5a <- data_fig5 %>% 
  group_by(vehicle_type,Stage) %>% 
  reframe(value=sum(value)) %>% ungroup()


p5 <- ggplot(data_fig5,aes(as.numeric(vehicle_type)+0.15,value))+
  geom_col(data=data_fig5a,aes(fill=Stage),position = position_stack(),
           col="black",linewidth=0.1,width=0.3)+
  # geom_text(data=total_lab4,aes(label=lab_total),size=8*5/14 * 0.8,
  #           nudge_y = 0)+
  # by energy
  geom_col(aes(x=as.numeric(vehicle_type)-0.15,fill=Impact_Name,group = lvl),position = position_stack(),
           col="black",linewidth=0.1,width=0.3)+
  geom_text(position = position_stack(vjust = 0.5),size=6*5/14 * 0.8,col="white",
            aes(x=as.numeric(vehicle_type)-0.15,label = abb_lab,group=lvl))+
  coord_flip()+
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE),
                     sec.axis = sec_axis(~ . * 1e9 / vmt_total$vmt*1.61, name = "kg per km"))+
  scale_fill_manual(values = c(stage_colors,mat_colors),
                    breaks = names(mat_colors))+
  scale_x_continuous(breaks=1:2,labels=c("EV","ICEV"))+
  labs(x="",y = "Million tons",fill="Material",tag="(d)",title="Critical Minerals requirements")+
  guides(fill= guide_legend(reverse = TRUE,nrow = 1))+
  theme_bw(8)+
  theme(panel.grid = element_blank(),
        axis.title.y.right = element_text(face = "italic"),
        plot.tag = element_text(face = "bold"),
        legend.position = "bottom")
p5
