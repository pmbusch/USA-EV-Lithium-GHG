# FigOtherImpacts
# PBH Oct 2025

source("Scripts/02-Load_Results.R")

## baseline scenario
df <- df_all %>%
  filter(
    Scenario_Sales == "Ambitious",
    Scenario_Lifetime == "Reference",
    Scenario_Capacity == "Reference",
    Scenario_Recycling == "Recycling 0%",
    Scenario_mpg == "Reference",
    Scenario_Grid == "ref2025"
  )

# fmt: skip
table(df$Scenario_Sales,df$Scenario_Lifetime,df$Scenario_Capacity,df$Scenario_Grid,df$Scenario_mpg,df$Scenario_Recycling)

## VMT  ------
vmt <- read.csv("Results/total_VMT.csv")
vmt <- vmt %>%
  mutate(
    Scenario_Lifetime = str_extract(Scenario, "Reference|Short|Long"),
    Scenario_Sales = str_extract(Scenario, "Momentum|Ambitious"),
    Scenario = NULL
  ) %>%
  filter(Scenario_Sales != "Baseline")
vmt_size <- vmt %>% group_by(Scenario_Sales, Scenario_Lifetime, vehSize) %>% reframe(vmt = sum(total_vmt))

vmt <- vmt %>% group_by(Scenario_Sales, Scenario_Lifetime, Year) %>% reframe(vmt = sum(total_vmt) / 1e6) %>% ungroup()

# 2023, 3.19 trillion miles in USA
# https://afdc.energy.gov/data/10315
vmt_total <- vmt %>% group_by(Scenario_Sales, Scenario_Lifetime) %>% reframe(vmt = sum(vmt) * 1e6) # total miles
vmt_total <- vmt_total %>% filter(Scenario_Sales == "Ambitious", Scenario_Lifetime == "Reference")


unique(df$impact)
dict <- read_excel("Inputs/Dict_Impacts.xlsx")
start_year <- 2025
range(df$Year)

# simplified stages
stage_colors <- viridis::viridis(6, option = "E", direction = -1)
names(stage_colors) <- stage_lvl
df <- df %>%
  mutate(
    Stage = case_when(
      str_detect(Stage, "Recycling|Replacement") ~ "LIB production",
      str_detect(Stage, "Maintenance") ~ "Vehicle production",
      T ~ Stage
    )
  )

# FIGURE ---------

total_df <- df %>%
  left_join(dict) |>
  filter(Category %in% c("Acidification", "Ozone Depletion", "Human health", "Smog formation")) %>%
  group_by(Category, Impact_Name, vehicle_type, Stage) %>%
  reframe(value = sum(value) / 1e9) %>% # from kg to million tons
  ungroup() |>
  mutate(labX = paste0(Category, " (million tons ", Impact_Name, ")"))

ggplot(total_df, aes(vehicle_type, value)) +
  geom_col(
    aes(fill = Stage),
    position = position_stack(),
    col = "black",
    linewidth = 0.1
  ) +
  facet_wrap(~labX, scale = "free") +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = " ", scientific = FALSE),
    expand = expansion(mult = c(0, 0.05))
    # sec.axis = sec_axis(~ . * 1e6 * 1e6 / vmt_total$vmt * 1.61, name = "grams per km")
  ) +
  scale_fill_manual(values = stage_colors) +
  labs(x = "", y = "") +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  theme_bw(8) +
  theme(
    panel.grid = element_blank(),
    plot.tag = element_text(face = "bold"),
    legend.position = "bottom",
    # strip.placement = "outside",
    strip.background = element_rect(fill = NA, color = NA),
    strip.text = element_text(size = 10, face = "bold"),
  )

ggsave("Figures/OtherImpacts.png", ggplot2::last_plot(), units = 'cm', dpi = 600, width = 18, height = 8.7 * 1.5)

# Totals
# total energy comparison
total_df |>
  group_by(labX, vehicle_type) |>
  reframe(value = sum(value)) |>
  pivot_wider(names_from = vehicle_type, values_from = value) |>
  mutate(diff = (EV - ICE) / ICE) |>
  mutate(EV_km = EV * 1e9 / vmt_total$vmt * 1.61, ICE_km = ICE * 1e9 / vmt_total$vmt * 1.61)

# EoF
