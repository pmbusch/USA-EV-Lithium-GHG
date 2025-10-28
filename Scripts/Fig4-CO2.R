# Time series of total carbon emissions
# PBH OCt 2025

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


total_df <- df %>%
  filter(impact == "kgCO2eq") %>%
  group_by(vehicle_type, Stage) %>%
  reframe(value = sum(value)) %>%
  ungroup() %>%
  mutate(labX = paste0("CO2eq-", vehicle_type))

# FIGURE ---------

## CO2 -----
total_lab <- total_df %>%
  mutate(value = value / 1e9) %>% # to Mtons
  group_by(vehicle_type) %>%
  reframe(value = sum(value)) %>%
  ungroup() %>%
  mutate(lab_total = paste0(round(value / 1e3), "M"))

p1 <- total_df %>%
  mutate(value = value / 1e9) %>% # to Mtons
  ggplot(aes(vehicle_type, value)) +
  geom_col(
    aes(fill = Stage),
    position = position_stack(),
    col = "black",
    linewidth = 0.1
  ) +
  # geom_text(
  #   data = total_lab,
  #   aes(label = lab_total),
  #   size = 8 * 5 / 14 * 0.8,
  #   nudge_y = 1000
  # ) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = " ", scientific = FALSE),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * 1e6 * 1e6 / vmt_total$vmt * 1.61, name = expression("grams " ~ CO[2] * e ~ " per km"))
  ) +
  scale_fill_manual(values = stage_colors) +
  labs(
    x = "",
    y = expression("Million tons " ~ CO[2] * italic(e) ~ " emissions 2025–2050 (whole fleet)"),
    tag = "(b)",
    title = "Cumulated emissions"
  ) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  theme_bw(8) +
  theme(panel.grid = element_blank(), plot.tag = element_text(face = "bold"), legend.position = "none")
p1


head(df)
data_fig_zoom <- df %>%
  filter(impact == "kgCO2eq") %>%
  filter(value > 0) |> # avoid amortization
  group_by(vehicle_type, Year) %>%
  reframe(metric = sum(value) / 1e3 / 1e6) %>%
  ungroup()

p_zoom <- ggplot(data_fig_zoom, aes(Year, metric, col = vehicle_type)) +
  geom_line(linewidth = 0.5) +
  geom_text(
    data = filter(data_fig_zoom, Year == 2049),
    aes(label = vehicle_type),
    size = 6 * 5 / 14 * 0.8,
    nudge_y = 40
  ) +
  ylim(0, 1750) +
  coord_cartesian(expand = F) +
  labs(x = "", y = "", fill = "", col = "", title = "") +
  theme_bw(6.5) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(hjust = 1), legend.position = "none")
p_zoom

# same figure but with stacked chars
data_fig <- df %>%
  filter(impact == "kgCO2eq") %>%
  filter(value > 0) |> # avoid amortization
  group_by(vehicle_type, Year, Stage) %>%
  reframe(metric = sum(value) / 1e3 / 1e6) %>%
  ungroup()

stage_colors <- viridis::viridis(6, option = "E", direction = -1)
names(stage_colors) <- stage_lvl

p <- ggplot(data_fig, aes(Year, metric, fill = Stage)) +
  geom_col(col = "black", linewidth = 0.1) +
  facet_wrap(~vehicle_type) +
  ylim(0, 1750) +
  coord_cartesian(expand = F) +
  scale_fill_manual(values = stage_colors) +
  labs(
    x = "",
    y = "",
    fill = "",
    col = "",
    tag = "(a)",
    title = expression("Climate change [" ~ million ~ tons ~ CO[2] * eq * "]")
  ) +
  theme_bw(8) +
  theme(
    panel.grid = element_blank(),
    # axis.text.x = element_text(hjust = 1),
    panel.spacing.x = unit(0.5, "cm"),
    plot.tag = element_text(face = "bold"),
    legend.position = "right"
  )

# Scenarios -------

source("Scripts/02b-Scenario_Load_Results.R")

df_scen <- df_all_scen %>%
  filter(impact == "kgCO2eq") %>%
  filter(Scenario_Sales == "Ambitious", Scenario_Recycling == "Recycling 0%") |>
  group_by(vehicle_type, Scenario_Capacity, Scenario_Lifetime, Scenario_Grid, Scenario_mpg) %>%
  reframe(value = sum(value) / 1e9) # to million tons

# Ranges for ICE
range_ice <- df_scen |>
  filter(vehicle_type == "ICE") |>
  filter(Scenario_Grid == "ref2025", Scenario_Capacity == "Reference", Scenario_Lifetime == "Reference") |>
  group_by(vehicle_type) |>
  reframe(minCO2 = min(value), maxCO2 = max(value)) |>
  mutate(scen = "MPG Improvement")

# Range Lifetime
range_life <- df_scen |>
  filter(vehicle_type == "EV") |>
  filter(Scenario_Grid == "ref2025", Scenario_Capacity == "Reference") |>
  group_by(vehicle_type) |>
  reframe(minCO2 = min(value), maxCO2 = max(value)) |>
  mutate(scen = "LIB Lifetime")

# Range Capacity
range_cap <- df_scen |>
  filter(vehicle_type == "EV") |>
  filter(Scenario_Grid == "ref2025", Scenario_Lifetime == "Reference") |>
  group_by(vehicle_type) |>
  reframe(minCO2 = min(value), maxCO2 = max(value)) |>
  mutate(scen = "LIB Capacity")

# Range Grid
range_grid <- df_scen |>
  filter(vehicle_type == "EV") |>
  filter(Scenario_Lifetime == "Reference", Scenario_Capacity == "Reference") |>
  group_by(vehicle_type) |>
  reframe(minCO2 = min(value), maxCO2 = max(value)) |>
  mutate(scen = "Electricity Grid")

range <- rbind(range_ice, range_life, range_cap, range_grid)
p2 <- p1 +
  geom_linerange(
    data = range,
    aes(x = vehicle_type, ymin = minCO2, ymax = maxCO2, col = scen),
    position = position_dodge(width = 0.5),
    inherit.aes = F
  ) +
  geom_text(data=range,aes(x=vehicle_type,label=scen,y=maxCO2+1.8e3,col=scen),size = 6 * 5 / 14 * 0.8,position = position_dodge(width = 0.5),inherit.aes = F) +
  scale_color_manual(
    values = c(
      "MPG Improvement" = "#1F77B4",
      "LIB Lifetime" = "#2CA02C",
      "LIB Capacity" = "#FF7F0E",
      "Electricity Grid" = "#9467BD"
    )
  ) +
  coord_flip(ylim = c(0, 27.5e3))
p2

library(cowplot)
plot_grid(p, p2, ncol = 1)

ggsave("Figures/Fig4.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 16, height = 8.7)

pdf("Figures/PDF/Figure4.pdf", width = 16 / 2.54, height = 8.7 / 2.54)
ggplot2::last_plot()
dev.off()

# Totals
total_df |>
  group_by(vehicle_type) |>
  reframe(value = sum(value) / 1e9) |>
  pivot_wider(names_from = vehicle_type, values_from = value) |>
  mutate(diff = (EV - ICE) / ICE) |>
  mutate(EV_km = EV * 1e3 / vmt_total$vmt * 1.61, ICE_km = ICE * 1e3 / vmt_total$vmt * 1.61)

# Stage accounting
total_df |>
  group_by(Stage, vehicle_type) |>
  reframe(value = sum(value, na.rm = T) / 1e9) |>
  pivot_wider(names_from = vehicle_type, values_from = value) |>
  mutate(perc_EV = EV / sum(EV), perc_ICE = ICE / sum(ICE, na.rm = T))


# Discounting --------------

unique(df$impact)

gwp <- tibble(
  impact = c("kg_CO2", "kg_CH4", "kg_N2O", "kg_PFC116", "kg_PFC14", "kg_SF6"),
  gwp = c(1, 29.8, 273, 12400, 7380, 24300)
)

data_fig2 <- df %>%
  filter(impact %in% c("kg_CO2", "kg_CH4", "kg_N2O", "kg_PFC116", "kg_PFC14", "kg_SF6")) %>%
  group_by(impact, vehicle_type, Year, Stage) %>%
  reframe(metric = sum(value) / 1e3 / 1e6) %>%
  ungroup() # million tons

# compare to CO2eq - Perfect fit
data_fig2 %>%
  left_join(gwp) %>%
  mutate(metric = metric * gwp) %>%
  group_by(vehicle_type, Year) %>%
  reframe(gwp_calc = sum(metric)) %>%
  left_join(data_fig_zoom) %>%
  pivot_longer(c(gwp_calc, metric), names_to = "key", values_to = "value") %>%
  ggplot(aes(Year, value, col = key)) +
  geom_line() +
  facet_wrap(~vehicle_type)


# TAWP - discount based using AR6 and Kendall 2012 (https://link.springer.com/article/10.1007/s11367-012-0436-5)
tawp <- read_excel("Inputs/TAWP_AR6.xlsx", sheet = "GWP_discount_100y")
tawp <- tawp %>%
  dplyr::select(-year) %>%
  rename(Year = period) %>%
  pivot_longer(c(-Year), names_to = "impact", values_to = "gwp") %>%
  mutate(impact = paste0("kg_", impact))


data_fig2 <- data_fig2 %>% left_join(tawp) %>% mutate(metric = metric * gwp)

# majority is CO2
data_fig2 %>%
  group_by(vehicle_type, impact, Year) %>%
  reframe(metric = sum(metric)) %>%
  ungroup() %>%
  ggplot(aes(Year, metric, fill = impact)) +
  geom_col() +
  facet_wrap(~vehicle_type)

# by stage
data_fig2 %>%
  group_by(vehicle_type, Stage, Year) %>%
  reframe(metric = sum(metric)) %>%
  ungroup() %>%
  ggplot(aes(Year, metric, fill = Stage)) +
  geom_col(col = "black", linewidth = 0.1) +
  facet_wrap(~vehicle_type) +
  # ylim(0,1750)+
  coord_cartesian(expand = F) +
  scale_fill_manual(values = stage_colors) +
  labs(x = "", y = "", fill = "", col = "", title = "million tons CO2eq - Equivalent to 2025") +
  theme_bw(8) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(hjust = 1),
    panel.spacing.x = unit(0.5, "cm"),
    legend.position = "right"
  )


# Bar plot comparison
df1 <- total_df %>%
  group_by(vehicle_type, Stage) %>%
  reframe(metric = sum(value) / 1e9) %>%
  ungroup() %>%
  mutate(vehicle_type = factor(vehicle_type)) %>%
  mutate(key = "Standard")

df2 <- data_fig2 %>%
  group_by(vehicle_type, Stage) %>%
  reframe(metric = sum(metric)) %>%
  ungroup() %>%
  mutate(vehicle_type = factor(vehicle_type)) %>%
  mutate(key = "Discounting")

# quantify difference for EV
df1 %>%
  group_by(vehicle_type) %>%
  reframe(metric = sum(metric)) %>%
  pivot_wider(names_from = vehicle_type, values_from = metric) %>%
  mutate(red = (ICE - EV) / ICE, delta = ICE - EV, red_label = paste0(round(red * 100, 0), "%"))

df2 %>%
  group_by(vehicle_type) %>%
  reframe(metric = sum(metric)) %>%
  pivot_wider(names_from = vehicle_type, values_from = metric) %>%
  mutate(red = (ICE - EV) / ICE, delta = ICE - EV, red_label = paste0(round(red * 100, 0), "%"))

# difference in delta emissions
(15514 - 13263) / 15514 # 14% reduction in total emissions reduction

# difference for EV (GWP vs TAWP)
(10589 - 9480) / 10589
# For ICE (GWP vs TAW[])
(25391 - 22173) / 25391

ggplot(df1, aes(as.numeric(vehicle_type) + 0.2, metric, fill = Stage)) +
  geom_col(col = "black", linewidth = 0.1, width = 0.4) +
  geom_col(
    data = df2,
    aes(x = as.numeric(vehicle_type) - 0.2),
    col = "black",
    linewidth = 0.1,
    width = 0.4,
    alpha = .5
  ) +
  annotate(
    geom = "text",
    x = 1.2,
    y = 13e3,
    label = "GWP-100  (standard)",
    size = 9 * 5 / 14 * 0.8,
    fontface = "italic"
  ) +
  annotate(
    geom = "text",
    x = 0.8,
    y = 12e3,
    label = "Discounted GWP-100",
    size = 9 * 5 / 14 * 0.8,
    fontface = "italic"
  ) +
  scale_fill_manual(values = stage_colors) +
  scale_x_continuous(breaks = 1:2, labels = c("EV", "ICEV")) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = " ", scientific = FALSE),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . * 1e6 * 1e6 / vmt_total$vmt * 1.61, name = expression("grams " ~ CO[2] * e ~ " per km"))
  ) +
  coord_flip() +
  labs(
    x = "",
    fill = "",
    col = "",
    y = expression("Million tons " ~ CO[2] * italic(e) ~ " emissions 2025–2050 (whole fleet)")
  ) +
  theme_bw(8) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(hjust = 1),
    panel.spacing.x = unit(0.5, "cm"),
    legend.position = "bottom"
  )

ggsave("Figures/Fig4_discount.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)


# old, not relevant
# Lithium Figure ----------

# get diff EV minus ICE
data_fig <- df %>%
  filter(impact == "kgCO2eq") %>%
  group_by(vehicle_type, Year) %>%
  reframe(kg = sum(value)) %>%
  ungroup()

data_fig <- data_fig %>% pivot_wider(names_from = vehicle_type, values_from = kg) %>% mutate(delta = EV - ICE)

# kg lithium or per battetry
li <- df %>%
  filter(impact %in% c("kgLithium")) %>%
  group_by(impact, vehicle_type, Year) %>%
  reframe(kg = sum(value)) %>%
  ungroup()

### add kWh requirements
kwh <- kwh_total %>%
  filter(Scenario_Sales == "Ambitious", Scenario_Lifetime == "Reference", Scenario_Capacity == "Reference") %>%
  group_by(Year, vehicle_type, Stage) %>%
  reframe(kwh = sum(kwh)) %>%
  ungroup()
kwh_recyc <- kwh_recycling_total %>%
  filter(
    Scenario_Sales == "Ambitious",
    Scenario_Lifetime == "Reference",
    Scenario_Recycling == "Recycling 45%",
    Scenario_Capacity == "Reference"
  ) %>%
  group_by(Year, vehicle_type, Stage) %>%
  reframe(kwh = sum(kwh)) %>%
  ungroup()
kwh <- rbind(kwh, kwh_recyc) %>%
  group_by(Year, vehicle_type) %>%
  reframe(kg = sum(kwh)) %>%
  ungroup() %>%
  mutate(impact = "kwh")


li <- li %>%
  rbind(kwh) %>%
  pivot_wider(names_from = vehicle_type, values_from = kg) %>%
  mutate(ICE = if_else(is.na(ICE), 0, ICE)) %>% # for kWh case
  mutate(delta_Li = EV - ICE) %>%
  dplyr::select(impact, Year, delta_Li)

data_fig <- data_fig %>%
  left_join(li) %>%
  mutate(
    impact = if_else(impact == "kgLithium", "tons CO2e avoided per kg Lithium", "tons CO2e avoided per kWh of battery")
  )
mutate(metric = -delta / delta_Li / 1e3) # tons avoided per kg of lithium

# over whole period
data_fig %>% group_by(impact) %>% reframe(x = sum(delta) / 1e3, y = sum(delta_Li)) %>% mutate(metric = -x / y) # 7.32 tons CO2 per kgLi, 470 kg CO2e per kWh


ggplot(data_fig, aes(Year, metric)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~impact, scales = "free") +
  # ylim(0,16)+
  # coord_cartesian(expand = F)+
  labs(x = "", y = "", fill = "", col = "", title = "tons CO2eq avoided per kg Lithium") +
  theme_bw(8) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(hjust = 1), legend.position = "none")

ggsave("Figures/Li_ts.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 2, height = 8.7)

# EoF
