# Cambium  Electricity Dataset 2024
# PBH SEPT 2025
# For electricity scenario

# Data source:
# Gagnon, Pieter; Pedro Andres Sanchez Perez; Julian Florez; James Morris; Marck Llerena Velasquez; and Jordan Eisenman. Cambium 2024 Data. National Renewable Energy Laboratory. https://scenarioviewer.nrel.gov
# https://scenarioviewer.nrel.gov/?project=5c7bef16-7e38-4094-92ce-8b03dfa93380&mode=download&layout=Default

library(tidyverse)

# LOAD -----

df <- read.csv("Inputs/Cambium/Cambium24_allScenarios_annual_gea.csv", skip = 5)


names(df)
df <- df %>%
  filter(scenario == "MidCase") %>%
  dplyr::select(
    gea,
    t,
    battery_MWh,
    biomass_MWh,
    canada_MWh,
    coal_MWh,
    csp_MWh,
    distpv_MWh,
    gas.cc_MWh,
    gas.ct_MWh,
    geothermal_MWh,
    hydro_MWh,
    nuclear_MWh,
    o.g.s_MWh,
    phs_MWh,
    upv_MWh,
    wind.ons_MWh,
    wind.ofs_MWh
  )

# re-classify, use EIA names to link to LCI easier
df <- df %>%
  pivot_longer(c(-gea, -t), names_to = "type", values_to = "MWh") %>%
  mutate(type = str_remove(type, "_MWh")) %>%
  mutate(
    type = case_when(
      type == "battery" ~ "Battery",
      type == "biomass" ~ "Wood and Other Biomass",
      type == "canada" ~ "Import Canada",
      type == "coal" ~ "Coal",
      type == "csp" ~ "Solar Thermal",
      type == "distpv" ~ "Solar Photovoltaic",
      type == "gas.cc" ~ "Natural Gas Combined Cycle",
      type == "gas.ct" ~ "Natural Gas Conventional",
      type == "geothermal" ~ "Geothermal",
      type == "hydro" ~ "Hydropower",
      type == "nuclear" ~ "Nuclear",
      type == "o.g.s" ~ "Natural Gas Conventional",
      type == "phs" ~ "Hydropower",
      type == "upv" ~ "Solar Photovoltaic",
      type == "wind.ons" ~ "Wind",
      type == "wind.ofs" ~ "Offshore Wind",
      T ~ type
    )
  )

# aggregate to new categorues
df <- df %>%
  group_by(gea, t, type) %>%
  reframe(MWh = sum(MWh)) %>%
  ungroup()

df %>%
  filter(t == 2035) %>%
  group_by(type) %>%
  reframe(MWh = sum(MWh)) %>%
  ungroup() %>%
  arrange(desc(MWh)) %>%
  mutate(share = MWh / sum(MWh))

# remove Battery and imports
df <- df %>% filter(!(type %in% c("Battery", "Import Canada")))


# colors
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")
cat_colors <- names(fuel_colors)

# For United States
data_fig <- df %>% mutate(value = MWh / 1e6)
data_fig <- data_fig %>% mutate(type = factor(type, levels = cat_colors))
p1 <- ggplot(data_fig, aes(t, value)) +
  geom_col(aes(fill = type)) +
  facet_wrap(~gea, scales = "free_y") +
  coord_cartesian(expand = F) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ' ')) +
  scale_fill_manual(values = fuel_colors) +
  labs(x = "", y = "", title = "Generation [billion kWh]", fill = "") +
  theme_bw(7) +
  guides(fill = guide_legend(nrow = 6)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.8, 0.1),
    legend.text = element_text(size = 6.5),
    legend.key.height = unit(0.25, 'cm'),
    legend.key.width = unit(0.25, 'cm'),
    legend.background = element_rect(fill = "transparent", color = NA)
  )
p1

ggsave(
  "Figures/Electricity/Cambium_MixEnergy.png",
  ggplot2::last_plot(),
  units = "cm",
  dpi = 600,
  width = 18.5,
  height = 9.7
)

data_agg <- data_fig %>%
  group_by(t, type) %>%
  reframe(value = sum(value)) %>%
  ungroup() %>%
  mutate(gea = "USA")
p1 %+% data_agg

# MIX
data_fig <- df %>% group_by(t, gea) %>% mutate(value = MWh / sum(MWh))
data_fig <- data_fig %>% mutate(type = factor(type, levels = cat_colors))
p1 <- ggplot(data_fig, aes(t, value)) +
  geom_col(aes(fill = type)) +
  facet_wrap(~gea) +
  coord_cartesian(expand = F) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = fuel_colors) +
  labs(
    x = "",
    y = "",
    title = "Share of Electricity Generation [%]",
    fill = ""
  ) +
  theme_bw(7) +
  guides(fill = guide_legend(nrow = 6)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.8, 0.1),
    legend.text = element_text(size = 6.5),
    legend.key.height = unit(0.25, 'cm'),
    legend.key.width = unit(0.25, 'cm'),
    legend.background = element_rect(fill = "transparent", color = NA)
  )
p1

ggsave(
  "Figures/Electricity/Cambium_Mix.png",
  ggplot2::last_plot(),
  units = "cm",
  dpi = 600,
  width = 18.5,
  height = 9.7
)


# add linear interpolation every 5 years
unique(df$t)
df <- df %>%
  group_by(gea, type) %>%
  complete(t = full_seq(t, 1)) %>%
  arrange(t) %>%
  mutate(MWh = zoo::na.approx(MWh, x = t, na.rm = FALSE)) %>%
  ungroup()


# Use data for GWh total generation
head(df)
df <- df %>% group_by(gea, t) %>% mutate(share = MWh / sum(MWh)) %>% ungroup()
df %>% group_by(gea, t) %>% reframe(sum(share))


write.csv(df, "Parameters/Electricity/Cambium_mix.csv", row.names = F)

# Long term marginal emissions factors -----
# get emissions directly

df <- read.csv("Inputs/Cambium/Cambium24_allScenarios_annual_gea.csv", skip = 5)

lme <- df %>%
  filter(scenario == "MidCase") %>%
  # combustion + precombustion
  dplyr::select(
    gea,
    t,
    "lrmer_co2_c",
    "lrmer_ch4_c",
    "lrmer_n2o_c",
    "lrmer_co2_p",
    "lrmer_ch4_p",
    "lrmer_n2o_p"
  )

lme <- lme %>%
  pivot_longer(c(-gea, -t), names_to = "pollutant", values_to = "kg_MWh") %>%
  mutate(pollutant = str_extract(pollutant, "co2|ch4|n2o")) %>%
  group_by(gea, t, pollutant) %>%
  reframe(kg_MWh = sum(kg_MWh)) %>%
  ungroup()


# add linear interpolation every 5 years
lme <- lme %>%
  group_by(gea, pollutant) %>%
  complete(t = full_seq(t, 1)) %>%
  arrange(t) %>%
  mutate(kg_MWh = zoo::na.approx(kg_MWh, x = t, na.rm = FALSE)) %>%
  ungroup()

# group them by state level, using population by county

census_join <- read.csv("Parameters/census_joins.csv")
cambium_join <- read.csv("Inputs/Cambium/county_to_gea_mapping_cambium23.csv")
# add some manual fixes
cambium_join <- cambium_join %>%
  mutate(State = NULL) %>%
  rbind(tibble(
    State.FIPS = 46,
    County.FIPS = 102,
    County = "Oglala Lakota",
    State.Abbr = "SD",
    ReEDS.BA = "p38",
    Cambium.GEA = "SPP_North"
  ))

cambium_join <- census_join %>%
  left_join(
    cambium_join,
    by = c("STATEFP" = "State.FIPS", "COUNTYFP" = "County.FIPS")
  )

cambium_join <- cambium_join %>%
  mutate(
    Cambium.GEA = if_else(
      is.na(Cambium.GEA) & Region_EMM == "ISNE",
      "ISONE",
      Cambium.GEA
    )
  )

# Filter Hawai and Alaska
cambium_join <- cambium_join %>%
  filter(!(STATEFP %in% c(2, 15)))

cambium_join %>% filter(is.na(Cambium.GEA))

# add emission factor
df_cambium <- cambium_join %>%
  left_join(lme, by = c("Cambium.GEA" = "gea"))

# by state
lme_state <- df_cambium %>%
  rename(period = t) %>%
  group_by(period, State, pollutant) %>%
  reframe(kg_MWh = weighted.mean(kg_MWh, pop)) %>%
  ungroup()

write.csv(
  lme_state,
  "Parameters/Electricity/cambium_LongMarginal.csv",
  row.names = F
)

# EoF
