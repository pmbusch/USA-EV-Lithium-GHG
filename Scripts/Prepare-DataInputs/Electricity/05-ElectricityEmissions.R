# Grid emissions at county level, with forecast to 2050
# PBH March 2025

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load inputs or parameters ---------
# county data
census_join <- read.csv("Parameters/census_joins.csv")
head(census_join)
# Note that HI and AK do not have a EIA region, but do have detail on ecoinvent region

# Electricity mixes
mix <- read.csv("Parameters/Electricity/EIA_mix.csv")
mix_HI_AK <- read.csv("Parameters/Electricity/HI_AK_mix.csv")
mix_cambium <- read.csv("Parameters/Electricity/Cambium_mix.csv")


# Electricity Mixes -----------
# Need to add HI and AK to EIA

# share mix of EIA (average emissions)
names(mix)
mix %>% group_by(period, regionName) %>% reframe(x = sum(share)) %>% arrange(desc(x))
mix <- mix %>% dplyr::select(scenario, period, regionName, fuel, share)

# HI and AK
names(mix_HI_AK)
mix_HI_AK <- mix_HI_AK %>% rename(regionName = SRNAME)
mix_HI_AK %>% group_by(period, regionName) %>% reframe(x = sum(share)) %>% arrange(desc(x))
mix_HI_AK <- mix_HI_AK %>% dplyr::select(period, regionName, fuel, share)

# same EIA scenario for all HI and AK
aux <- c()
for (i in unique(mix$scenario)) {
  aux <- rbind(aux, mutate(mix_HI_AK, scenario = i))
}
rm(i)
mix <- rbind(mix, aux)

# census data ---

# correct issue with census - add HI and AK from ecoinvent region map
census_join <- census_join %>%
  mutate(
    Region_Electricity_EIA = if_else(State %in% c("Hawaii", "Alaska"), Region_ecoinvent_detail, Region_Electricity_EIA)
  )

df <- census_join %>% left_join(mix, by = c("Region_Electricity_EIA" = "regionName"))
nrow(df) / 1e6 # 15M
names(df)
df %>% filter(is.na(fuel)) # perfect join

# Join cambium
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

cambium_join <- census_join %>% left_join(cambium_join, by = c("STATEFP" = "State.FIPS", "COUNTYFP" = "County.FIPS"))

cambium_join <- cambium_join %>%
  mutate(Cambium.GEA = if_else(is.na(Cambium.GEA) & Region_EMM == "ISNE", "ISONE", Cambium.GEA))
# Remove Hawai and Alaska
cambium_join <- cambium_join %>% filter(!(STATEFP %in% c(2, 15)))

# perfect join
cambium_join %>% filter(is.na(Cambium.GEA))

# add mix
df_cambium <- cambium_join %>% left_join(mix_cambium, by = c("Cambium.GEA" = "gea"))

# Join to big DF as scenario
names(df)
names(df_cambium)
df_cambium <- df_cambium %>%
  dplyr::select(
    State,
    NAMELSAD,
    STATEFP,
    COUNTYFP,
    NAME,
    pop,
    Region_Transport,
    Region_EMM,
    Region_Electricity_EIA,
    Region_ecoinvent_detail,
    Region_ecoinvent,
    t,
    type,
    share
  ) %>%
  rename(period = t, fuel = type) %>%
  mutate(scenario = "Cambium")

# add Alaska and Hawai
df_cambium <- rbind(df_cambium, filter(df, scenario == "ref2025", State %in% c("Alaska", "Hawaii"))) %>%
  mutate(scenario = "Cambium")

df <- rbind(df, df_cambium)

unique(df$scenario)
nrow(df) / 1e6 # 16M

# Ecoinvent (impacts per kWh) ----

# emission factors
impacts <- read.csv("Parameters/Electricity/ecoinvent_electricity.csv")
range(impacts$kgCO2eq)
names(impacts)

# Match fuels
impacts <- impacts %>%
  filter(str_detect(Name, "electricity production|waste incineration")) %>%
  mutate(Name = str_remove(Name, "electricity production, "))
unique(impacts$Name)
unique(df$fuel)
join_fuel <- read.csv("Inputs/join_fuels.csv")
impacts <- impacts %>% rename(ecoinvent_fuel = Name) %>% left_join(join_fuel) %>% rename(fuel = EIA_Fuel)

# filter fuels not used
impacts <- impacts %>% filter(!is.na(fuel))

# national avg
impacts_nat <- impacts %>%
  group_by(fuel) %>%
  reframe(across(-c(sheet, ecoinvent_fuel, Region), ~ mean(.x))) %>%
  ungroup()

range(impacts_nat$kgCO2eq)

# Join to census
unique(census_join$Region_ecoinvent)
unique(impacts$Region)

df_impacts <- df %>% left_join(impacts, by = c("Region_ecoinvent" = "Region", "fuel"))
names(df_impacts)
df_impacts %>% filter(is.na(kgCO2eq)) %>% group_by(scenario, Region_ecoinvent, fuel) %>% tally()

# unmatch could be due to regional part, so add national averages
df_impacts$ecoinvent_fuel <- NULL

names(impacts_nat)[-1] <- paste0("nat_", names(impacts_nat)[-1])


df_impacts <- df_impacts %>%
  left_join(impacts_nat) %>%
  mutate(
    kgCO2eq = if_else(is.na(kgCO2eq), nat_kgCO2eq, kgCO2eq),
    MJ = if_else(is.na(MJ), nat_MJ, MJ),
    MJ_nonRenewable = if_else(is.na(MJ_nonRenewable), nat_MJ_nonRenewable, MJ_nonRenewable),
    kgSO2eq = if_else(is.na(kgSO2eq), nat_kgSO2eq, kgSO2eq),
    kgCFC11eq = if_else(is.na(kgCFC11eq), nat_kgCFC11eq, kgCFC11eq),
    kgPM2.5eq = if_else(is.na(kgPM2.5eq), nat_kgPM2.5eq, kgPM2.5eq),
    kgO3eq = if_else(is.na(kgO3eq), nat_kgO3eq, kgO3eq),
    MJ_Biomass = if_else(is.na(MJ_Biomass), nat_MJ_Biomass, MJ_Biomass),
    MJ_Coal = if_else(is.na(MJ_Coal), nat_MJ_Coal, MJ_Coal),
    MJ_Crudeoil = if_else(is.na(MJ_Crudeoil), nat_MJ_Crudeoil, MJ_Crudeoil),
    MJ_Geothermal = if_else(is.na(MJ_Geothermal), nat_MJ_Geothermal, MJ_Geothermal),
    MJ_Hydro = if_else(is.na(MJ_Hydro), nat_MJ_Hydro, MJ_Hydro),
    MJ_Naturalgas = if_else(is.na(MJ_Naturalgas), nat_MJ_Naturalgas, MJ_Naturalgas),
    MJ_Otherenergy = if_else(is.na(MJ_Otherenergy), nat_MJ_Otherenergy, MJ_Otherenergy),
    MJ_Solar = if_else(is.na(MJ_Solar), nat_MJ_Solar, MJ_Solar),
    MJ_Uranium = if_else(is.na(MJ_Uranium), nat_MJ_Uranium, MJ_Uranium),
    MJ_Wind = if_else(is.na(MJ_Wind), nat_MJ_Wind, MJ_Wind),
    kg_Coal = if_else(is.na(kg_Coal), nat_kg_Coal, kg_Coal),
    kg_Crudeoil = if_else(is.na(kg_Crudeoil), nat_kg_Crudeoil, kg_Crudeoil),
    kg_Naturalgas = if_else(is.na(kg_Naturalgas), nat_kg_Naturalgas, kg_Naturalgas),
    kg_Otherenergy = if_else(is.na(kg_Otherenergy), nat_kg_Otherenergy, kg_Otherenergy),
    kg_Uranium = if_else(is.na(kg_Uranium), nat_kg_Uranium, kg_Uranium),
    kg_CO2 = if_else(is.na(kg_CO2), nat_kg_CO2, kg_CO2),
    kg_CH4 = if_else(is.na(kg_CH4), nat_kg_CH4, kg_CH4),
    kg_N2O = if_else(is.na(kg_N2O), nat_kg_N2O, kg_N2O),
    kg_PFC116 = if_else(is.na(kg_PFC116), nat_kg_PFC116, kg_PFC116),
    kg_PFC14 = if_else(is.na(kg_PFC14), nat_kg_PFC14, kg_PFC14),
    kg_SF6 = if_else(is.na(kg_SF6), nat_kg_SF6, kg_SF6)
  ) %>%
  dplyr::select(
    -nat_kgCO2eq,
    -nat_MJ,
    -nat_MJ_nonRenewable,
    -nat_kgSO2eq,
    -nat_kgCFC11eq,
    -nat_kgPM2.5eq,
    -nat_kgO3eq,
    -nat_MJ_Biomass,
    -nat_MJ_Coal,
    -nat_MJ_Crudeoil,
    -nat_MJ_Geothermal,
    -nat_MJ_Hydro,
    -nat_MJ_Naturalgas,
    -nat_MJ_Otherenergy,
    -nat_MJ_Solar,
    -nat_MJ_Uranium,
    -nat_kg_Coal,
    -nat_kg_Crudeoil,
    -nat_kg_Naturalgas,
    -nat_kg_Otherenergy,
    -nat_kg_Uranium,
    -nat_MJ_Wind,
    -nat_kg_CO2,
    -nat_kg_CH4,
    -nat_kg_N2O,
    -nat_kg_PFC116,
    -nat_kg_PFC14,
    -nat_kg_SF6
  )

# ALL assigned
df_impacts %>% filter(is.na(kgCO2eq))


# CO2e by year and county --------
# Aggregate
head(df_impacts)
names(df_impacts)
# big table, a lot of Memory ram involved
electricity <- df_impacts %>%
  mutate(across(
    -c(
      State,
      NAMELSAD,
      STATEFP,
      COUNTYFP,
      NAME,
      pop,
      Region_EMM,
      Region_Electricity_EIA,
      Region_ecoinvent_detail,
      Region_ecoinvent,
      Region_Transport,
      scenario,
      period,
      fuel,
      share,
      sheet
    ),
    ~ .x * share
  )) %>%
  group_by(scenario, State, STATEFP, COUNTYFP, NAME, period) %>%
  reframe(
    pop = mean(pop),
    # share=sum(share), # to debug
    across(
      -c(
        NAMELSAD,
        pop,
        Region_EMM,
        sheet,
        share,
        Region_Electricity_EIA,
        fuel,
        Region_ecoinvent_detail,
        Region_ecoinvent,
        Region_Transport,
      ),
      ~ sum(.x)
    )
  ) %>%
  ungroup()

# T&D losses - 5%
td_loss <- tibble(period = 2022:2050) %>%
  mutate(
    losses = case_when(
      # period < 2030 ~ 0.08 - 0.04 / 8 * (period - 2022),
      T ~ 0.05
    )
  )

electricity <- electricity %>%
  left_join(td_loss) %>%
  mutate(across(-c(scenario, State, STATEFP, COUNTYFP, NAME, period, pop), ~ .x / (1 - losses))) %>%
  mutate(losses = NULL)

write.csv(electricity, "Parameters/Electricity/countyElectricityImpacts.csv", row.names = F)


# at state
electricity_state <- electricity %>%
  filter(scenario == "ref2025") %>%
  # filter(scenario=="Cambium") %>%
  group_by(scenario, State, period) %>%
  reframe(kgCO2eq = weighted.mean(kgCO2eq, pop)) %>%
  ungroup()


state_highlight <- c("California", "Texas", "Florida", "Michigan", "Massachusetts", "New York", "Minnesota")

state_colors <- c("#FF5733", "#8B4513", "#FFA500", "#4682B4", "#800080", "#00008B", "#228B22", "#808080")
names(state_colors) <- c(state_highlight, "Others")

electricity_state <- electricity_state %>% mutate(state_color = if_else(State %in% state_highlight, State, "Others"))

# figure
ggplot(electricity_state, aes(period, kgCO2eq, group = State, col = state_color)) +
  geom_line(alpha = .7) +
  geom_text_repel(
    data = filter(electricity_state, period == 2050),
    size = 5 * 5 / 14 * 0.8,
    alpha = .5,
    max.overlaps = 30,
    aes(label = State),
    nudge_x = 3
  ) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050)) +
  scale_color_manual(values = state_colors) +
  coord_cartesian(expand = F, ylim = c(0, 0.95)) +
  labs(x = "", y = "", title = "Average kg CO2e per kWh", fill = "") +
  theme_bw(7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")

ggsave(
  "Figures/Electricity/Electricity_CO2.png",
  ggplot2::last_plot(),
  units = "cm",
  dpi = 600,
  width = 8.7 * 2.2,
  height = 12
)

## map figure ------

# shp_state <-  vect(paste0(url_file,"/Shapefiles/tl_2024_us_state/tl_2024_us_state.shp"))
# terra::plot(shp_state)
# as.data.frame(shp_state) %>% head()
# shp_state$NAME %>% unique()

data_fig <- electricity %>%
  filter(period %in% c(2025, 2050)) %>%
  filter(scenario == "ref2025") %>%
  mutate(kgCO2eq = kgCO2eq * 1000) %>% # kg per MWh
  mutate(STATEFP = paste0(if_else(str_length(STATEFP) == 1, "0", ""), STATEFP)) %>%
  mutate(
    COUNTYFP = paste0(case_when(str_length(COUNTYFP) == 1 ~ "00", str_length(COUNTYFP) == 2 ~ "0", T ~ ""), COUNTYFP)
  )

# county map from tigris library
library(tigris)
tigcounties <- counties()
tigcounties <- tigcounties[as.numeric(tigcounties$STATEFP) < 60, ]

# add emissions data
# unique(shp$STATEFP)
# unique(shp$COUNTYFP)
tigcounties <- merge(tigcounties, data_fig, by = c("STATEFP", "COUNTYFP"), all.x = TRUE)
tigcounties <- subset(tigcounties, !is.na(tigcounties$kgCO2eq))

# put HI and AK at the bottom
# https://stackoverflow.com/questions/13757771/relocating-alaska-and-hawaii-on-thematic-map-of-the-usa-with-ggplot2
geo_shifted <- shift_geometry(tigcounties, position = "below", preserve_area = FALSE)

map <- ggplot(geo_shifted) +
  geom_sf(aes(fill = kgCO2eq), color = "black", linewidth = 0.05) +
  facet_wrap(~period) +
  scale_fill_gradientn(colors = c("tan", "darkred")) +
  # coord_sf(xlim = c(-125, -66), ylim = c(24, 50)) +  # Continental US bounds
  labs(fill = expression("Grid Intensity [kg CO"["2"] * "e per MWh]")) +
  theme_void() +
  theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0)) +
  guides(fill = guide_colorbar(barwidth = 50))
map

# histogram to show legend
range(geo_shifted$kgCO2eq)
hist <- ggplot(geo_shifted, aes(kgCO2eq, fill = after_stat(x))) +
  geom_histogram(col = "white", binwidth = 50, boundary = 0) +
  facet_wrap(~period, ncol = 2, scales = "free_x") +
  scale_x_continuous(limits = c(0, 1.05e3)) +
  scale_fill_gradientn(colors = c("tan", "darkred")) +
  coord_cartesian(expand = F) +
  labs(x = expression("Grid Intensity [kg CO"["2"] * "e per MWh]"), y = "") +
  theme_bw(10) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    # strip.text = element_text(face = "bold"),
    strip.text = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black"),
    strip.background = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    panel.spacing = unit(3, "cm"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )
hist

library(cowplot)
# combine them
ggdraw() + draw_plot(hist, 0, 0.1, 0.9, 0.2) + draw_plot(map, 0, 0.05, 1, 0.95)

ggsave(
  "Figures/Electricity/Electricity_CO2_map.png",
  ggplot2::last_plot(),
  units = "cm",
  dpi = 600,
  width = 8.7 * 2.2,
  height = 12
)

# For all scenarios - TAKES A LOT OF TIME
scens <- read.csv("Inputs/Join_EIAScenarios.csv")
for (i in unique(df$scenario)) {
  aux_co2 <- electricity %>%
    filter(period %in% c(2025, 2050)) %>%
    filter(scenario == i) %>%
    mutate(kgCO2eq = kgCO2eq * 1000) %>%
    mutate(STATEFP = paste0(if_else(str_length(STATEFP) == 1, "0", ""), STATEFP)) %>%
    mutate(
      COUNTYFP = paste0(case_when(str_length(COUNTYFP) == 1 ~ "00", str_length(COUNTYFP) == 2 ~ "0", T ~ ""), COUNTYFP)
    )

  new_data <- geo_shifted %>% mutate(kgCO2eq = NULL) %>% left_join(aux_co2, by = c("State", "COUNTYFP", "period"))

  aux <- scens %>% filter(scenario == i) %>% pull(scenarioDescription)

  p3 <- map + ggtitle(aux)
  p3$data <- new_data
  p_hist <- hist
  p_hist$data <- new_data

  ggdraw() + draw_plot(p_hist, 0, 0.1, 0.9, 0.2) + draw_plot(p3, 0, 0.05, 1, 0.95)

  ggsave(
    paste0("Figures/Electricity/EIA_Scenarios/", i, ".png"),
    ggplot2::last_plot(),
    units = "cm",
    dpi = 600,
    width = 8.7 * 2.2,
    height = 12
  )
}
rm(i)


# Figure of MIX by time -----

head(mix)
# colors
source("Scripts/01-CommonVariables.R", encoding = "UTF-8")
cat_colors <- names(fuel_colors)
mix <- mix %>% mutate(fuel = factor(fuel, levels = cat_colors))

mix %>%
  filter(scenario == "ref2025") %>%
  group_by(regionName, period) %>%
  filter(share > 0) %>%
  reframe(x = sum(share)) %>%
  arrange(desc(x))

# order 5 rows by 6 columns - orderd by position in Map: https://www.eia.gov/outlooks/aeo/pdf/nerc_map.pdf
regions_order <- c(
  # row 1
  "ASCC Miscellaneous",
  "ASCC Alaska Grid",
  "Midcontinent / West",
  "Midcontinent / East",
  "NPCC / Upstate New York",
  "NPCC / New England",
  # row 2
  "WECC / Northwest",
  "SPP / North",
  "PJM / Commonwealth Edison",
  "PJM / West",
  "PJM / East",
  "NPCC / New York City & Long Island",
  # row 3
  "WECC / Basin",
  "WECC / Rockies",
  "SPP / Central",
  "Midcontinent / Central",
  "SERC / Central",
  "PJM / Dominion",
  # row 4
  "WECC / California North",
  "WECC / California South",
  "WECC / Southwest",
  "SPP / South",
  "Midcontinent / South",
  "SERC / East",
  # row 5
  "HICC Oahu",
  "HICC Miscellaneous",
  "Texas Reliability Entity",
  "SERC / Southeastern",
  "Florida RCC",
  "United States (avg.)"
)


mix %>%
  # filter(regionName!="United States") %>%
  filter(period > 2024) %>%
  filter(scenario == "ref2025") %>%
  mutate(x = regionName) %>%
  mutate(
    regionName = regionName %>%
      str_replace("Northeast Power Coordinating Council", "NPCC") %>%
      str_replace("Western Electricity Coordinating Council", "WECC") %>%
      str_replace(" Reliability Corporation", "") %>%
      str_replace("Southwest Power Pool", "SPP") %>%
      str_replace("Reliability Coordinating Council", "RCC") %>%
      str_replace(" Power Pool Area", "") %>%
      str_replace("and Long", "& Long") %>%
      str_replace("United States", "United States (avg.)")
  ) %>%
  mutate(regionName = factor(regionName, levels = regions_order)) %>%
  complete(regionName, period, fuel, fill = list(share = 0)) %>%
  ggplot(aes(period, share)) +
  # geom_area(aes(fill=fuel,group=fuel))+ # weird spikes for some reasons
  geom_col(aes(fill = fuel, group = fuel), width = 1) +
  facet_wrap(~regionName) +
  coord_cartesian(expand = F, ylim = c(0, 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(2030, 2040, 2050)) +
  scale_fill_manual(values = fuel_colors) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "", y = "", title = "Share of Electricity Generation [%]", fill = "") +
  theme_bw(7) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 5),
    legend.position = "bottom",
    legend.text = element_text(size = 6),
    legend.key.height = unit(0.25, 'cm'),
    legend.key.width = unit(0.25, 'cm'),
    legend.background = element_rect(fill = "transparent", color = NA)
  )

ggsave("Figures/Electricity/Mix.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18.5, height = 9.7)

# Material Impacts ------
impacts_material <- read.csv("Parameters/Electricity/ecoinvent_electricity_material.csv")

impacts_material <- impacts_material %>%
  filter(str_detect(Name, "electricity production|waste incineration")) %>%
  mutate(Name = str_remove(Name, "electricity production, "))
unique(impacts_material$Name)
unique(df$fuel)
join_fuel <- read.csv("Inputs/join_fuels.csv")
impacts_material <- impacts_material %>%
  rename(ecoinvent_fuel = Name) %>%
  left_join(join_fuel) %>%
  rename(fuel = EIA_Fuel)

# filter fuels not used
impacts_material <- impacts_material %>% filter(!is.na(fuel))

# national avg
impacts_nat_material <- impacts_material %>%
  group_by(fuel) %>%
  reframe(across(starts_with("kg"), ~ mean(.x))) %>%
  ungroup()
range(impacts_material$kgMat)
range(impacts_nat_material$kgMat)

# Join to census
unique(census_join$Region_ecoinvent)
unique(impacts_material$Region)

df_impacts_mat <- df %>% left_join(impacts_material, by = c("Region_ecoinvent" = "Region", "fuel"))
names(df_impacts_mat)
df_impacts_mat %>% filter(is.na(kgMat)) %>% group_by(scenario, Region_ecoinvent, fuel) %>% tally()

# unmatch could be due to regional part, so add national averages
df_impacts_mat$ecoinvent_fuel <- NULL

names(impacts_nat_material)[-1] <- paste0("nat_", names(impacts_nat_material)[-1])

df_impacts_mat <- df_impacts_mat %>%
  left_join(impacts_nat_material) %>%
  mutate(
    kgMat = if_else(is.na(kgMat), nat_kgMat, kgMat),
    kgMetal = if_else(is.na(kgMetal), nat_kgMetal, kgMetal),
    kgAluminium = if_else(is.na(kgAluminium), nat_kgAluminium, kgAluminium),
    kgCobalt = if_else(is.na(kgCobalt), nat_kgCobalt, kgCobalt),
    kgCopper = if_else(is.na(kgCopper), nat_kgCopper, kgCopper),
    kgLithium = if_else(is.na(kgLithium), nat_kgLithium, kgLithium),
    kgNickel = if_else(is.na(kgNickel), nat_kgNickel, kgNickel),
    kgREE = if_else(is.na(kgREE), nat_kgREE, kgREE),
    kgLead = if_else(is.na(kgLead), nat_kgLead, kgLead),
    kgGold = if_else(is.na(kgGold), nat_kgGold, kgGold),
    kgBarium = if_else(is.na(kgBarium), nat_kgBarium, kgBarium),
    kgCalcium = if_else(is.na(kgCalcium), nat_kgCalcium, kgCalcium),
    kgChromium = if_else(is.na(kgChromium), nat_kgChromium, kgChromium),
    kgIron = if_else(is.na(kgIron), nat_kgIron, kgIron),
    kgMagnesium = if_else(is.na(kgMagnesium), nat_kgMagnesium, kgMagnesium),
    kgManganese = if_else(is.na(kgManganese), nat_kgManganese, kgManganese),
    kgPhosphorus = if_else(is.na(kgPhosphorus), nat_kgPhosphorus, kgPhosphorus),
    kgSilicon = if_else(is.na(kgSilicon), nat_kgSilicon, kgSilicon),
    kgSulphur = if_else(is.na(kgSulphur), nat_kgSulphur, kgSulphur),
    kgZinc = if_else(is.na(kgZinc), nat_kgZinc, kgZinc)
  ) %>%
  dplyr::select(
    -nat_kgMat,
    -nat_kgMetal,
    -nat_kgAluminium,
    -nat_kgCobalt,
    -nat_kgCopper,
    -nat_kgLithium,
    -nat_kgNickel,
    -nat_kgREE,
    -nat_kgZinc,
    -nat_kgBarium,
    -nat_kgCalcium,
    -nat_kgChromium,
    -nat_kgIron,
    -nat_kgMagnesium,
    -nat_kgManganese,
    -nat_kgPhosphorus,
    -nat_kgSilicon,
    -nat_kgSulphur
  )

# ALL assigned
df_impacts_mat %>% filter(is.na(kgMat))


# Aggregate
names(df_impacts_mat)
electricity_mat <- df_impacts_mat %>%
  mutate(across(starts_with("kg"), ~ .x * share)) %>%
  group_by(scenario, State, STATEFP, COUNTYFP, NAME, period) %>%
  reframe(
    pop = mean(pop),
    # share=sum(share), # to debug
    across(starts_with("kg"), ~ sum(.x))
  ) %>%
  ungroup()

# range(electricity_mat$share)

# T&D losses - 5%
td_loss <- tibble(period = 2022:2050) %>%
  mutate(
    losses = case_when(
      # period < 2030 ~ 0.08 - 0.04 / 8 * (period - 2022),
      T ~ 0.05
    )
  )

electricity_mat <- electricity_mat %>%
  left_join(td_loss) %>%
  mutate(across(starts_with("kg"), ~ .x / (1 - losses)), losses = NULL)

write.csv(electricity_mat, "Parameters/Electricity/countyElectricityImpactsMaterial.csv", row.names = F)


#####
# OLD -----
#####

# Fossil Fuel per MWh -----------
fossil <- read.csv("Parameters/Electricity/ecoinvent_fossil_electricity.csv")
fossil <- fossil %>%
  filter(str_detect(Name, "electricity production|waste incineration")) %>%
  mutate(Name = str_remove(Name, "electricity production, "))
unique(fossil$Name)

df2 <- census_join %>% left_join(mix, by = c("Region_Electricity_EIA" = "regionName"))
unique(df2$fuel)
join_fuel <- read.csv("Inputs/join_fuels.csv")
fossil <- fossil %>% rename(ecoinvent_fuel = Name) %>% left_join(join_fuel) %>% rename(fuel = EIA_Fuel)

# filter fuels not used
fossil <- fossil %>% filter(!is.na(fuel))
unique(fossil$fuel)

# national avg
fossil_nat <- fossil %>% group_by(fuel, fossilFuel, unit) %>% reframe(value = mean(value)) %>% ungroup()

# Join to census
unique(census_join$Region_ecoinvent)
unique(fossil$Region)

df2 <- df2 %>% left_join(fossil, by = c("Region_ecoinvent" = "Region", "fuel"))
names(df2)
df2 %>% filter(is.na(value)) %>% group_by(scenario, Region_ecoinvent, fuel) %>% tally()

# unmatch could be due to regional part, so add national averages
df2$ecoinvent_fuel <- NULL
df_na <- df2 %>% filter(is.na(value)) %>% dplyr::select(-sheet, -value, -fossilFuel, -unit)
df_na <- df_na %>% left_join(fossil_nat) %>% mutate(sheet = "National")
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
  mutate(value = value * share) %>%
  group_by(scenario, State, STATEFP, COUNTYFP, NAME, period, fossilFuel, unit) %>%
  reframe(pop = mean(pop), value = sum(value)) %>%
  ungroup()

# T&D losses - 5%
td_loss <- tibble(period = 2022:2050) %>%
  mutate(
    losses = case_when(
      # period < 2030 ~ 0.08 - 0.04 / 8 * (period - 2022),
      T ~ 0.05
    )
  )

electricity2 <- electricity2 %>% left_join(td_loss) %>% mutate(value = value / (1 - losses), losses = NULL)

write.csv(electricity2, "Parameters/Electricity/countyElectricityFossilFuel.csv", row.names = F)
# electricity2 <- read.csv("Parameters/Electricity/countyElectricityCarbon.csv")

# at state
electricity_state <- electricity2 %>%
  filter(scenario == "ref2025") %>%
  mutate(fossilFuel = paste0(fossilFuel, " (", unit, ")")) %>%
  group_by(State, period, fossilFuel) %>%
  reframe(value = weighted.mean(value, pop)) %>%
  ungroup()


state_highlight <- c("California", "Texas", "Florida", "Michigan", "Massachusetts", "New York", "Minnesota")

state_colors <- c("#FF5733", "#8B4513", "#FFA500", "#4682B4", "#800080", "#00008B", "#228B22", "#808080")
names(state_colors) <- c(state_highlight, "Others")

electricity_state <- electricity_state %>% mutate(state_color = if_else(State %in% state_highlight, State, "Others"))

# figure
ggplot(electricity_state, aes(period, value, group = State, col = state_color)) +
  geom_line(alpha = .7) +
  geom_text_repel(
    data = filter(electricity_state, period == 2050),
    size = 5 * 5 / 14 * 0.8,
    alpha = .5,
    max.overlaps = 30,
    aes(label = State),
    nudge_x = 3
  ) +
  facet_wrap(~fossilFuel, scales = "free_y") +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050)) +
  scale_color_manual(values = state_colors) +
  # coord_cartesian(expand = F,ylim = c(0,0.9))+
  labs(x = "", y = "", title = "Average Fossil Fuel Resource consumption per kWh", fill = "") +
  theme_bw(7) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")

ggsave(
  "Figures/Electricity/Electricity_fossil.png",
  ggplot2::last_plot(),
  units = "cm",
  dpi = 600,
  width = 8.7 * 2.2,
  height = 12
)

# EoF
