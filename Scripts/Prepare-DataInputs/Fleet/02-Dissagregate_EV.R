# Dissagregate EVs at US level into state level based on 2023 registrations and population
# PBH March 2025
# https://afdc.energy.gov/data/10962

library(tidyverse)

# EV Sale 2023 ---------

# Sales 2023 = EV Registration 2023 - EV Registration 2022
# only BEVs included, for 2023
ev <- readxl::read_excel("Inputs/10962-ev-registration-counts-by-state_9-06-24.xlsx", sheet = "CompiledData")
names(ev)
# get new sales as difference in registration
ev <- ev %>%
  mutate(
    sales_2023 = Reg_2023 - Reg_2022,
    sales_2022 = Reg_2022 - Reg_2021,
    sales_2021 = Reg_2021 - Reg_2020,
    sales_2020 = Reg_2020 - Reg_2019,
    sales_2019 = Reg_2019 - Reg_2018,
    sales_2018 = Reg_2018 - Reg_2017,
    sales_2017 = Reg_2017 - Reg_2016,
    sales_2016 = Reg_2016
  )
sum(ev$sales_2023) / 1e6 # 1.1M


# historical US sales
historical_ev <- ev %>%
  dplyr::select(
    State,
    sales_2016,
    sales_2017,
    sales_2018,
    sales_2019,
    sales_2020,
    sales_2021,
    sales_2022,
    sales_2023
  ) %>%
  pivot_longer(c(-State), names_to = "period", values_to = "unit") %>%
  mutate(period = as.integer(str_remove(period, "sales_"))) %>%
  group_by(period) %>%
  reframe(sales = sum(unit)) %>%
  ungroup()
write.csv(historical_ev, "Parameters/historical_evSales.csv", row.names = F)

# get joins for region transport from EIA data
dict_reg <- read.csv("Inputs/Join_TransportEIA_State.csv")
ev <- ev %>% left_join(dict_reg, by = c("State" = "NAME"))

# get population by state, from census data (need to generate census file beforehand)
census <- read.csv("Parameters/census_joins.csv")
census <- census %>% group_by(State, Region_Transport) %>% reframe(pop = sum(pop))
sum(census$pop) / 1e6 # 334M

# add pop
ev <- ev %>% left_join(census)
sum(ev$pop) / 1e6

# calculate share of EV regs and share of pop by state within regions
ev_share <- ev %>%
  mutate(pop = pop / sum(pop), ev = sales_2023 / sum(sales_2023)) %>%
  ungroup() %>%
  mutate(sales_2023 = NULL)
sum(ev_share$ev)

# State code
st_code <- read.csv("Inputs/Join_StateCode.csv")

# Barplot
ev_share %>%
  left_join(st_code) %>%
  rename(Population = pop, `EV Registration` = ev) %>%
  pivot_longer(c(Population, `EV Registration`), names_to = "key", values_to = "value") %>%
  mutate(State_code = if_else(value > 0.02, State_code, "")) %>%
  ggplot(aes(key, value, fill = State)) +
  geom_col(col = "black", linewidth = 0.05) +
  geom_text(
    aes(label = State_code),
    position = position_stack(vjust = .5),
    size = 6 * 5 / 14 * 0.8
  ) +
  coord_cartesian(expand = F) +
  # ggforce::facet_col(facets = vars(Region_Transport),
  #                    scales = "free_y",
  #                    space = "free")+
  # scale_y_continuous(labels=scales::percent)+
  # scale_fill_viridis_d()+
  labs(x = "", y = "State share within EIA Transport Region") +
  theme_bw(8) +
  scale_fill_manual(values = scico::scico(51, palette = "batlow", direction = 1)) +
  theme(legend.position = "none", panel.grid = element_blank(), strip.text = element_text(size = 6))

ggsave("Figures/Fleet/EV_reg_share.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 8.7 * 1.5, height = 14)


# Dissaggregate based on goals and starting point ------

# BEV adoption rates according to ICCT ambitious 2045 100% BEV sales

# for USA: 100% by 2035
adoption <- read.csv("Inputs/ICCT_ambitious_evAdoption.csv")

# almost linear, just do linear interpolation
ggplot(adoption, aes(period, ev_sales_adoption)) + geom_line() + theme_bw(8)


# Simple model: linear interpolation between EV 2023 Sales Share and Population Share

df_share <- ev_share %>% dplyr::select(State, pop, ev) %>% mutate(linearStep = (pop - ev) / 12) # 2023 to 2035

df_share <- expand.grid(period = 2023:2050, State = unique(df_share$State)) %>%
  left_join(df_share) %>%
  mutate(
    ev_share = case_when(
      period < 2036 ~ ev + linearStep * (12 + period - 2035), # linear interpolation
      T ~ pop
    )
  ) %>% # use population
  dplyr::select(period, State, ev_share)

# add historical EV sales share by state
ev_share_historical <- ev %>%
  dplyr::select(State, sales_2016, sales_2017, sales_2018, sales_2019, sales_2020, sales_2021, sales_2022) %>%
  pivot_longer(c(-State), names_to = "period", values_to = "unit") %>%
  mutate(period = as.integer(str_remove(period, "sales_"))) %>%
  mutate(unit = if_else(unit > 0, unit, 0)) %>% # avoid negative cases for some states
  group_by(period) %>%
  mutate(ev_share = unit / sum(unit)) %>%
  ungroup() %>%
  dplyr::select(period, State, ev_share)

df_share <- rbind(ev_share_historical, df_share)

df_share %>% group_by(period) %>% reframe(x = sum(ev_share)) %>% arrange(x)

# Figure
labels <- df_share %>%
  left_join(st_code) %>%
  filter(period == 2050) %>%
  mutate(period = 2050.5) %>%
  mutate(label_st = if_else(ev_share > 0.015, State_code, ""))

ggplot(df_share, aes(period, ev_share, fill = State)) +
  geom_area(col = "black", linewidth = 0.05) +
  geom_text(
    data = labels,
    aes(label = label_st),
    size = 6 * 5 / 14 * 0.8,
    position = position_stack(vjust = .5)
  ) +
  geom_vline(xintercept = 2022, linetype = "dashed", linewidth = 0.2) +
  annotate("text", x = 2021.5, y = 0.1, label = "Historical", angle = 90, size = 8 * 5 / 14 * 0.8) +
  coord_cartesian(expand = F, xlim = c(2016, 2051)) +
  scale_y_continuous(
    labels = scales::percent,
    sec.axis = sec_axis(~., labels = scales::percent, name = "Based on Population")
  ) +
  labs(x = "", y = "Based on EV Registration Records", fill = "Region") +
  # scale_fill_viridis_d()+scale_color_viridis_d()+
  scale_fill_manual(values = scico::scico(51, palette = "batlow", direction = 1)) +
  theme_bw(8) +
  theme(panel.grid = element_blank(), legend.position = "none")

ggsave(
  "Figures/Fleet/Sales_EV_dissagregated.png",
  ggplot2::last_plot(),
  units = "cm",
  dpi = 600,
  width = 8.7 * 2,
  height = 10
)

# Save share
df_share <- df_share %>% dplyr::select(period, State, ev_share)
write.csv(df_share, "Parameters/Operation/EV_share_state.csv", row.names = F)

# sales over time make sense?
# Only for USA and BEV Cars/Vans
sales <- read.csv("Parameters/Operation/SalesEV.csv")

sales <- sales %>% filter(Scenario == "Ambitious") %>% rename(period = Year)

# always positive and increasing
sales <- df_share %>% left_join(sales) %>% mutate(ev_sales = Sales * ev_share)

ggplot(sales, aes(period, ev_sales)) +
  geom_line(aes(col = Vehicle)) +
  facet_wrap(~State, scales = "free_y") +
  # geofacet::facet_geo(~State)+
  labs(x = "", y = "", title = "EV Sales, by state") +
  theme_bw(8)

# EoF
