# Materials requirements
## Analysis of baseline results
#  Barplots for materials
## PBH Sept 2025

# LOAD Results -------

source("Scripts/02-Load_Results.R")

# baseline scenario
df <- df_all %>%
  filter(
    Scenario_Sales == "Ambitious",
    Scenario_Lifetime == "Reference",
    Scenario_Capacity == "Reference",
    Scenario_mpg == "Reference",
    Scenario_Recycling %in% c("Recycling 0%", "Recycling 80%"),
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


# Figure -----

unique(df$impact)
dict <- read_excel("Inputs/Dict_Impacts.xlsx")
start_year <- 2025
range(df$Year)

total_df <- df %>%
  left_join(dict) %>%
  filter(!is.na(Category)) %>%
  group_by(Scenario_Recycling, Category, Impact_Name, Abbr, vehicle_type, Stage) %>%
  reframe(value = sum(value)) %>%
  ungroup() %>% #   mutate(Stage=factor(Stage,levels=stage_lvl)) %>%
  mutate(
    Impact_Name = Impact_Name %>%
      str_replace("Crudeoil", "Crude oil") %>%
      str_replace("Naturalgas", "Natural gas") %>%
      str_replace("Non metal", "Other Materials")
  ) %>%
  mutate(labX = paste0(Impact_Name, "-", vehicle_type))


# Simplified stages

# fix, for 80% LIB recycling add the baseline for LIB production and LIB reaplcemente
aux <- total_df |>
  filter(Scenario_Recycling == "Recycling 0%") |>
  filter(str_detect(Stage, "LIB prod|Replacement")) |>
  mutate(Scenario_Recycling = "Recycling 80%")
total_df <- rbind(total_df, aux)

stage_colors <- viridis::viridis(6, option = "E", direction = -1)
names(stage_colors) <- stage_lvl
total_df <- total_df %>%
  mutate(
    Stage = case_when(
      str_detect(Stage, "Recycling|Replacement") ~ "LIB production",
      str_detect(Stage, "Maintenance") ~ "Vehicle production",
      T ~ Stage
    )
  )
unique(total_df$Stage)

# a) Non-metal vs Metal vs Energy -----

data_fig_a <- total_df %>%
  filter(Scenario_Recycling == "Recycling 0%") |>
  filter(Category %in% c("Material consumption", "Energy material")) %>%
  mutate(Impact_Name = if_else(Category == "Energy material", "Energy", Impact_Name)) %>%
  group_by(vehicle_type, Impact_Name, Stage) %>%
  reframe(value = sum(value) / 1e9) %>%
  ungroup() # million tons

# create non metal category
data_fig_a <- data_fig_a %>%
  pivot_wider(names_from = Impact_Name, values_from = value) %>%
  mutate(`Non-Metal` = `Other Materials` - Metal) %>%
  mutate(`Other Materials` = NULL) %>%
  pivot_longer(c(Metal, `Non-Metal`, Energy), names_to = "Impact_Name", values_to = "value") %>%
  mutate(Abbr = Impact_Name) %>%
  mutate(vehicle_type = factor(vehicle_type)) %>%
  mutate(abb_lab = if_else(abs(value) > 10, Abbr, ""))

# b) Metals ------

metals <- read_excel("Inputs/Elements_Metals.xlsx")
# not include silicon here, as it is not a metal
metals <- metals %>% filter(Metal == "Metal") %>% pull(Material) %>% c("REE")

data_fig_b <- total_df %>%
  filter(Scenario_Recycling == "Recycling 0%") |>
  filter(Impact_Name %in% metals) %>%
  group_by(vehicle_type, Impact_Name, Abbr, Stage) %>%
  reframe(value = sum(value) / 1e9) %>%
  ungroup() %>% # million tons
  mutate(vehicle_type = factor(vehicle_type)) %>%
  mutate(abb_lab = if_else(abs(value) > 10, Abbr, ""))


# c) Critical Minerals ------

# USGS 2025 report for critical minerals: https://pubs.usgs.gov/of/2025/1047/ofr20251047.pdf
# fmt: skip
cm <- c("Aluminium", "Barium", "Chromium" ,"Cobalt", "Copper", "Lead", "Lithium", "Magnesium", "Manganese", "Nickel","Silicon", "REE","Zinc")

# fmt: skip
cm_usgs <- c("Samarium","Rhodium","Lutetium","Terbium","Dysprosium","Gallium","Germanium","Gadolinium","Tungsten","Niobium","Magnesium","Yttrium","Potash",
            "Hafnium","Aluminum","Thulium","Neodymium","Silicon","Antimony","Barite","Graphite","Indium","Vanadium","Palladium","Manganese","Lanthanum",
            "Praseodymium","Titanium","Copper","Platinum","Ruthenium","Zinc","Iridium","Cobalt","Erbium","Chromium","Silver","Tin","Bismuth","Nickel",
            "Tantalum","Holmium","Fluorspar","Lithium","Rhenium","Cerium","Lead","Beryllium","Europium","Ytterbium","Zirconium","Cesium","Rubidium","Scandium")

data_fig_c <- total_df %>%
  filter(Scenario_Recycling == "Recycling 0%") |>
  filter(Impact_Name %in% cm) %>%
  group_by(vehicle_type, Impact_Name, Abbr, Stage) %>%
  reframe(value = sum(value) / 1e9) %>%
  ungroup() %>% # million tons
  mutate(vehicle_type = factor(vehicle_type)) %>%
  mutate(abb_lab = if_else(abs(value) > 10, Abbr, ""))

# d) Energy Extraction ------

data_fig_d <- total_df %>%
  filter(Scenario_Recycling == "Recycling 0%") |>
  filter(Category == "Energy material") %>%
  group_by(vehicle_type, Impact_Name, Abbr, Stage) %>%
  reframe(value = sum(value) / 1e9) %>%
  ungroup() %>% # million tons
  mutate(vehicle_type = factor(vehicle_type)) %>%
  mutate(abb_lab = if_else(abs(value) > 10, Abbr, ""))

# e) Critical minerals high recycling -----
data_fig_e <- total_df %>%
  filter(
    (Scenario_Recycling == "Recycling 80%" & Stage == "LIB production") |
      (Stage != "LIB production" & Scenario_Recycling == "Recycling 0%")
  ) |>
  filter(Impact_Name %in% cm) %>%
  group_by(vehicle_type, Impact_Name, Abbr, Stage) %>%
  reframe(value = sum(value) / 1e9) %>%
  ungroup() %>% # million tons
  mutate(vehicle_type = factor(vehicle_type)) %>%
  mutate(abb_lab = if_else(abs(value) > 10, Abbr, ""))

# Combined -----

mat_colors <- c(
  "Aluminium" = "#134D61",
  "Barium" = "#3B6C55",
  "Chromium" = "#818231",
  "Cobalt" = "#3B5BA5CC",
  "Copper" = "#D9883DCC",
  "Lead" = "#0E375E",
  "Lithium" = "#76B7B2CC",
  "Magnesium" = "#D29243",
  "Manganese" = "#F19D6B",
  "Nickel" = "#59A14FCC",
  "Silicon" = "#FDAC9C",
  "REE" = "#AF7AA1CC",
  "Zinc" = "#215F61",
  "Gold" = "#001959",
  "Calcium" = "#5D7843",
  "Iron" = "#AA8C2C",
  "Phosphorus" = "#FCBBCA",
  "Sulphur" = "#F9CCF9",
  "Non-Metal" = "#4E79A7CC",
  "Metal" = "#E15759CC",
  "Energy" = "#BA8E23",
  "Coal" = "#8c564b",
  "Oil" = "#9467bd",
  "Natural gas" = "#CD7F32",
  "Uranium" = "#2ca02c",
  "Other" = "#c5b0d5"
) # Fuel colors (copy-pasted)

mat_levels <- names(mat_colors)

comb_lvl2 <- expand.grid(stage_lvl, mat_levels) %>% mutate(x = paste0(Var1, Var2)) %>% pull(x)

data_fig <- rbind(
  mutate(data_fig_a, key = "Material"),
  mutate(data_fig_b, key = "Metal"),
  mutate(data_fig_c, key = "Critical minerals"),
  mutate(data_fig_d, key = "Fossil energy"),
  mutate(data_fig_e, key = "80% Recycling")
) %>%
  mutate(lvl = factor(paste0(Stage, Impact_Name), levels = comb_lvl2)) %>%
  mutate(key = factor(key, levels = c("Material", "Metal", "Critical minerals", "Fossil energy", "80% Recycling"))) %>%
  mutate(Impact_Name = factor(Impact_Name, levels = rev(mat_levels))) %>%
  mutate(Stage = factor(Stage, levels = rev(stage_lvl))) %>%
  arrange(lvl)

total_fig <- data_fig %>%
  group_by(key, vehicle_type, Impact_Name, Abbr) %>%
  reframe(value = sum(value)) %>%
  ungroup() %>% # million ton
  group_by(key) %>%
  mutate(x = value / sum(value)) %>%
  mutate(abb_lab = if_else(value / sum(value) > 0.03, Abbr, "")) %>%
  ungroup() %>%
  mutate(abb_lab2 = if_else(value / sum(value) < 0.01, Abbr, "")) %>%
  mutate(col_text = if_else(Abbr %in% c("Al", "Ba"), "special", "normal")) %>%
  mutate(Impact_Name = factor(Impact_Name, levels = (mat_levels)))


stage_text <- data_fig |>
  filter(
    (key == "Material" & vehicle_type == "EV" & abb_lab == "Energy")
    # (key == "Metal" & vehicle_type == "EV" & abb_lab == "Fe") |
    # (key == "Fossil energy" & vehicle_type == "EV" & abb_lab == "Oil")
  ) %>%
  # mutate(col_text = if_else(Stage == "LIB production", "special", "normal")) |>
  arrange(Stage)

# Save Figure Data
write.csv(total_fig, "Results/Data Figures/Fig3.csv", row.names = FALSE)

f.makePlot <- function(filter_cond, special = F) {
  total_fig_plot <- total_fig |> filter(!!rlang::enquo(filter_cond))
  data_fig_plot <- data_fig |> filter(!!rlang::enquo(filter_cond))
  stage_text_plot <- stage_text |> filter(!!rlang::enquo(filter_cond))

  if (special) {
    # Legend for Stage
    p <- ggplot(total_fig_plot, aes(as.numeric(vehicle_type) - 0.2, value)) +
      geom_col(data = data_fig_plot,aes(fill = Stage, group = lvl),
        position = position_stack(),col = "black",
        linewidth = 0.1,width = 0.3,alpha = .7) +
      geom_text(data = stage_text_plot,position = position_stack(vjust = 0.5),
        size = 7 * 5 / 14 * 0.8,
        angle=c(90,90,90),
        aes(x = as.numeric(vehicle_type) - 0.2, label = Stage)) +
      scale_fill_manual(values = stage_colors, labels = c("Vehicle\nproduction", "LIB\nproduction", "Driving")) +
      guides(fill = guide_legend(reverse = TRUE, ncol = 1), color = "none") +
      ggnewscale::new_scale_fill() # trick to avoid a Stage fill legend
  } else {
    p <- ggplot(total_fig_plot, aes(as.numeric(vehicle_type) - 0.2, value)) +
      geom_col(data = data_fig_plot,aes(fill = Stage, group = lvl),
        position = position_stack(),col = "black",
        linewidth = 0.1,width = 0.3,alpha = .7) +
      geom_text(data = stage_text_plot,position = position_stack(vjust = 0.5),
        size = 7 * 5 / 14 * 0.8,
        angle=c(90,90,90),
        aes(x = as.numeric(vehicle_type) - 0.2, label = Stage))
  }
  p <- p +
    geom_col(aes(as.numeric(vehicle_type) + 0.2, fill = Impact_Name),
      position = position_stack(),col = "black",linewidth = 0.1,width = 0.5) +
    geom_text(position = position_stack(vjust = 0.5),size = 7 * 5 / 14 * 0.8,
      aes(x = as.numeric(vehicle_type) + 0.2,label = abb_lab,col = col_text,group = Impact_Name)) +
    # facet_wrap(~key, scales = "free") +
    scale_y_continuous(
      labels = function(x) format(x, big.mark = " ", scientific = FALSE),
      expand = expansion(mult = c(0, 0.05)),
      sec.axis = sec_axis(~ . * 1e9 / vmt_total$vmt / 1.61, name = "kg per km")
    ) +
    scale_fill_manual(values = c(stage_colors, mat_colors), breaks = names(mat_colors)[14:18]) +
    scale_color_manual(values = c("special" = "white", "normal" = "black")) +
    scale_x_continuous(breaks = 1:2, labels = c("EV", "ICEV")) +
    labs(x = "", y = "Million tons 2025-2050 (whole fleet)", fill = "") +
    guides(fill = guide_legend(reverse = T, nrow = 2, keywidth = 0.5, keyheight = 0.5), color = "none") +
    theme_bw(10) +
    theme(
      panel.grid = element_blank(),
      strip.background = element_blank(),
      legend.text = element_text(size = 5),
      legend.title = element_text(size = 6),
      legend.key.size = unit(0.2, "cm"),
      legend.background = element_rect(color = "black", size = 0.2),
      legend.key = element_rect(color = "black"),
      axis.title.y.right = element_text(face = "italic"),
      plot.tag = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right"
    )
}


library(cowplot)
p1 <- f.makePlot(key == "Material", T) +
  ggtitle("Material") +
  scale_fill_manual(values = c(stage_colors, mat_colors), breaks = names(mat_colors)[19:21], name = NULL) +
  guides(fill = guide_legend(reverse = F, ncol = 1)) +
  theme(legend.position = c(0.25, 0.9))
p2 <- f.makePlot(key == "Metal") +
  ggtitle("Metal") +
  labs(fill = NULL) +
  guides(fill = guide_legend(reverse = F, ncol = 1)) +
  theme(legend.position = c(0.78, 0.8))
p3 <- f.makePlot(key == "Critical minerals") +
  ggtitle("Critical minerals") +
  coord_cartesian(ylim = c(0, 260)) +
  scale_fill_manual(values = c(stage_colors, mat_colors), breaks = names(mat_colors)[1:13], name = NULL) +
  guides(fill = guide_legend(reverse = F, ncol = 1)) +
  theme(legend.position = c(0.73, 0.78))
p4 <- f.makePlot(key == "Fossil energy") +
  ggtitle("Fossil energy") +
  scale_fill_manual(values = c(stage_colors, mat_colors), breaks = names(mat_colors)[22:26], name = NULL) +
  guides(fill = guide_legend(reverse = F, ncol = 1)) +
  theme(legend.position = c(0.25, 0.85))
p5 <- f.makePlot(key == "80% Recycling") +
  ggtitle("LIB 80% Recycling") +
  coord_cartesian(ylim = c(0, 260)) +
  guides(fill = guide_legend(reverse = F, ncol = 1)) +
  theme(legend.position = "none")

plot_grid(p1, plot_grid(p2, p3, p4, p5, ncol = 2), rel_widths = c(0.33, 0.67))

ggsave("Figures/Fig3.png", ggplot2::last_plot(), units = "cm", dpi = 600, width = 18, height = 8.7 * 2)

pdf("Figures/PDF/Figure3.pdf", width = 18 / 2.54, height = 8.7 * 2 / 2.54)
ggplot2::last_plot()
dev.off()

# Totals
data_fig |>
  group_by(key, vehicle_type) |>
  reframe(value = sum(value)) |>
  pivot_wider(names_from = vehicle_type, values_from = value) |>
  mutate(diff_ice = (ICE - EV) / EV) |>
  mutate(diff = (EV - ICE) / ICE) |>
  mutate(EV_km = EV * 1e9 / vmt_total$vmt / 1.61, ICE_km = ICE * 1e9 / vmt_total$vmt / 1.61) |>
  mutate(EV_km_per_kg = 1 / EV_km, ICE_km_per_kg = 1 / ICE_km)

.Last.value %>% write.table('clipboard', sep = '\t', row.names = FALSE)

# total energy - in million tons
data_fig_d |>
  group_by(vehicle_type, Impact_Name) |>
  reframe(value = sum(value)) |>
  pivot_wider(names_from = vehicle_type, values_from = value) |>
  mutate(diff = (EV - ICE) / ICE)
# NG density 0.8 kg/m3

# Critical minerals - million tons
data_fig |>
  filter(Impact_Name %in% c("Lithium", "Copper", "Nickel", "Cobalt")) |>
  group_by(key, Impact_Name, vehicle_type) |>
  reframe(value = sum(value)) |>
  pivot_wider(names_from = vehicle_type, values_from = value) |>
  mutate(diff_ice = (ICE - EV) / EV) |>
  mutate(diff = (EV - ICE) / ICE) |>
  mutate(EV_km = EV * 1e9 / vmt_total$vmt / 1.61, ICE_km = ICE * 1e9 / vmt_total$vmt / 1.61) |>
  mutate(EV_km_per_kg = 1 / EV_km, ICE_km_per_kg = 1 / ICE_km)

# kwh
kwh_total |>
  filter(Scenario_Sales == "Ambitious", Scenario_Lifetime == "Reference", Scenario_Capacity == "Reference") |>
  group_by(vehicle_type) |>
  reframe(twh = sum(kwh) / 1e9)

#
# library(grid)
#
# png("Figures/plot_with_line.png", width = 8.7*2/2.54 * 600,     height = 8.7/2.54 * 600,     res = 600)
# grid.draw(p)
# grid.lines(x = unit(c(0.17, 0.39), "npc"),
#            y = unit(c(0.76, 0.91), "npc"),
#            gp = gpar(col = "lightgray", lty = "dashed", lwd = 1))
#
# dev.off()

## Materials ------
# order

mat_levels <- c(
  "Aluminium",
  "Cobalt",
  "Copper",
  "Lithium",
  "Nickel",
  "REE",
  "Zinc",
  "Barium",
  "Calcium",
  "Chromium",
  "Iron",
  "Magnesium",
  "Manganese",
  "Phosphorus",
  "Silicon",
  "Sulphur",
  "Other Materials"
)
comb_lvl2 <- expand.grid(mat_levels, stage_lvl) %>% mutate(x = paste0(Var1, Var2)) %>% pull(x)

# fuel colors
mat_colors <- scico::scico(17, palette = "batlow", direction = 1)
names(mat_colors) <- mat_levels


data_fig4 <- total_df %>%
  # filter(Category %in% c("Material consumption detail","ETM")|Impact_Name=="Other Materials") %>%
  filter(Category %in% c("Material consumption detail", "ETM")) %>%
  mutate(value = value / 1e9) %>%
  mutate(lvl = factor(paste0(Impact_Name, Stage), levels = comb_lvl2)) %>%
  mutate(vehicle_type = factor(vehicle_type)) %>%
  arrange(lvl) %>%
  mutate(abb_lab = if_else(abs(value) > 10, Abbr, ""))


# minor effect
# substract from other materials
# subst <- data_fig4 %>%
#   filter(Impact_Name!="Other Materials") %>%
#   group_by(vehicle_type,Stage) %>% reframe(minus_mat=sum(value)) %>% ungroup()
#
# data_fig4 <- data_fig4 %>%
#   left_join(subst) %>%
#   mutate(value=value-if_else(Impact_Name=="Other Materials",minus_mat,0))

data_fig4a <- data_fig4 %>% group_by(vehicle_type, Stage) %>% reframe(value = sum(value)) %>% ungroup()

total_lab4 <- total_df %>%
  filter(Category %in% c("Material consumption detail", "ETM")) %>%
  group_by(Category, vehicle_type) %>%
  reframe(value = sum(value) / 1e9) %>%
  ungroup() %>%
  mutate(lab_total = paste0(round(value), " Mtons"))

p4 <- ggplot(data_fig4, aes(as.numeric(vehicle_type) + 0.15, value)) +
  geom_col(
    data = data_fig4a,
    aes(fill = Stage),
    position = position_stack(),
    col = "black",
    linewidth = 0.1,
    width = 0.3
  ) +
  # geom_text(data=total_lab4,aes(label=lab_total),size=8*5/14 * 0.8,
  #           nudge_y = 0)+
  # by energy
  geom_col(
    aes(x = as.numeric(vehicle_type) - 0.15, fill = Impact_Name, group = lvl),
    position = position_stack(),
    col = "black",
    linewidth = 0.1,
    width = 0.3
  ) +
  geom_text(
    position = position_stack(vjust = 0.5),
    size = 6 * 5 / 14 * 0.8,
    aes(x = as.numeric(vehicle_type) - 0.15, label = abb_lab, group = lvl)
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = " ", scientific = FALSE),
    sec.axis = sec_axis(~ . * 1e9 / vmt_total$vmt * 1.61, name = "kg per km (avg.)")
  ) +
  scale_fill_manual(values = c(stage_colors, mat_colors), breaks = names(mat_colors)) +
  scale_x_continuous(breaks = 1:2, labels = c("EV", "ICEV")) +
  labs(
    x = "",
    y = "Million tons 2025-2050 (whole fleet)",
    fill = "Material",
    tag = "(a)",
    title = "Material requirements"
  ) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 2)) +
  theme_bw(8) +
  theme(
    panel.grid = element_blank(),
    axis.title.y.right = element_text(face = "italic"),
    plot.tag = element_text(face = "bold"),
    legend.position = "bottom"
  )
p4


## ETMs -----
data_fig5 <- data_fig4 %>% filter(Category %in% c("ETM")) %>% mutate(abb_lab = if_else(abs(value) > 2, Abbr, ""))
data_fig5a <- data_fig5 %>% group_by(vehicle_type, Stage) %>% reframe(value = sum(value)) %>% ungroup()


p5 <- ggplot(data_fig5, aes(as.numeric(vehicle_type) + 0.15, value)) +
  geom_col(
    data = data_fig5a,
    aes(fill = Stage),
    position = position_stack(),
    col = "black",
    linewidth = 0.1,
    width = 0.3
  ) +
  # geom_text(data=total_lab4,aes(label=lab_total),size=8*5/14 * 0.8,
  #           nudge_y = 0)+
  # by energy
  geom_col(
    aes(x = as.numeric(vehicle_type) - 0.15, fill = Impact_Name, group = lvl),
    position = position_stack(),
    col = "black",
    linewidth = 0.1,
    width = 0.3
  ) +
  geom_text(
    position = position_stack(vjust = 0.5),
    size = 6 * 5 / 14 * 0.8,
    col = "white",
    aes(x = as.numeric(vehicle_type) - 0.15, label = abb_lab, group = lvl)
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = " ", scientific = FALSE),
    sec.axis = sec_axis(~ . * 1e9 / vmt_total$vmt * 1.61, name = "kg per km (avg.)")
  ) +
  scale_fill_manual(values = c(stage_colors, mat_colors), breaks = names(mat_colors)) +
  scale_x_continuous(breaks = 1:2, labels = c("EV", "ICEV")) +
  labs(
    x = "",
    y = "Million tons 2025-2050 (whole fleet)",
    fill = "Material",
    tag = "(b)",
    title = "Critical Minerals requirements"
  ) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
  theme_bw(8) +
  theme(
    panel.grid = element_blank(),
    axis.title.y.right = element_text(face = "italic"),
    plot.tag = element_text(face = "bold"),
    legend.position = "bottom"
  )
p5
