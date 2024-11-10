#fig1abc--------------------------
library(ggplot2)
library(dplyr)
library(readxl)

file_path43 <- "/.../bio_SDG_raw_total.xlsx"
bio_SDG_raw <- read_excel(file_path43, sheet = "bio_SDG_raw_总分更新版")

Global_biomass <- bio_SDG_raw %>%
  group_by(Biomass_type, scenario, Year) %>%
  summarise(
    SumHeatvalue_EJ = sum(SumHeatvalue_EJ),
    EnerHeatvalue_EJ = sum(EnerHeatvalue_EJ),
    .groups = 'drop' 
  )
write.csv(Global_biomass,"/.../globalbiomass.csv")

filtered_scenarios <- c("SSP126", "SSP245", "SSP445", "SSP560")
Global_biomass <- Global_biomass %>%
  filter(scenario %in% filtered_scenarios) %>%
  mutate(
    upper_bound = SumHeatvalue_EJ * 1.20,
    lower_bound = SumHeatvalue_EJ * 0.80
  )

custom_colors <- c(
  "SSP126" = "#336600",
  "SSP245" = "#FFCC00",
  "SSP445" = "#0066CC",
  "SSP560" = "#FF6666"
)
custom_labels <- c(
  "SSP126" = "SSP1",
  "SSP245" = "SSP2",
  "SSP445" = "SSP4",
  "SSP560" = "SSP5"
)

plot_biomass_bar <- function(data, biomass_type) {
  data_filtered <- data %>% filter(Biomass_type == biomass_type)
  p <- ggplot(data_filtered, aes(x = Year, y = SumHeatvalue_EJ, color = scenario, group = scenario)) +
    geom_line(size = 1.5,alpha = 1.2) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound, fill = scenario), alpha = 0.1,linetype = "dashed",size = 0.3) +
    labs(
         x = NULL,
         y = NULL,
         color = "Scenario",
         fill = "Scenario") +
    scale_color_manual(values = custom_colors, labels = custom_labels) +
    scale_fill_manual(values = custom_colors, labels = custom_labels) +
    scale_x_continuous(breaks = seq(2025, 2060, by = 5), limits = c(2025, 2060), expand = c(0, 0)) +
    theme_bw(base_size = 20)+
  theme(
    plot.title = element_blank(),  # 去掉图标题
    legend.title = element_blank(),  # 去掉图例标题
    axis.title.y = element_text(vjust = 1.5),  # 设置y轴标题的位置
    panel.grid.major = element_blank(),  # 去掉主要网格线
    panel.grid.minor = element_blank()  # 去掉次要网格线
  )
  return(p)
}

global_AR <- plot_biomass_bar(Global_biomass, "AR")
global_LR <- plot_biomass_bar(Global_biomass, "LR")
global_WR <- plot_biomass_bar(Global_biomass, "WR")


#fig1def-----------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggforce)
library(cowplot)
library(countrycode)

world <- ne_countries(scale = "medium", returnclass = "sf")

regions <- data.frame(
  region = c("Africa_Eastern", "Africa_Northern", "Africa_Southern", "Africa_Western", "Argentina", "Australia_NZ",
             "Brazil", "Canada", "Central America and Caribbean", "Central Asia", "China", "Colombia", "EU-12",
             "EU-15", "Europe_Eastern", "European Free Trade Association", "Europe_Non_EU", "India", "Indonesia",
             "Japan", "Mexico", "Middle East", "Pakistan", "Russia", "South Africa", "South America_Northern",
             "South America_Southern", "South Asia", "Southeast Asia", "South Korea", "Taiwan", "USA"),
  countries = c("Seychelles, Burundi, Comoros, Djibouti, Eritrea, Ethiopia, Kenya, Madagascar, Mauritius, Rwanda, Somalia, Uganda, S. Sudan, Sudan, Somaliland",
                "Western Sahara, Algeria, Egypt, Libya, Morocco, Tunisia",
                "Angola, Botswana, Lesotho, Mozambique, Malawi, Namibia, Eswatini, Tanzania, Zambia, Zimbabwe",
                "São Tomé and Principe, Benin, Burkina Faso, Central African Republic, Ivory Coast, Cameroon, Democratic Republic of the Congo, Republic of the Congo, Cape Verde, Gabon, Ghana, Gambia, Guinea, Guinea-Bissau, Equatorial Guinea, Liberia, Mali, Mauritania, Niger, Nigeria, Senegal, Sierra Leone, Chad, Togo",
                "Argentina",
                "Australia, New Zealand",
                "Brazil",
                "Canada",
                "El Salvador, Costa Rica, Guatemala, Honduras, Nicaragua, Panama, Aruba, Anguilla, Antigua and Barbuda, Bahamas, Barbados, Belize, Bermuda, Cayman Islands, Cuba, Curacao, Dominica, Dominican Republic, Grenada, Guadeloupe, Haiti, Jamaica, Martinique, Montserrat, Puerto Rico, Saint Barthelemy, Saint Kitts and Nevis, Saint Lucia, Saint Martin (French part), Saint Vincent and the Grenadines, Trinidad and Tobago, British Virgin Is.",
                "Armenia, Azerbaijan, Georgia, Kazakhstan, Kyrgyzstan, Mongolia, Tajikistan, Turkmenistan, Uzbekistan",
                "China, Macao, Hong Kong",
                "Colombia",
                "Bulgaria, Cyprus, Czech Republic, Estonia, Hungary, Latvia, Lithuania, Malta, Poland, Romania, Slovakia, Slovenia",
                "Greenland, San Marino, Andorra, Austria, Belgium, Denmark, Finland, France, Germany, Greece, Iceland, Ireland, Italy, Vatican, Luxembourg, Monaco, Netherlands, Norway, Portugal, Spain, Sweden, Switzerland, United Kingdom, Br. Indian Ocean Ter.,Saint Helena, Falkland Is.,  Turks and Caicos Is.",
                "Belarus, Moldova, Ukraine",
                "Iceland, Norway, Switzerland",
                "Albania, Bosnia and Herzegovina, Croatia, North Macedonia, Montenegro, Serbia, Turkey, S. Geo. and the Is.",
                "India",
                "Indonesia",
                "Japan",
                "Mexico",
                "Bahrain, Iran, Iraq, Israel, Jordan, Kuwait, Lebanon, Oman, Palestine, Qatar, Saudi Arabia, Syria, United Arab Emirates, Yemen",
                "Pakistan",
                "Russia",
                "South Africa",
                "French Guiana, Guyana, Suriname, Venezuela",
                "Bolivia, Chile, Ecuador, Paraguay, Peru, Uruguay",
                "Afghanistan, Bangladesh, Bhutan, Maldives, Nepal, Sri Lanka",
                "North Korea, American Samoa, Brunei, Cambodia, Cook Islands, Fiji, Micronesia (Federated States of), Guam, Kiribati, Laos, Malaysia, Marshall Islands, Myanmar, Nauru, New Caledonia, Niue, Palau, Papua New Guinea, Philippines, Samoa, Singapore, Solomon Islands, Thailand, Timor-Leste, Tonga, Tuvalu, Vanuatu, Vietnam",
                "South Korea",
                "Taiwan",
                "United States, N. Mariana Is.")
)

regions <- regions %>%
  mutate(countries = strsplit(as.character(countries), ", ")) %>%
  mutate(iso_a3 = lapply(countries, function(x) countrycode(x, "country.name", "iso3c")))

regions$iso_a3[[1]][which(regions$countries[[1]] == "S. Sudan")] <- "SDS"
regions$iso_a3[[1]][which(regions$countries[[1]] == "Somaliland")] <- "SOL"

world <- world %>%
  mutate(region = NA)
for (i in 1:nrow(regions)) {
  for (country_code in regions$iso_a3[[i]]) {
    world$region[world$adm0_a3 == country_code] <- regions$region[i]
  }
}
library(ggforce)

file_path12 <- "/Users/noginger/Library/CloudStorage/OneDrive-个人/课题/第二项研究/收集数据/作图数据/图1地图.xlsx"
data <- read_excel(file_path12, sheet = "Sheet1")

coords <- data.frame(
  region = unique(data$region),
  X = c(36, 12, 20, 7, -65, 145, -49, -111, -70, 70, 110, -75, 20, 0, 30, 40, 11, 80, 121, 140, -99, 50, 65, 95, 25, -66, -65, 100, 128, 115, 25, -100),
  Y = c(5, 27, -19, 10, -35, -27, -10, 62, 20, 48, 32, 4, 47, 52, 52, 38, 66, 27, -3, 40, 22, 35, 32, 60, -30, 6, -17, 20, 37, 10, 120, 36)
)

process_data <- function(data, year) {
  filtered_data <- data %>%
    filter(Year == year, scenario == "SSP126")
  
  total_potential <- filtered_data %>%
    group_by(region) %>%
    summarise(
      biomass_potential = sum(SumHeatvalue_EJ),
      energy_potential = sum(EnerHeatvalue_EJ)
    )
  
  category_proportion <- filtered_data %>%
    group_by(region, Biomass_type) %>%
    summarise(energy_value = sum(EnerHeatvalue_EJ)) %>%
    pivot_wider(names_from = Biomass_type, values_from = energy_value, values_fill = list(energy_value = 0)) %>%
    mutate(
      total = AR + LR + WR,
      AR = AR / total,
      LR = LR / total,
      WR = WR / total
    ) %>%
    select(region, AR, LR, WR)
  
  merged_data <- total_potential %>%
    left_join(category_proportion, by = "region") %>%
    left_join(coords, by = "region")
  
  category_data_long <- merged_data %>%
    pivot_longer(cols = c(AR, LR, WR), names_to = "category", values_to = "proportion") %>%
    mutate(start_angle = lag(cumsum(proportion), default = 0),
           end_angle = cumsum(proportion))
  
  list(merged_data = merged_data, category_data_long = category_data_long)
}
library(tidyr)
data_2025 <- process_data(data, 2025)
data_2040 <- process_data(data, 2040)
data_2060 <- process_data(data, 2060)

world25 <- world %>%
  left_join(data_2025$merged_data %>% select(region, biomass_potential, energy_potential), by = "region")
library(ggforce)
library(ggnewscale)

create_plot <- function(world, category_data_long, title) {
  world <- world %>%
    mutate(biomass_potential_bin = cut(biomass_potential,
                                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40),
                                       labels = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40"),
                                       include.lowest = TRUE))
  ggplot() +
    geom_sf(data = world, aes(fill = biomass_potential_bin), color = NA) +
    scale_fill_manual(
      values = c(
        "0-5" = "#4575b4",    # 深蓝色
        "5-10" = "#91bfdb",   # 浅蓝色
        "10-15" = "#e0f3f8",  # 淡蓝色
        "15-20" = "#fde0ef",  # 粉色
        "20-25" = "#fdbbba",  # 淡粉色
        "25-30" = "#fc8d8d",  # 浅红色
        "30-35" = "#d73027",  # 红色
        "35-40" = "#a50026"   # 深红色
      ),
      name = "AFRs (EJ)"
    ) +
    new_scale_fill() +  # 添加新的填充比例
    geom_arc_bar(data = category_data_long, aes(
      x0 = X, y0 = Y, r0 = 0, r = sqrt(energy_potential) / max(sqrt(data_2025$merged_data$energy_potential)) * 10,
      start = 2 * pi * start_angle, end = 2 * pi * end_angle, fill = category
    ), alpha = 0.8, color = "black", linewidth = 0.5) +  # 设置边框颜色和线宽
    scale_fill_manual(values = c("AR" = "#F5CEA3", "LR" = "#77a88d", "WR" = "#8481bc"), name = "Category") +  # 自定义颜色
    scale_size_continuous(name = "Energy Potential (EJ)", range = c(2, 20)) +  # 增加图例
    guides(size = guide_legend(override.aes = list(shape = 21, fill = "grey", alpha = 0.5))) +  # 自定义图例
    labs(
      x = NULL,
      y = NULL) +
    theme_bw(base_size = 20) +
    theme(
      plot.title = element_blank(),  # 去掉图标题
      legend.title = element_blank(),  # 去掉图例标题
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "right"
    ) +
    labs(title = NULL) +
    coord_sf(ylim = c(-60, 90), xlim = NULL, expand = FALSE)
}

plot_2025 <- create_plot(world25, data_2025$category_data_long, "Biomass Potential and Energy Utilization (2025, SSP126)")
world40 <- world %>%
  left_join(data_2040$merged_data %>% select(region, biomass_potential, energy_potential), by = "region")
plot_2040 <- create_plot(world40, data_2040$category_data_long, "Biomass Potential and Energy Utilization (2040, SSP126)")
world60 <- world %>%
  left_join(data_2060$merged_data %>% select(region, biomass_potential, energy_potential), by = "region")
plot_2060 <- create_plot(world60, data_2060$category_data_long, "Biomass Potential and Energy Utilization (2060, SSP126)")


library(cowplot)
combined_plot_abc <- plot_grid(
  global_AR +  annotate(
    "text", x = 2025.5 , y = 141, label = "Crop residues",
    size = 7, color = "black", hjust = 0,
    fontface = "italic"  # 设置文本为粗体
  ) + 
    theme(legend.position = "none"), 
  global_LR + 
    annotate(
      "text", x = 2025.5 , y = 39, label = "Livestock excrements",
      size = 7, color = "black", hjust = 0,
      fontface = "italic"  # 设置文本为粗体
    ) + 
    theme(legend.position = "none"), 
  global_WR + 
    annotate(
      "text", x = 2025.5 , y = 200, label = "Forestry residues",
      size = 7, color = "black", hjust = 0,
      fontface = "italic"  # 设置文本为粗体
    ) + theme(
    legend.position = c(0.005, 0.88), 
    legend.justification = "left",
    legend.background = element_rect(fill = NA)  # 设置图例背景为空白
  ) +
    guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1)),  # 横向排列图例 
  ncol = 1, 
  labels = c("a", "b", "c"),
  label_size = 24, label_x = -0.03, label_y = 1.02,
  align = 'v'
)


combined_plot_def <- plot_grid(
  plot_2025 +  annotate(
    "text", x = -175 , y = 85, label = "2025",
    size = 7, color = "black", hjust = 0,
    fontface = "italic"  # 设置文本为粗体
  ) + 
    theme(legend.position = "none"), 
  plot_2040 + annotate(
    "text", x = -175 , y = 85, label = "2040",
    size = 7, color = "black", hjust = 0,
    fontface = "italic"  # 设置文本为粗体
  ) + 
    theme(legend.position = "none"), 
  plot_2060 +
    annotate(
      "text", x = -175 , y = 85, label = "2060",
      size = 7, color = "black", hjust = 0,
      fontface = "italic"  # 设置文本为粗体
    ) + 
    geom_circle(
      aes(x0 = -120, y0 = 0, r = sqrt(3) / sqrt(10.654) * 10), 
      fill = "lightgrey", alpha = 0.4,
      color = "black"
    ) +  # 第一个圆，位置和大小可调整
    geom_circle(
      aes(x0 = -120, y0 = -20, r = sqrt(6) / sqrt(10.654) * 10), 
      fill = "lightgrey",  alpha = 0.4,
      color = "black"
    ) +  # 第二个圆，位置和大小可调整
    geom_circle(
      aes(x0 = -120, y0 = -45, r = sqrt(12) / sqrt(10.654) * 10), 
      fill = "lightgrey",  alpha = 0.4,
      color = "black"
    ) +  # 第三个圆，位置和大小可调整
    # 为每个圆添加对应的文本
    annotate(
      "text", x = -120 + sqrt(3) / sqrt(10.654) * 10 + 7, y = 0, label = "3 EJ",
      size = 6, color = "black", hjust = 0
    ) +  # 在第一个圆的右侧添加数字3
    annotate(
      "text", x = -120 + sqrt(6) / sqrt(10.654) * 10 + 5, y = -20, label = "6 EJ", 
      size = 6, color = "black", hjust = 0
    ) +  # 在第二个圆的右侧添加数字6
    annotate(
      "text", x = -120 + sqrt(12) / sqrt(10.654) * 10 + 1, y = -45, label = "12 EJ",
      size = 6, color = "black", hjust = 0
    ) +  # 在第三个圆的右侧添加数字12
    annotate(
      "text", x = -147 , y = 6.5, label = "(EJ)",
      size = 5.5, color = "black", hjust = 0
    ) +  # 添加单位EJ
    theme(
      legend.position = c(0.002, 0.35), 
      legend.justification = "left",
      legend.background = element_rect(fill = NA),  # 设置图例背景为空白
      legend.box = "vertical",  # 确保图例竖向排列，但次序是扇形在下面
      legend.spacing.y = unit(0.1, 'cm')  # 调整图例组之间的间距
    ) +
    scale_fill_manual(
      values = c("AR" = "#F5CEA3", "LR" = "#77a88d", "WR" = "#8481bc"),
      name = "Category",  # 自定义图例标题
      labels = c("CRs", "LEs", "FRs")  # 自定义图例标签
    ) +
    guides(
      color = guide_legend(ncol = 1, order = 1, byrow = TRUE),  # 底色图例在上面，紧凑排列
      fill = guide_legend(ncol = 1, order = 2, byrow = TRUE)  # 扇形图例在下面，紧凑排列
    ),
  ncol = 1, 
  labels = c("d", "e", "f"),
  label_size = 24, label_x = 0.02, label_y = 1.02,
  align = 'v'
)
# 输出最终图表
final_combined_plot1 <- plot_grid(
  combined_plot_abc, 
  combined_plot_def, 
  ncol = 2,
  rel_widths = c(0.5, 1)  # abc 子图的宽度为原来的 0.5，def 子图保持宽度为 1
)

final_combined_plot1 <- final_combined_plot1 + 
  theme(plot.margin = unit(c(0.3, 0, 0.3, 0.4), "in"))

ggsave("Fig1.jpeg", final_combined_plot1, width = 19, height = 16, units = "in", dpi = 300)

#over
#fig1-SI-------
# SSP2-M
process_data <- function(data, year) {
  filtered_data <- data %>%
    filter(Year == year, scenario == "SSP245")
  
  total_potential <- filtered_data %>%
    group_by(region) %>%
    summarise(
      biomass_potential = sum(SumHeatvalue_EJ),
      energy_potential = sum(EnerHeatvalue_EJ)
    )
  
  category_proportion <- filtered_data %>%
    group_by(region, Biomass_type) %>%
    summarise(energy_value = sum(EnerHeatvalue_EJ)) %>%
    pivot_wider(names_from = Biomass_type, values_from = energy_value, values_fill = list(energy_value = 0)) %>%
    mutate(
      total = AR + LR + WR,
      AR = AR / total,
      LR = LR / total,
      WR = WR / total
    ) %>%
    select(region, AR, LR, WR)
  
  merged_data <- total_potential %>%
    left_join(category_proportion, by = "region") %>%
    left_join(coords, by = "region")
  
  category_data_long <- merged_data %>%
    pivot_longer(cols = c(AR, LR, WR), names_to = "category", values_to = "proportion") %>%
    mutate(start_angle = lag(cumsum(proportion), default = 0),
           end_angle = cumsum(proportion))
  
  list(merged_data = merged_data, category_data_long = category_data_long)
}
library(tidyr)
data_2025 <- process_data(data, 2025)
data_2040 <- process_data(data, 2040)
data_2060 <- process_data(data, 2060)

# 将区域数据合并到地图数据
world25 <- world %>%
  left_join(data_2025$merged_data %>% select(region, biomass_potential, energy_potential), by = "region")
library(ggforce)
library(ggnewscale)

create_plot <- function(world, category_data_long, title) {
  world <- world %>%
    mutate(biomass_potential_bin = cut(biomass_potential,
                                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40),
                                       labels = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40"),
                                       include.lowest = TRUE))
  ggplot() +
    geom_sf(data = world, aes(fill = biomass_potential_bin), color = NA) +
    scale_fill_manual(
      values = c(
        "0-5" = "#4575b4",    # 深蓝色
        "5-10" = "#91bfdb",   # 浅蓝色
        "10-15" = "#e0f3f8",  # 淡蓝色
        "15-20" = "#fde0ef",  # 粉色
        "20-25" = "#fdbbba",  # 淡粉色
        "25-30" = "#fc8d8d",  # 浅红色
        "30-35" = "#d73027",  # 红色
        "35-40" = "#a50026"   # 深红色
      ),
      name = "Biomass Potential (EJ)"
    ) +
    new_scale_fill() +  # 添加新的填充比例
    geom_arc_bar(data = category_data_long, aes(
      x0 = X, y0 = Y, r0 = 0, r = sqrt(energy_potential) / max(sqrt(10.65)) * 10,
      start = 2 * pi * start_angle, end = 2 * pi * end_angle, fill = category
    ), alpha = 0.8, color = "black", linewidth = 0.5) +  # 设置边框颜色和线宽
    scale_fill_manual(values = c("AR" = "#F5CEA3", "LR" = "#77a88d", "WR" = "#8481bc"), name = "Category") +  # 自定义颜色
    scale_size_continuous(name = "Energy Potential (EJ)", range = c(2, 20)) +  # 增加图例
    guides(size = guide_legend(override.aes = list(shape = 21, fill = "grey", alpha = 0.5))) +  # 自定义图例
    labs(
      x = NULL,
      y = NULL) +
    theme_bw(base_size = 20) +
    theme(
      plot.title = element_blank(),  # 去掉图标题
      legend.title = element_blank(),  # 去掉图例标题
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "right"
      
    ) +
    labs(title = NULL) +
    coord_sf(ylim = c(-60, 90), xlim = NULL, expand = FALSE)
}

plot_2025 <- create_plot(world25, data_2025$category_data_long, "Biomass Potential and Energy Utilization (2025, SSP126)")
world40 <- world %>%
  left_join(data_2040$merged_data %>% select(region, biomass_potential, energy_potential), by = "region")
plot_2040 <- create_plot(world40, data_2040$category_data_long, "Biomass Potential and Energy Utilization (2040, SSP126)")
world60 <- world %>%
  left_join(data_2060$merged_data %>% select(region, biomass_potential, energy_potential), by = "region")
plot_2060 <- create_plot(world60, data_2060$category_data_long, "Biomass Potential and Energy Utilization (2060, SSP126)")


Plot1_SI_2M <- plot_grid(
  plot_2025 +  annotate(
    "text", x = -175 , y = 85, label = "2025",
    size = 7, color = "black", hjust = 0,
    fontface = "italic"  # 设置文本为粗体
  ) + 
    theme(legend.position = "none"), 
  plot_2040 + annotate(
    "text", x = -175 , y = 85, label = "2040",
    size = 7, color = "black", hjust = 0,
    fontface = "italic"  # 设置文本为粗体
  ) + 
    theme(legend.position = "none"), 
  plot_2060 +
    annotate(
      "text", x = -175 , y = 85, label = "2060",
      size = 7, color = "black", hjust = 0,
      fontface = "italic"  # 设置文本为粗体
    ) + 
    geom_circle(
      aes(x0 = -120, y0 = 0, r = sqrt(3) / sqrt(10.654) * 10), 
      fill = "lightgrey", alpha = 0.4,
      color = "black"
    ) +  # 第一个圆，位置和大小可调整
    geom_circle(
      aes(x0 = -120, y0 = -20, r = sqrt(6) / sqrt(10.654) * 10), 
      fill = "lightgrey",  alpha = 0.4,
      color = "black"
    ) +  # 第二个圆，位置和大小可调整
    geom_circle(
      aes(x0 = -120, y0 = -45, r = sqrt(12) / sqrt(10.654) * 10), 
      fill = "lightgrey",  alpha = 0.4,
      color = "black"
    ) +  # 第三个圆，位置和大小可调整
    # 为每个圆添加对应的文本
    annotate(
      "text", x = -120 + sqrt(3) / sqrt(10.654) * 10 + 7, y = 0, label = "3 EJ",
      size = 6, color = "black", hjust = 0
    ) +  # 在第一个圆的右侧添加数字3
    annotate(
      "text", x = -120 + sqrt(6) / sqrt(10.654) * 10 + 5, y = -20, label = "6 EJ", 
      size = 6, color = "black", hjust = 0
    ) +  # 在第二个圆的右侧添加数字6
    annotate(
      "text", x = -120 + sqrt(12) / sqrt(10.654) * 10 + 1, y = -45, label = "12 EJ",
      size = 6, color = "black", hjust = 0
    ) +  # 在第三个圆的右侧添加数字12
    annotate(
      "text", x = -147 , y = 6.5, label = "(EJ)",
      size = 5.5, color = "black", hjust = 0
    ) +  # 添加单位EJ
    theme(
      legend.position = c(0.002, 0.35), 
      legend.justification = "left",
      legend.background = element_rect(fill = NA),  # 设置图例背景为空白
      legend.box = "vertical",  # 确保图例竖向排列，但次序是扇形在下面
      legend.spacing.y = unit(0.1, 'cm')  # 调整图例组之间的间距
    ) +
    scale_fill_manual(
      values = c("AR" = "#F5CEA3", "LR" = "#77a88d", "WR" = "#8481bc"),
      name = "Category",  # 自定义图例标题
      labels = c("CRs", "LEs", "FRs")  # 自定义图例标签
    ) +
    guides(
      color = guide_legend(ncol = 1, order = 1, byrow = TRUE),  # 底色图例在上面，紧凑排列
      fill = guide_legend(ncol = 1, order = 2, byrow = TRUE)  # 扇形图例在下面，紧凑排列
    ),
  ncol = 1, 
  labels = c("a", "b", "c"),
  label_size = 24, label_x = -0.005, label_y = 1.02,
  align = 'v'
)

ggsave("Plot1_SI_2M.jpeg", Plot1_SI_2M, width = 12, height = 16, units = "in", dpi = 300)

# SSP4-M
process_data <- function(data, year) {
  filtered_data <- data %>%
    filter(Year == year, scenario == "SSP445")
  
  total_potential <- filtered_data %>%
    group_by(region) %>%
    summarise(
      biomass_potential = sum(SumHeatvalue_EJ),
      energy_potential = sum(EnerHeatvalue_EJ)
    )
  
  category_proportion <- filtered_data %>%
    group_by(region, Biomass_type) %>%
    summarise(energy_value = sum(EnerHeatvalue_EJ)) %>%
    pivot_wider(names_from = Biomass_type, values_from = energy_value, values_fill = list(energy_value = 0)) %>%
    mutate(
      total = AR + LR + WR,
      AR = AR / total,
      LR = LR / total,
      WR = WR / total
    ) %>%
    select(region, AR, LR, WR)
  
  merged_data <- total_potential %>%
    left_join(category_proportion, by = "region") %>%
    left_join(coords, by = "region")
  
  category_data_long <- merged_data %>%
    pivot_longer(cols = c(AR, LR, WR), names_to = "category", values_to = "proportion") %>%
    mutate(start_angle = lag(cumsum(proportion), default = 0),
           end_angle = cumsum(proportion))
  
  list(merged_data = merged_data, category_data_long = category_data_long)
}
library(tidyr)
data_2025 <- process_data(data, 2025)
data_2040 <- process_data(data, 2040)
data_2060 <- process_data(data, 2060)

# 将区域数据合并到地图数据
world25 <- world %>%
  left_join(data_2025$merged_data %>% select(region, biomass_potential, energy_potential), by = "region")
library(ggforce)
library(ggnewscale)

create_plot <- function(world, category_data_long, title) {
  # 将 biomass_potential 分段
  world <- world %>%
    mutate(biomass_potential_bin = cut(biomass_potential,
                                       breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40),
                                       labels = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40"),
                                       include.lowest = TRUE))
  ggplot() +
    geom_sf(data = world, aes(fill = biomass_potential_bin), color = NA) +
    scale_fill_manual(
      values = c(
        "0-5" = "#4575b4",    # 深蓝色
        "5-10" = "#91bfdb",   # 浅蓝色
        "10-15" = "#e0f3f8",  # 淡蓝色
        "15-20" = "#fde0ef",  # 粉色
        "20-25" = "#fdbbba",  # 淡粉色
        "25-30" = "#fc8d8d",  # 浅红色
        "30-35" = "#d73027",  # 红色
        "35-40" = "#a50026"   # 深红色
      ),
      name = "Biomass Potential (EJ)"
    ) +
    new_scale_fill() +  # 添加新的填充比例
    geom_arc_bar(data = category_data_long, aes(
      x0 = X, y0 = Y, r0 = 0, r = sqrt(energy_potential) / max(sqrt(10.65)) * 10,
      start = 2 * pi * start_angle, end = 2 * pi * end_angle, fill = category
    ), alpha = 0.8, color = "black", linewidth = 0.5) +  # 设置边框颜色和线宽
    scale_fill_manual(values = c("AR" = "#F5CEA3", "LR" = "#77a88d", "WR" = "#8481bc"), name = "Category") +  # 自定义颜色
    scale_size_continuous(name = "Energy Potential (EJ)", range = c(2, 20)) +  # 增加图例
    guides(size = guide_legend(override.aes = list(shape = 21, fill = "grey", alpha = 0.5))) +  # 自定义图例
    labs(
      x = NULL,
      y = NULL) +
    theme_bw(base_size = 20) +
    theme(
      plot.title = element_blank(),  # 去掉图标题
      legend.title = element_blank(),  # 去掉图例标题
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "right"
    ) +
    labs(title = NULL) +
    coord_sf(ylim = c(-60, 90), xlim = NULL, expand = FALSE)
}

plot_2025 <- create_plot(world25, data_2025$category_data_long, "Biomass Potential and Energy Utilization (2025, SSP126)")
# 将区域数据合并到地图数据
world40 <- world %>%
  left_join(data_2040$merged_data %>% select(region, biomass_potential, energy_potential), by = "region")
plot_2040 <- create_plot(world40, data_2040$category_data_long, "Biomass Potential and Energy Utilization (2040, SSP126)")
# 将区域数据合并到地图数据
world60 <- world %>%
  left_join(data_2060$merged_data %>% select(region, biomass_potential, energy_potential), by = "region")
plot_2060 <- create_plot(world60, data_2060$category_data_long, "Biomass Potential and Energy Utilization (2060, SSP126)")


Plot1_SI_4M <- plot_grid(
  plot_2025 +  annotate(
    "text", x = -175 , y = 85, label = "2025",
    size = 7, color = "black", hjust = 0,
    fontface = "italic"  # 设置文本为粗体
  ) + 
    theme(legend.position = "none"), 
  plot_2040 + annotate(
    "text", x = -175 , y = 85, label = "2040",
    size = 7, color = "black", hjust = 0,
    fontface = "italic"  # 设置文本为粗体
  ) + 
    theme(legend.position = "none"), 
  plot_2060 +
    annotate(
      "text", x = -175 , y = 85, label = "2060",
      size = 7, color = "black", hjust = 0,
      fontface = "italic"  # 设置文本为粗体
    ) + 
    geom_circle(
      aes(x0 = -120, y0 = 0, r = sqrt(3) / sqrt(10.654) * 10), 
      fill = "lightgrey", alpha = 0.4,
      color = "black"
    ) +  # 第一个圆，位置和大小可调整
    geom_circle(
      aes(x0 = -120, y0 = -20, r = sqrt(6) / sqrt(10.654) * 10), 
      fill = "lightgrey",  alpha = 0.4,
      color = "black"
    ) +  # 第二个圆，位置和大小可调整
    geom_circle(
      aes(x0 = -120, y0 = -45, r = sqrt(12) / sqrt(10.654) * 10), 
      fill = "lightgrey",  alpha = 0.4,
      color = "black"
    ) +  # 第三个圆，位置和大小可调整
    # 为每个圆添加对应的文本
    annotate(
      "text", x = -120 + sqrt(3) / sqrt(10.654) * 10 + 7, y = 0, label = "3 EJ",
      size = 6, color = "black", hjust = 0
    ) +  # 在第一个圆的右侧添加数字3
    annotate(
      "text", x = -120 + sqrt(6) / sqrt(10.654) * 10 + 5, y = -20, label = "6 EJ", 
      size = 6, color = "black", hjust = 0
    ) +  # 在第二个圆的右侧添加数字6
    annotate(
      "text", x = -120 + sqrt(12) / sqrt(10.654) * 10 + 1, y = -45, label = "12 EJ",
      size = 6, color = "black", hjust = 0
    ) +  # 在第三个圆的右侧添加数字12
    annotate(
      "text", x = -147 , y = 6.5, label = "(EJ)",
      size = 5.5, color = "black", hjust = 0
    ) +  # 添加单位EJ
    theme(
      legend.position = c(0.002, 0.35), 
      legend.justification = "left",
      legend.background = element_rect(fill = NA),  # 设置图例背景为空白
      legend.box = "vertical",  # 确保图例竖向排列，但次序是扇形在下面
      legend.spacing.y = unit(0.1, 'cm')  # 调整图例组之间的间距
    ) +
    scale_fill_manual(
      values = c("AR" = "#F5CEA3", "LR" = "#77a88d", "WR" = "#8481bc"),
      name = "Category",  # 自定义图例标题
      labels = c("CRs", "LEs", "FRs")  # 自定义图例标签
    ) +
    guides(
      color = guide_legend(ncol = 1, order = 1, byrow = TRUE),  # 底色图例在上面，紧凑排列
      fill = guide_legend(ncol = 1, order = 2, byrow = TRUE)  # 扇形图例在下面，紧凑排列
    ),
  ncol = 1, 
  labels = c("a", "b", "c"),
  label_size = 24, label_x = -0.005, label_y = 1.02,
  align = 'v'
)

ggsave("Plot1_SI_4M.jpeg", Plot1_SI_4M, width = 12, height = 16, units = "in", dpi = 300)






#fig2-abc-----------------------------------------------------------------
library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)
file_path2a <- "/.../regionenergySSP1H.xlsx"
regionEner <- read_excel(file_path2a, sheet = 1)
regionEner1 <- regionEner %>% group_by(region, Year) %>%
  summarise(
    Bioenergy_MJ = sum(Bioenergy_MJ),
    `finalenergydemand(EJ)` = sum(`finalenergydemand(EJ)`)/3,
    proportion100 = sum(proportion100)
  )

region_abbreviations <- c(
  "USA" = "USA",
  "Taiwan" = "TWN",
  "Southeast Asia" = "SEA",
  "South Korea" = "KOR",
  "South Asia" = "SAS",
  "South America_Southern" = "SAMS",
  "South America_Northern" = "SAMN",
  "South Africa" = "ZAF",
  "Russia" = "RUS",
  "Pakistan" = "PAK",
  "Middle East" = "MEA",
  "Mexico" = "MEX",
  "Japan" = "JPN",
  "Indonesia" = "IDN",
  "India" = "IND",
  "European Free Trade Association" = "EFTA",
  "Europe_Non_EU" = "NEU",
  "Europe_Eastern" = "EEU",
  "EU-15" = "EU15",
  "EU-12" = "EU12",
  "Colombia" = "COL",
  "China" = "CHN",
  "Central Asia" = "CAS",
  "Central America and Caribbean" = "CAC",
  "Canada" = "CAN",
  "Brazil" = "BRA",
  "Australia_NZ" = "ANZ",
  "Argentina" = "ARG",
  "Africa_Western" = "WAF",
  "Africa_Southern" = "SAF",
  "Africa_Northern" = "NAF",
  "Africa_Eastern" = "EAF"
)

region_order <- c("EAF", "NAF", "SAF", "WAF", "ZAF", 
                  "CAS", "CHN", "IDN", "IND", "JPN", "KOR", "MEA", "PAK", "SAS", "SEA", "TWN",
                  "EEU", "EFTA", "EU12", "EU15", "NEU", "RUS",
                  "CAC", "CAN", "MEX", "USA",
                  "ANZ",
                  "ARG", "BRA", "COL", "SAMN","SAMS"
)

regionEner1 <- regionEner1 %>%
  mutate(region = factor(region, levels = names(region_abbreviations), labels = region_abbreviations),
         region = factor(region, levels = region_order))

plot_year <- function(year_data, year) {
  max_bioenergy_tj <- max(year_data$Bioenergy_MJ / 1e12)
  max_proportion <- max(year_data$proportion100)
  
  p <- ggplot(year_data, aes(x = region)) +
    geom_bar(aes(y = Bioenergy_MJ / 1e12), stat = "identity", fill = "#1E88E5", color = NA) +  
    geom_point(aes(y = proportion100 * 8 / 25), color = "#E53935", size = 2, shape = 17) +  
    geom_vline(xintercept = c(5.5, 16.5, 22.5, 26.5, 27.5), color = "gray40", linetype = "dashed") +  
    annotate("text", x = c(1.15, 6.05, 17.35, 24.15, 27.38, 30.88), y = 7, label = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"), color = "gray40", size = 5, fontface = "bold.italic") +  # 添加区域名称标注
    annotate("text", x = 1.15, y = 8, label = "SSP1-H", color = "black", size = 4.5, fontface = "italic") +  # 添加情景标注
    geom_hline(yintercept = 3.2, color = "#E53935", linetype = "dashed", size = 0.5, alpha = 0.8) + 
    scale_y_continuous(
      name = "Bioenergy output (EJ)",
      limits = c(0, 8),  # 设置能源产出范围为0-8 EJ
      sec.axis = sec_axis(~ . * (25 / 8), name = "Percentage in energy demand (%)", breaks = seq(0, 25, 5))  # 设置右轴的刻度标签
    ) +
    labs(title = "", x = "") +  # 去掉图的标题和横轴标题
    theme_bw() +  # 使用 theme_bw()
    theme(
      panel.grid = element_blank(),  # 去掉灰色网格
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # 添加边框
      axis.text.y = element_text(color = "black", size = 14),  # 将左轴数字标注改为柱图颜色,字体大小14
      axis.text.y.right = element_text(color = "black", size = 14),  # 将右轴数字标注改为点图颜色,字体大小14
      axis.title.y = element_text(color = "black", size = 14),  # 左轴标题颜色改为蓝色, 字体大小16
      axis.title.y.right = element_text(color = "black", size = 14),  # 右轴标题颜色改为粉色, 字体大小16
      axis.text.x = element_text(size = 12, angle = 315),  # 将横轴标注字体大小改为11
      plot.title = element_text(size = 11)  # 将图标题字体大小改为11
    )
  
  return(p)
}
years <- unique(regionEner1$Year)

plots_year <- lapply(years, function(year) {
  year_data <- regionEner1 %>% filter(Year == year)
  plot_year(year_data, year)
})
a2025_1H <- plots_year[[match(2025, years)]]
b2040_1H <- plots_year[[match(2040, years)]]
c2060_1H <- plots_year[[match(2060, years)]]
#over
#Plot2-def-----------------------
library(ggplot2)
library(treemapify)
library(readxl)
library(patchwork)
library(dplyr)

regionEner <- read_excel(file_path2a, sheet = 1)
regionEner2 <- regionEner %>% group_by(Development, Year, Biomass_type) %>%
  summarise(
    BR_ener = sum(BR_ener)
  )

file_path2d <- "/.../globalenergytechSSP1H.xlsx"
biotecEner <- read_excel(file_path2d, sheet = 1)

year_totals <- colSums(biotecEner[-1])

custom_colors <- c(
  "CHP_AR" = "#4CAF50", "BF_AR" = "#E91E63", "Bg_AR" = "#03A9F4", "Be_AR" = "#9C27B0",
  "BP_AR" = "#FFC107", "DH_AR" = "#FF9800", "CFPG_AR" = "#673AB7", "BgPG_LR" = "#8BC34A",
  "Cp_LR" = "#9E9E9E", "Bg_LR" = "#00BCD4", "GPG_WR" = "#4CAF50", "BF_WR" = "#F44336",
  "CFPG_WR" = "#673AB7", "RJF_WR" = "#E91E63"
)

custom_labels <- c(
  "CHP_AR" = "CHP(CRs)", "BF_AR" = "BF(CRs)", "Bg_AR" = "Bg(CRs)", "Be_AR" = "Be(CRs)",
  "BP_AR" = "BP(CRs)", "DH_AR" = "DH(CRs)", "CFPG_AR" = "CFPG (CRs)", "BgPG_LR" = "BgPG(LEs)",
  "Cp_LR" = "Cp(LEs)", "Bg_LR" = "Bg(LEs)", "GPG_WR" = "GPG(FRs)", "BF_WR" = "BF(FRs)",
  "CFPG_WR" = "CFPG(FRs)", "RJF_WR" = "RJF(FRs)"
)

plot_treemap <- function(data, year_name) {
  data <- data %>%
    mutate(
      scaled_area = get(year_name) / year_totals[year_name],  # 标准化处理
      proportion_label = custom_labels[tech_name]   # 添加比例标签
    )
  
  ggplot(data, aes(area = scaled_area, fill = tech_name, label = proportion_label)) +
    geom_treemap() +
    geom_treemap_text(
      fontface = "bold",
      color = "white",
      place = "bottomright",
      grow = FALSE,
      size = 20  # 调整标签大小
    ) +
    scale_fill_manual(values = custom_colors) +
    #labs(title = paste(" ", year_name)) +
    theme(legend.position = NULL)
}

years <- names(biotecEner)[-1]  # 获取所有情景的列名
plots <- lapply(years, function(year) {
  plot_treemap(biotecEner, year) + 
    theme(legend.position = "none")  # 移除单个图的图例
})

d2025_1H <- plots[[match(2025, years)]]
e2040_1H <- plots[[match(2040, years)]]
f2060_1H <- plots[[match(2060, years)]]
#over

#SSP2-M
regionEner <- read_excel(file_path2a, sheet = 3)
regionEner1 <- regionEner %>% group_by(region, Year) %>%
  summarise(
    Bioenergy_MJ = sum(Bioenergy_MJ),
    `finalenergydemand(EJ)` = sum(`finalenergydemand(EJ)`)/3,
    proportion100 = sum(proportion100)
  )

regionEner1 <- regionEner1 %>%
  mutate(region = factor(region, levels = names(region_abbreviations), labels = region_abbreviations),
         region = factor(region, levels = region_order))

plot_year <- function(year_data, year) {
  max_bioenergy_tj <- max(year_data$Bioenergy_MJ / 1e12)
  max_proportion <- max(year_data$proportion100)
  
  p <- ggplot(year_data, aes(x = region)) +
    geom_bar(aes(y = Bioenergy_MJ / 1e12), stat = "identity", fill = "#1E88E5", color = NA) +  
    geom_point(aes(y = proportion100 * 8 / 25), color = "#E53935", size = 2, shape = 17) +  
    geom_vline(xintercept = c(5.5, 16.5, 22.5, 26.5, 27.5), color = "gray40", linetype = "dashed") +  # 添加灰色竖线分隔
    annotate("text", x = c(1.15, 6.05, 17.35, 24.15, 27.38, 30.88), y = 7, label = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"), color = "gray40", size = 5, fontface = "bold.italic") +  # 添加区域名称标注
    annotate("text", x = 1.15, y = 8, label = "SSP2-M", color = "black", size = 4.5, fontface = "italic") +  # 添加情景标注
    geom_hline(yintercept = 3.2, color = "#E53935", linetype = "dashed", size = 0.5, alpha = 0.8) + 
    scale_y_continuous(
      name = "Bioenergy output (EJ)",
      limits = c(0, 8),  
      sec.axis = sec_axis(~ . * (25 / 8), name = "Percentage in energy demand (%)", breaks = seq(0, 25, 5))  # 设置右轴的刻度标签
    ) +
    labs(title = "", x = "") +  # 去掉图的标题和横轴标题
    theme_bw() + 
    theme(
      panel.grid = element_blank(),  # 去掉灰色网格
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # 添加边框
      axis.text.y = element_text(color = "black", size = 14),  # 将左轴数字标注改为柱图颜色,字体大小14
      axis.text.y.right = element_text(color = "black", size = 14),  # 将右轴数字标注改为点图颜色,字体大小14
      axis.title.y = element_text(color = "black", size = 14),  # 左轴标题颜色改为蓝色, 字体大小16
      axis.title.y.right = element_text(color = "black", size = 14),  # 右轴标题颜色改为粉色, 字体大小16
      axis.text.x = element_text(size = 12, angle = 315),  # 将横轴标注字体大小改为11
      plot.title = element_text(size = 11)  # 将图标题字体大小改为11
    )
  
  return(p)
}

years <- unique(regionEner1$Year)

plots_year <- lapply(years, function(year) {
  year_data <- regionEner1 %>% filter(Year == year)
  plot_year(year_data, year)
})
a2025_2M <- plots_year[[match(2025, years)]]
b2040_2M <- plots_year[[match(2040, years)]]
c2060_2M <- plots_year[[match(2060, years)]]


file_path2d <- "/.../globalenergytechSSP1H.xlsx"
biotecEner <- read_excel(file_path2d, sheet = 3)

year_totals <- colSums(biotecEner[-1])

years <- names(biotecEner)[-1]  # 获取所有情景的列名
plots <- lapply(years, function(year) {
  plot_treemap(biotecEner, year) + 
    theme(legend.position = "none")  # 移除单个图的图例
})

d2025_2M <- plots[[match(2025, years)]]
e2040_2M <- plots[[match(2040, years)]]
f2060_2M <- plots[[match(2060, years)]]

#SSP4-M
regionEner <- read_excel(file_path2a, sheet = 5)
regionEner1 <- regionEner %>% group_by(region, Year) %>%
  summarise(
    Bioenergy_MJ = sum(Bioenergy_MJ),
    `finalenergydemand(EJ)` = sum(`finalenergydemand(EJ)`)/3,
    proportion100 = sum(proportion100)
  )

regionEner1 <- regionEner1 %>%
  mutate(region = factor(region, levels = names(region_abbreviations), labels = region_abbreviations),
         region = factor(region, levels = region_order))

plot_year <- function(year_data, year) {
  max_bioenergy_tj <- max(year_data$Bioenergy_MJ / 1e12)
  max_proportion <- max(year_data$proportion100)
  
  p <- ggplot(year_data, aes(x = region)) +
    geom_bar(aes(y = Bioenergy_MJ / 1e12), stat = "identity", fill = "#1E88E5", color = NA) +  
    geom_point(aes(y = proportion100 * 8 / 25), color = "#E53935", size = 2, shape = 17) +  
    geom_vline(xintercept = c(5.5, 16.5, 22.5, 26.5, 27.5), color = "gray40", linetype = "dashed") +  # 添加灰色竖线分隔
    annotate("text", x = c(1.15, 6.05, 17.35, 24.15, 27.38, 30.88), y = 7, label = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"), color = "gray40", size = 5, fontface = "bold.italic") +  # 添加区域名称标注
    annotate("text", x = 1.15, y = 8, label = "SSP4-M", color = "black", size = 4.5, fontface = "italic") +  # 添加情景标注
    geom_hline(yintercept = 3.2, color = "#E53935", linetype = "dashed", size = 0.5, alpha = 0.8) + 
    scale_y_continuous(
      name = "Bioenergy output (EJ)",
      limits = c(0, 8),  
      sec.axis = sec_axis(~ . * (25 / 8), name = "Percentage in energy demand (%)", breaks = seq(0, 25, 5))  # 设置右轴的刻度标签
    ) +
    labs(title = "", x = "") +  # 去掉图的标题和横轴标题
    theme_bw() +  
    theme(
      panel.grid = element_blank(),  # 去掉灰色网格
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # 添加边框
      axis.text.y = element_text(color = "black", size = 14),  # 将左轴数字标注改为柱图颜色,字体大小14
      axis.text.y.right = element_text(color = "black", size = 14),  # 将右轴数字标注改为点图颜色,字体大小14
      axis.title.y = element_text(color = "black", size = 14),  # 左轴标题颜色改为蓝色, 字体大小16
      axis.title.y.right = element_text(color = "black", size = 14),  # 右轴标题颜色改为粉色, 字体大小16
      axis.text.x = element_text(size = 12, angle = 315),  # 将横轴标注字体大小改为11
      plot.title = element_text(size = 11)  # 将图标题字体大小改为11
    )
  
  return(p)
}

years <- unique(regionEner1$Year)

plots_year <- lapply(years, function(year) {
  year_data <- regionEner1 %>% filter(Year == year)
  plot_year(year_data, year)
})
a2025_4M <- plots_year[[match(2025, years)]]
b2040_4M <- plots_year[[match(2040, years)]]
c2060_4M <- plots_year[[match(2060, years)]]


biotecEner <- read_excel(file_path2d, sheet = 5)

year_totals <- colSums(biotecEner[-1])

years <- names(biotecEner)[-1]  # 获取所有情景的列名
plots <- lapply(years, function(year) {
  plot_treemap(biotecEner, year) + 
    theme(legend.position = "none")  # 移除单个图的图例
})

d2025_4M <- plots[[match(2025, years)]]
e2040_4M <- plots[[match(2040, years)]]
f2060_4M <- plots[[match(2060, years)]]

#over

#SSP5-L
regionEner <- read_excel(file_path2a, sheet = 6)
regionEner1 <- regionEner %>% group_by(region, Year) %>%
  summarise(
    Bioenergy_MJ = sum(Bioenergy_MJ),
    `finalenergydemand(EJ)` = sum(`finalenergydemand(EJ)`)/3,
    proportion100 = sum(proportion100)
  )

# 将地区名称替换为缩写并设置为因子以指定顺序
regionEner1 <- regionEner1 %>%
  mutate(region = factor(region, levels = names(region_abbreviations), labels = region_abbreviations),
         region = factor(region, levels = region_order))

plot_year <- function(year_data, year) {
  max_bioenergy_tj <- max(year_data$Bioenergy_MJ / 1e12)
  max_proportion <- max(year_data$proportion100)
  
  p <- ggplot(year_data, aes(x = region)) +
    geom_bar(aes(y = Bioenergy_MJ / 1e12), stat = "identity", fill = "#1E88E5", color = NA) +  # 将能源产出除以1e12以显示为EJ,柱子颜色改为#66CCFF,去掉边框
    geom_point(aes(y = proportion100 * 8 / 25), color = "#E53935", size = 2, shape = 17) +  # 点的颜色改为#FF9999
    geom_vline(xintercept = c(5.5, 16.5, 22.5, 26.5, 27.5), color = "gray40", linetype = "dashed") +  # 添加灰色竖线分隔
    annotate("text", x = c(1.15, 6.05, 17.35, 24.15, 27.38, 30.88), y = 7, label = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America"), color = "gray40", size = 5, fontface = "bold.italic") +  # 添加区域名称标注
    annotate("text", x = 1.15, y = 8, label = "SSP5-L", color = "black", size = 4.5, fontface = "italic") +  # 添加情景标注
    geom_hline(yintercept = 3.2, color = "#E53935", linetype = "dashed", size = 0.5, alpha = 0.8) + 
    scale_y_continuous(
      name = "Bioenergy output (EJ)",
      limits = c(0, 8),  # 设置能源产出范围为0-8 EJ
      sec.axis = sec_axis(~ . * (25 / 8), name = "Percentage in energy demand (%)", breaks = seq(0, 25, 5))  # 设置右轴的刻度标签
    ) +
    labs(title = "", x = "") +  # 去掉图的标题和横轴标题
    theme_bw() +  # 使用 theme_bw()
    theme(
      panel.grid = element_blank(),  # 去掉灰色网格
      panel.border = element_rect(color = "black", fill = NA, size = 1),  # 添加边框
      axis.text.y = element_text(color = "black", size = 14),  # 将左轴数字标注改为柱图颜色,字体大小14
      axis.text.y.right = element_text(color = "black", size = 14),  # 将右轴数字标注改为点图颜色,字体大小14
      axis.title.y = element_text(color = "black", size = 14),  # 左轴标题颜色改为蓝色, 字体大小16
      axis.title.y.right = element_text(color = "black", size = 14),  # 右轴标题颜色改为粉色, 字体大小16
      axis.text.x = element_text(size = 12, angle = 315),  # 将横轴标注字体大小改为11
      plot.title = element_text(size = 11)  # 将图标题字体大小改为11
    )
  
  return(p)
}

years <- unique(regionEner1$Year)

# 根据不同年份绘制图表
plots_year <- lapply(years, function(year) {
  year_data <- regionEner1 %>% filter(Year == year)
  plot_year(year_data, year)
})
# 分别提取每个年份的图表
a2025_5L <- plots_year[[match(2025, years)]]
b2040_5L <- plots_year[[match(2040, years)]]
c2060_5L <- plots_year[[match(2060, years)]]


biotecEner <- read_excel(file_path2d, sheet = 6)

year_totals <- colSums(biotecEner[-1])

years <- names(biotecEner)[-1]  # 获取所有情景的列名
plots <- lapply(years, function(year) {
  plot_treemap(biotecEner, year) + 
    theme(legend.position = "none")  # 移除单个图的图例
})

d2025_5L <- plots[[match(2025, years)]]
e2040_5L <- plots[[match(2040, years)]]
f2060_5L <- plots[[match(2060, years)]]

#over

c2060_1H <- c2060_1H + labs(tag = "a") + theme(plot.tag = element_text(face = "bold", size = 18))
c2060_2M <- c2060_2M + labs(tag = "b") + theme(plot.tag = element_text(face = "bold", size = 18))
c2060_4M <- c2060_4M + labs(tag = "c") + theme(plot.tag = element_text(face = "bold", size = 18))
c2060_5L <- c2060_5L + labs(tag = "d") + theme(plot.tag = element_text(face = "bold", size = 18))
region_ener_2060 <- wrap_plots(list(c2060_1H, c2060_2M, c2060_4M, c2060_5L), ncol = 1) +
  plot_annotation() 

f2060_1H <- f2060_1H + labs(tag = "e") + theme(plot.tag = element_text(face = "bold", size = 18))
f2060_2M <- f2060_2M + labs(tag = "f") + theme(plot.tag = element_text(face = "bold", size = 18))
f2060_4M <- f2060_4M + labs(tag = "g") + theme(plot.tag = element_text(face = "bold", size = 18))
f2060_5L <- f2060_5L + labs(tag = "h") + theme(plot.tag = element_text(face = "bold", size = 18))
biotecener_treemap_2060 <- wrap_plots(list(f2060_1H, f2060_2M, f2060_4M, f2060_5L), ncol = 2) +
  plot_annotation()

Plot2_try <- plot_grid(
  region_ener_2060, 
  biotecener_treemap_2060, 
  nrow = 2,
  rel_heights = c(1, 0.5)  # abc 子图的宽度为原来的 0.5，def 子图保持宽度为 1
)

Plot2_try <- Plot2_try + 
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "in"))

ggsave("Fig2.jpeg", Plot2_try, width = 16, height = 20, dpi = 300) 



#fig2-SI-----
a2025_1H <- a2025_1H + labs(tag = "a") + theme(plot.tag = element_text(face = "bold", size = 18))
a2025_2M <- a2025_2M + labs(tag = "b") + theme(plot.tag = element_text(face = "bold", size = 18))
a2025_4M <- a2025_4M + labs(tag = "c") + theme(plot.tag = element_text(face = "bold", size = 18))
a2025_5L <- a2025_5L + labs(tag = "d") + theme(plot.tag = element_text(face = "bold", size = 18))
region_ener_2025 <- wrap_plots(list(a2025_1H, a2025_2M, a2025_4M, a2025_5L), ncol = 1) +
  plot_annotation() 

d2025_1H <- d2025_1H + labs(tag = "e") + theme(plot.tag = element_text(face = "bold", size = 18))
d2025_2M <- d2025_2M + labs(tag = "f") + theme(plot.tag = element_text(face = "bold", size = 18))
d2025_4M <- d2025_4M + labs(tag = "g") + theme(plot.tag = element_text(face = "bold", size = 18))
d2025_5L <- d2025_5L + labs(tag = "h") + theme(plot.tag = element_text(face = "bold", size = 18))
biotecener_treemap_2025 <- wrap_plots(list(d2025_1H, d2025_2M, d2025_4M, d2025_5L), ncol = 2) +
  plot_annotation()

Plot2_SI_2025 <- plot_grid(
  region_ener_2025, 
  biotecener_treemap_2025, 
  nrow = 2,
  rel_heights = c(1, 0.5)  # abc 子图的宽度为原来的 0.5，def 子图保持宽度为 1
)

Plot2_SI_2025 <- Plot2_SI_2025 + 
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "in"))

ggsave("Plot2_SI_2025.jpeg", Plot2_SI_2025, width = 16, height = 20, dpi = 300)  # 保存为PDF文件

#Plot2-2040
b2040_1H <- b2040_1H + labs(tag = "a") + theme(plot.tag = element_text(face = "bold", size = 18))
b2040_2M <- b2040_2M + labs(tag = "b") + theme(plot.tag = element_text(face = "bold", size = 18))
b2040_4M <- b2040_4M + labs(tag = "c") + theme(plot.tag = element_text(face = "bold", size = 18))
b2040_5L <- b2040_5L + labs(tag = "d") + theme(plot.tag = element_text(face = "bold", size = 18))
region_ener_2040 <- wrap_plots(list(b2040_1H, b2040_2M, b2040_4M, b2040_5L), ncol = 1) +
  plot_annotation() 

e2040_1H <- e2040_1H + labs(tag = "e") + theme(plot.tag = element_text(face = "bold", size = 18))
e2040_2M <- e2040_2M + labs(tag = "f") + theme(plot.tag = element_text(face = "bold", size = 18))
e2040_4M <- e2040_4M + labs(tag = "g") + theme(plot.tag = element_text(face = "bold", size = 18))
e2040_5L <- e2040_5L + labs(tag = "h") + theme(plot.tag = element_text(face = "bold", size = 18))
biotecener_treemap_2040 <- wrap_plots(list(e2040_1H, e2040_2M, e2040_4M, e2040_5L), ncol = 2) +
  plot_annotation()

Plot2_SI_2040 <- plot_grid(
  region_ener_2040, 
  biotecener_treemap_2040, 
  nrow = 2,
  rel_heights = c(1, 0.5)  # abc 子图的宽度为原来的 0.5，def 子图保持宽度为 1
)

Plot2_SI_2040 <- Plot2_SI_2040 + 
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "in"))

ggsave("Plot2_SI_2040.jpeg", Plot2_SI_2040, width = 16, height = 20, dpi = 300)  # 保存为PDF文件

#over




#fig3 abcd-----------------
library(ggplot2)
library(readxl)
library(ggrepel)
library(cowplot)

file_path21 <- "/.../bubbletechenvironment.xlsx"
df <- read_excel(file_path21, sheet = 1)

df$情景 <- factor(df$情景, levels = c("SSP1-H", "SSP1-M", "SSP2-M", "SSP2-L", "SSP4-M", "SSP5-L"))

size_limits <- range(abs(df$`总环境影响潜值`), na.rm = TRUE)

plots <- list()

p_legend <- ggplot(df[df$情景 == "SSP1-H", ], aes(x = `技术能源化潜力`, y = `技术应用比例`, size = abs(`总环境影响潜值`), color = `技术名称`, stroke = ifelse(`总环境影响潜值` > 0, 1.5, 0))) + 
  geom_point(alpha = 0.5, fill = "lightgray", shape = 21, color = "black") +  # 将气泡填充为浅灰色
  scale_size_continuous(
    range = c(3, 20), 
    limits = size_limits, 
    guide = guide_legend(order = 1, direction = "vertical")  # 设置图例为水平放置
  ) +
  scale_y_continuous(limits = c(0, 1)) +  # 设置纵坐标范围
  scale_x_continuous(limits = c(0, 13)) +  # 设置横坐标范围
  labs(size = "TEI per technology") +  # 只保留气泡大小的图例
  theme_bw() +
  theme(
    legend.position = "right", 
    legend.title = element_text(size = 12),  # 调整图例标题字体大小
    legend.text = element_text(size = 10),  # 调整图例标签字体大小
    legend.key = element_rect(fill = "white", color = NA),  # 去掉图例背景
    legend.key.size = unit(1, "lines")  # 调整图例大小
  )

legend <- get_legend(p_legend)

custom_labels <- c(
  "CHP_AR" = "CHP(CRs)", "BF_AR" = "BF(CRs)", "Bg_AR" = "Bg(CRs)", "Be_AR" = "Be(CRs)",
  "BP_AR" = "BP(CRs)", "DH_AR" = "DH(CRs)", "CFPG_AR" = "CFPG (CRs)", "BgPG_LR" = "BgPG(LEs)",
  "Cp_LR" = "Cp(LEs)", "Bg_LR" = "Bg(LEs)", "GPG_WR" = "GPG(FRs)", "BF_WR" = "BF(FRs)",
  "CFPG_WR" = "CFPG(FRs)", "RJF_WR" = "RJF(FRs)"
)
custom_colors <- c(
  "CHP_AR" = "#4CAF50", "BF_AR" = "#E91E63", "Bg_AR" = "#03A9F4", "Be_AR" = "#9C27B0",
  "BP_AR" = "#FFC107", "DH_AR" = "#FF9800", "CFPG_AR" = "#673AB7", "BgPG_LR" = "#8BC34A",
  "Cp_LR" = "#9E9E9E", "Bg_LR" = "#00BCD4", "GPG_WR" = "#4CAF50", "BF_WR" = "#F44336",
  "CFPG_WR" = "#673AB7", "RJF_WR" = "#E91E63"
)
for(scenario in levels(df$情景)) {
  p <- ggplot(df[df$情景 == scenario, ], aes(x = `技术能源化潜力`, y = `技术应用比例`, size = abs(`总环境影响潜值`), color = `技术名称`, stroke = ifelse(`总环境影响潜值` > 0, 1.5, 0))) + 
    geom_point(alpha = 0.5) +
    geom_text_repel(aes(label = custom_labels[`技术名称`]), size = 3, max.overlaps = 10, nudge_x = 0.02, nudge_y = 0.02, hjust = 0, vjust = 1) +  # 使用自定义标签
    scale_size_continuous(range = c(3, 20), limits = size_limits) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +  # 设置纵坐标范围并显示为百分数
    scale_color_manual(values = custom_colors) +  # 使用自定义颜色
    labs(
      x = "Biomass allocated to technologies (EJ)",
      y = "Technology share",
      size = "Total Environmental Impact",
      color = "Bio-technology"
    ) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),  # 去掉网格
      axis.text.x = element_text(size = 15, color = "black"),  # 设置x轴标注字体大小
      axis.text.y = element_text(size = 15, color = "black"),  # 设置y轴标注字体大小
      axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      legend.position = "none",  # 移除图例
      plot.title = element_blank()  # 移除默认标题
    ) +
    annotate(
      "text", x = 0, y = 1, 
      label = paste(scenario), size = 5, hjust = 0, vjust = 1, fontface = "italic"
    )  # 在图片内部左上角添加标题
  
  plots[[scenario]] <- p
}

pbubble1H <- plots[["SSP1-H"]]
pbubble2M <- plots[["SSP2-M"]]
pbubble4M <- plots[["SSP4-M"]]
pbubble5L <- plots[["SSP5-L"]]

library(grid)  

pbubble1H_with_legend <- pbubble1H +
  annotation_custom(
    grob = legend,
    xmin = 30, xmax = 32,  # 调整横坐标范围，根据图的尺寸设置位置
    ymin = 0.02, ymax = 0.2  # 调整纵坐标范围，根据图的尺寸设置位置
  )


#fig3e-----
#环境影响分种类分地区数据
file_path3e <- "/.../environmentregion.xlsx"
globalenvir <- read_excel(file_path3e, sheet = 1)
globalenvir1 <- globalenvir %>% group_by(region, scenario) %>%
  summarise(
    EnerHeatvalue_EJ = sum(EnerHeatvalue_EJ),
    BR_ener = sum(BR_ener),
    `Global Warming` = sum(`Global Warming`),
    `Stratospheric Ozone Depletion` = sum(`Stratospheric Ozone Depletion`),
    `Photochemical Ozone Formation-human damage` = sum(`Photochemical Ozone Formation-human damage`),
    `Fine Particulate Matter Formation` = sum(`Fine Particulate Matter Formation`),
    `Photochemical Ozone Formation-ecosystem damage` = sum(`Photochemical Ozone Formation-ecosystem damage`),
    `Terrestrial Acidification` = sum(`Terrestrial Acidification`),
    `Freshwater Eutrophication` = sum(`Freshwater Eutrophication`),
    `Marine Eutrophication` = sum(`Marine Eutrophication`),
    `Fossil Resource Scarcity` = sum(`Fossil Resource Scarcity`),
    `total environmental impact` = sum(`total environmental impact`)
  )
library(dplyr)

globalenvirSSP1H <- globalenvir1 %>% 
  filter(scenario == "SSP1-H")

library(ggplot2)
library(tidyr)
library(dplyr)

df_long <- globalenvirSSP1H %>%
  pivot_longer(cols = c("Global Warming", "Stratospheric Ozone Depletion", "Photochemical Ozone Formation-human damage",
                        "Photochemical Ozone Formation-ecosystem damage", "Fine Particulate Matter Formation", "Terrestrial Acidification", 
                        "Freshwater Eutrophication", "Marine Eutrophication", "Fossil Resource Scarcity"),
               names_to = "Environmental Impact", values_to = "Value")

impact_abbreviations <- c(
  "Global Warming" = "GW",
  "Stratospheric Ozone Depletion" = "SOD",
  "Photochemical Ozone Formation-human damage" = "POFH",
  "Photochemical Ozone Formation-ecosystem damage" = "POFE",
  "Fine Particulate Matter Formation" = "FPMF",
  "Terrestrial Acidification" = "TA",
  "Freshwater Eutrophication" = "FE",
  "Marine Eutrophication" = "ME",
  "Fossil Resource Scarcity" = "FRS"
)

region_abbreviations <- c(
  "USA" = "USA",
  "Taiwan" = "TWN",
  "Southeast Asia" = "SEA",
  "South Korea" = "KOR",
  "South Asia" = "SAS",
  "South America_Southern" = "SAMS",
  "South America_Northern" = "SAMN",
  "South Africa" = "ZAF",
  "Russia" = "RUS",
  "Pakistan" = "PAK",
  "Middle East" = "MEA",
  "Mexico" = "MEX",
  "Japan" = "JPN",
  "Indonesia" = "IDN",
  "India" = "IND",
  "European Free Trade Association" = "EFTA",
  "Europe_Non_EU" = "NEU",
  "Europe_Eastern" = "EEU",
  "EU-15" = "EU15",
  "EU-12" = "EU12",
  "Colombia" = "COL",
  "China" = "CHN",
  "Central Asia" = "CAS",
  "Central America and Caribbean" = "CAC",
  "Canada" = "CAN",
  "Brazil" = "BRA",
  "Australia_NZ" = "ANZ",
  "Argentina" = "ARG",
  "Africa_Western" = "WAF",
  "Africa_Southern" = "SAF",
  "Africa_Northern" = "NAF",
  "Africa_Eastern" = "EAF"
)

region_order <- c("EAF", "NAF", "SAF", "WAF", "ZAF", 
                  "CAS", "CHN", "IDN", "IND", "JPN", "KOR", "MEA", "PAK", "SAS", "SEA", "TWN",
                  "EEU", "EFTA", "EU12", "EU15", "NEU", "RUS",
                  "CAC", "CAN", "MEX", "USA",
                  "ANZ",
                  "ARG", "BRA", "COL", "SAMN","SAMS"
)

df_long <- df_long %>%
  mutate(region = factor(region, levels = names(region_abbreviations), labels = region_abbreviations),
         region = factor(region, levels = region_order),
         `Environmental Impact` = factor(`Environmental Impact`, 
                                         levels = names(impact_abbreviations), 
                                         labels = impact_abbreviations))
df_long$Value <- -df_long$Value

pregionalSSP1H <- ggplot(df_long, aes(x = region, y = `Environmental Impact`, fill = Value)) +
  geom_tile(color = "black", size = 0.5) +  # 每个方块用灰色边框分隔
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "Environmental Impact", limits = c(-1e+7, 3e+8), alpha = 0.6) +  # 使用 "viridis" 色系
  theme_minimal() +  # 使用简约主题
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, color = "black"),  # 旋转横轴标签并设置字体大小
    axis.text.y = element_text(size = 14, color = "black"),  # 设置纵轴标签字体大小
    axis.title.x = element_blank(),  # 去掉横轴标题
    axis.title.y = element_blank(),  # 去掉纵轴标题
    plot.title = element_blank(),  # 去掉图的标题
    panel.grid = element_blank()
  )

library(cowplot)

top_plots <- plot_grid(
  pbubble1H_with_legend + labs(tag = "a") + theme(plot.tag = element_text(face = "bold", size = 20)), 
  pbubble2M + labs(tag = "b") + theme(plot.tag = element_text(face = "bold", size = 20)), 
  pbubble4M + labs(tag = "c") + theme(plot.tag = element_text(face = "bold", size = 20)), 
  pbubble5L + labs(tag = "d") + theme(plot.tag = element_text(face = "bold", size = 20)), 
  ncol = 2  # 每行两列
)

final_plot3 <- plot_grid(
  top_plots, 
  pregionalSSP1H + labs(tag = "e") + theme(plot.tag = element_text(face = "bold", size = 20)), 
  ncol = 1, 
  rel_heights = c(2, 0.5)  
)

ggsave("Fig3.jpeg", final_plot3, width = 16, height = 20, dpi = 300)

#Fig4------------------
library(ggplot2)  
library(readxl)  
library(dplyr)  
library(tidyr)  
library(patternplot)

filepath_ghg <- "/.../GHGtechregion.xlsx"
df_ghg <- read_excel(filepath_ghg, sheet = 1)  
df_ghg_ok <- df_ghg  %>%
  group_by(scenario, region, Year) %>%
  summarise(`CHP(CRs)` = sum(`CHP(CRs)`),
            `BF(CRs)`	= sum(`BF(CRs)`),
            `Bg(CRs)`	= sum(`Bg(CRs)`),
            `Be(CRs)`	= sum(`Be(CRs)`),
            `BP(CRs)`	= sum(`BP(CRs)`),
            `DH(CRs)`	= sum(`DH(CRs)`),
            `CFPG(CRs)`	= sum(`CFPG(CRs)`),
            `BgPG(LEs)`	= sum(`BgPG(LEs)`),
            `Cp(LEs)`	= sum(`Cp(LEs)`),
            `Bg(LEs)`	= sum(`Bg(LEs)`),
            `GPG(FRs)` = sum(`GPG(FRs)`),
            `BF(FRs)`	= sum(`BF(FRs)`),
            `CFPG(FRs)`	= sum(`CFPG(FRs)`),
            `RJF(FRs)`= sum(`RJF(FRs)`)
  )

df_long <- df_ghg_ok %>%
  pivot_longer(cols = -c(scenario, region, Year), 
               names_to = "Technology", 
               values_to = "GHG_Reduction")

region_abbreviations <- c(
  "USA" = "USA",
  "Taiwan" = "TWN",
  "Southeast Asia" = "SEA",
  "South Korea" = "KOR",
  "South Asia" = "SAS",
  "South America_Southern" = "SAMS",
  "South America_Northern" = "SAMN",
  "South Africa" = "ZAF",
  "Russia" = "RUS",
  "Pakistan" = "PAK",
  "Middle East" = "MEA",
  "Mexico" = "MEX",
  "Japan" = "JPN",
  "Indonesia" = "IDN",
  "India" = "IND",
  "European Free Trade Association" = "EFTA",
  "Europe_Non_EU" = "NEU",
  "Europe_Eastern" = "EEU",
  "EU-15" = "EU15",
  "EU-12" = "EU12",
  "Colombia" = "COL",
  "China" = "CHN",
  "Central Asia" = "CAS",
  "Central America and Caribbean" = "CAC",
  "Canada" = "CAN",
  "Brazil" = "BRA",
  "Australia_NZ" = "ANZ",
  "Argentina" = "ARG",
  "Africa_Western" = "WAF",
  "Africa_Southern" = "SAF",
  "Africa_Northern" = "NAF",
  "Africa_Eastern" = "EAF"
)

region_order <- c("EAF", "NAF", "SAF", "WAF", "ZAF", 
                  "CAS", "CHN", "IDN", "IND", "JPN", "KOR", "MEA", "PAK", "SAS", "SEA", "TWN",
                  "EEU", "EFTA", "EU12", "EU15", "NEU", "RUS",
                  "CAC", "CAN", "MEX", "USA",
                  "ANZ",
                  "ARG", "BRA", "COL", "SAMN","SAMS"
)
df_long <- df_long %>%
  mutate(region = factor(region, levels = names(region_abbreviations), labels = region_abbreviations),
         region = factor(region, levels = region_order))

custom_colors <- c(
  "CHP(CRs)" = "#ff7f0e", "BF(CRs)" = "#d62728", "Bg(CRs)" = "#2ca02c", "Be(CRs)" = "#aec7e8",
  "BP(CRs)" = "#9467bd", "DH(CRs)" = "#8c564b", "CFPG(CRs)" = "#e377c2", "BgPG(LEs)" = "#7f7f7f",
  "Cp(LEs)" = "#bcbd22", "Bg(LEs)" = "#17becf", "GPG(FRs)" = "#ffbb78", "BF(FRs)" = "#1f77b4",
  "CFPG(FRs)" = "#98df8a", "RJF(FRs)" = "#ff9896"
)

technology_order <- c("CHP(CRs)", "BF(CRs)", "Bg(CRs)", "Be(CRs)", "BP(CRs)", 
                      "DH(CRs)", "CFPG(CRs)", "BgPG(LEs)", "Cp(LEs)", "Bg(LEs)",
                      "GPG(FRs)", "BF(FRs)", "CFPG(FRs)", "RJF(FRs)")
df_long <- df_long %>%
  mutate(Technology = factor(Technology, levels = technology_order))
line_types <- c(
  "CHP(CRs)" = 2, "BF(CRs)" = 2, "Bg(CRs)" = 2, "Be(CRs)" = 2,
  "BP(CRs)" = 2, "DH(CRs)" = 2, "CFPG(CRs)" = 2, "BgPG(LEs)" = 1,
  "Cp(LEs)" = 1, "Bg(LEs)" = 1, "GPG(FRs)" = 3, "BF(FRs)" = 3,
  "CFPG(FRs)" = 3, "RJF(FRs)" = 3)

label_data <- data.frame(
  x = c(1, 6, 17.3, 23.8, 27, 29.2),
  y = rep(800, 6),  
  label = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
)

custom_labels <- c(
  "CHP(CRs)" = "Combined heat and power (CRs)",
  "BF(CRs)" = "Briquette fuel (CRs)",
  "Bg(CRs)" = "Biogas (CRs)",
  "Be(CRs)" = "Bioethanol (CRs)",
  "BP(CRs)" = "Biopyrolysis (CRs)",
  "DH(CRs)" = "Direct burning for heat (CRs)",
  "CFPG(CRs)" = "Co-firing for power generation (CRs)",
  "BgPG(LEs)" = "Biogas for power generation (LEs)",
  "Cp(LEs)" = "Composting (LEs)",
  "Bg(LEs)" = "Biogas (LEs)",
  "GPG(FRs)" = "Gasfication power generation (FRs)",
  "BF(FRs)" = "Briquette fuel (FRs)",
  "CFPG(FRs)" = "Co-firing for power generation (FRs)",
  "RJF(FRs)" = "Renewable jet fuel (FRs)"
)

ghgplotssp1H <- ggplot(df_long, aes(x = region, y = GHG_Reduction, fill = Technology, linetype = Technology)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.6) +  
  geom_vline(xintercept = c(5.5, 16.5, 22.5, 26.5, 27.5), color = "gray40", linetype = "dashed", linewidth = 0.5) +  # 添加灰色竖线分隔
  geom_text(data = label_data, aes(x = x, y = y, label = label), 
            color = "gray40", size = 6, fontface = "bold.italic", inherit.aes = FALSE) +  # 使用数据框添加区域名称标注
  scale_fill_manual(values = custom_colors, labels = custom_labels) +  # 使用自定义颜色和图例标签
  scale_linetype_manual(values = line_types, labels = custom_labels) +  # 使用自定义线型和图例标签
  facet_wrap(~ Year, ncol = 1) +  # 根据年份创建三个子图
  labs(y = "GHGs mitigation (Mt)") +  
  theme_bw(base_size = 26) +  
  theme(
    panel.grid = element_blank(),  # 去掉网格
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),  # 设置x轴刻度标注为黑色并加粗
    axis.text.y = element_text(color = "black"),  # 设置y轴刻度标注为黑色
    axis.title.x = element_blank(),  # 去掉x轴标题
    legend.position = "bottom",  # 将图例放在图的下方
    legend.title = element_blank(),  # 去掉图例的名称
    legend.direction = "horizontal",  # 将图例水平排列
    legend.box = "horizontal",  # 将图例框设置为水平
    legend.spacing.x = unit(0.5, 'cm'),  # 调整图例项之间的间距
    legend.key.width = unit(2, "lines"),  # 调整图例项的宽度
    strip.text = element_text(face = "bold.italic", size = 20),  # 设置分面内字体为加粗斜体，并增大字号
    strip.background = element_rect(color = "white", fill = "grey90", size = 0),  # 设置分面边框颜色和填充颜色
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)  # 设置图的边框颜色和粗细
    ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE), linetype = guide_legend(nrow = 2, byrow = TRUE))  # 设置图例为3行

ggsave("Plots4.pdf", ghgplotssp1H, width = 25, height = 16)
ggsave("Plots4.jpeg", ghgplotssp1H, width = 25, height = 16, dpi = 300)

#over

#Fig4-SI----
#SSP1-M
df_ghg <- read_excel(filepath_ghg, sheet = 2)  
df_ghg_ok <- df_ghg  %>%
  group_by(scenario, region, Year) %>%
  summarise(`CHP(CRs)` = sum(`CHP(CRs)`), `BF(CRs)`	= sum(`BF(CRs)`), `Bg(CRs)`	= sum(`Bg(CRs)`), `Be(CRs)`	= sum(`Be(CRs)`),
            `BP(CRs)`	= sum(`BP(CRs)`), `DH(CRs)`	= sum(`DH(CRs)`), `CFPG(CRs)`	= sum(`CFPG(CRs)`), `BgPG(LEs)`	= sum(`BgPG(LEs)`),
            `Cp(LEs)`	= sum(`Cp(LEs)`), `Bg(LEs)`	= sum(`Bg(LEs)`),`GPG(FRs)` = sum(`GPG(FRs)`), `BF(FRs)`	= sum(`BF(FRs)`),
            `CFPG(FRs)`	= sum(`CFPG(FRs)`),`RJF(FRs)`= sum(`RJF(FRs)`))
df_long <- df_ghg_ok %>% 
  pivot_longer(cols = -c(scenario, region, Year), 
               names_to = "Technology", 
               values_to = "GHG_Reduction")
df_long <- df_long %>% mutate(region = factor(region, levels = names(region_abbreviations), labels = region_abbreviations),
         region = factor(region, levels = region_order))
df_long <- df_long %>% mutate(Technology = factor(Technology, levels = technology_order))
label_data <- data.frame(
  x = c(1, 6, 17.3, 23.8, 27, 29.2), y = rep(500, 6), 
  label = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
)
ghgplotssp1M <- ggplot(df_long, aes(x = region, y = GHG_Reduction, fill = Technology, linetype = Technology)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.6) +  # 堆叠柱状图，带边框
  geom_vline(xintercept = c(5.5, 16.5, 22.5, 26.5, 27.5), color = "gray40", linetype = "dashed", linewidth = 0.5) +  # 添加灰色竖线分隔
  geom_text(data = label_data, aes(x = x, y = y, label = label), 
            color = "gray40", size = 6, fontface = "bold.italic", inherit.aes = FALSE) +  # 使用数据框添加区域名称标注
  scale_fill_manual(values = custom_colors, labels = custom_labels) +  # 使用自定义颜色和图例标签
  scale_linetype_manual(values = line_types, labels = custom_labels) +  # 使用自定义线型和图例标签
  facet_wrap(~ Year, ncol = 1) +  # 根据年份创建三个子图
  labs(y = "GHGs mitigation (Mt)") +  # 仅设置y轴标签，去掉x轴标签
  theme_bw(base_size = 26) +  # 简约主题，设置字体大小
  theme(
    panel.grid = element_blank(),  # 去掉网格
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),  # 设置x轴刻度标注为黑色并加粗
    axis.text.y = element_text(color = "black"),  # 设置y轴刻度标注为黑色
    axis.title.x = element_blank(),  # 去掉x轴标题
    legend.position = "bottom",  # 将图例放在图的下方
    legend.title = element_blank(),  # 去掉图例的名称
    legend.direction = "horizontal",  # 将图例水平排列
    legend.box = "horizontal",  # 将图例框设置为水平
    legend.spacing.x = unit(0.5, 'cm'),  # 调整图例项之间的间距
    legend.key.width = unit(2, "lines"),  # 调整图例项的宽度
    strip.text = element_text(face = "bold.italic", size = 20),  # 设置分面内字体为加粗斜体，并增大字号
    strip.background = element_rect(color = "white", fill = "grey90", size = 0),  # 设置分面边框颜色和填充颜色
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)  # 设置图的边框颜色和粗细
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE), linetype = guide_legend(nrow = 2, byrow = TRUE))  # 设置图例为两行
ggsave("Plots4_SI_1M.jpeg", ghgplotssp1M, width = 25, height = 16, dpi = 300)
#over

#SSP2-M
df_ghg <- read_excel(filepath_ghg, sheet = 3)  # 读取数据
df_ghg_ok <- df_ghg  %>%
  group_by(scenario, region, Year) %>%
  summarise(`CHP(CRs)` = sum(`CHP(CRs)`), `BF(CRs)`	= sum(`BF(CRs)`), `Bg(CRs)`	= sum(`Bg(CRs)`), `Be(CRs)`	= sum(`Be(CRs)`),
            `BP(CRs)`	= sum(`BP(CRs)`), `DH(CRs)`	= sum(`DH(CRs)`), `CFPG(CRs)`	= sum(`CFPG(CRs)`), `BgPG(LEs)`	= sum(`BgPG(LEs)`),
            `Cp(LEs)`	= sum(`Cp(LEs)`), `Bg(LEs)`	= sum(`Bg(LEs)`),`GPG(FRs)` = sum(`GPG(FRs)`), `BF(FRs)`	= sum(`BF(FRs)`),
            `CFPG(FRs)`	= sum(`CFPG(FRs)`),`RJF(FRs)`= sum(`RJF(FRs)`))
# 将数据转为长格式
df_long <- df_ghg_ok %>% 
  pivot_longer(cols = -c(scenario, region, Year), 
               names_to = "Technology", 
               values_to = "GHG_Reduction")
# 应用地区的缩写和排序
df_long <- df_long %>% mutate(region = factor(region, levels = names(region_abbreviations), labels = region_abbreviations),
                              region = factor(region, levels = region_order))
# 应用技术的排序
df_long <- df_long %>% mutate(Technology = factor(Technology, levels = technology_order))
# 创建区域名称标注的数据框
label_data <- data.frame(
  x = c(1, 6, 17.3, 23.8, 27, 29.2), y = rep(500, 6),  # 在每个面板上放置相同的y位置
  label = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
)
# 绘制堆叠柱状图
ghgplotssp2M <- ggplot(df_long, aes(x = region, y = GHG_Reduction, fill = Technology, linetype = Technology)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.6) +  # 堆叠柱状图，带边框
  geom_vline(xintercept = c(5.5, 16.5, 22.5, 26.5, 27.5), color = "gray40", linetype = "dashed", linewidth = 0.5) +  # 添加灰色竖线分隔
  geom_text(data = label_data, aes(x = x, y = y, label = label), 
            color = "gray40", size = 6, fontface = "bold.italic", inherit.aes = FALSE) +  # 使用数据框添加区域名称标注
  scale_fill_manual(values = custom_colors, labels = custom_labels) +  # 使用自定义颜色和图例标签
  scale_linetype_manual(values = line_types, labels = custom_labels) +  # 使用自定义线型和图例标签
  facet_wrap(~ Year, ncol = 1) +  # 根据年份创建三个子图
  labs(y = "GHGs mitigation (Mt)") +  # 仅设置y轴标签，去掉x轴标签
  theme_bw(base_size = 26) +  # 简约主题，设置字体大小
  theme(
    panel.grid = element_blank(),  # 去掉网格
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),  # 设置x轴刻度标注为黑色并加粗
    axis.text.y = element_text(color = "black"),  # 设置y轴刻度标注为黑色
    axis.title.x = element_blank(),  # 去掉x轴标题
    legend.position = "bottom",  # 将图例放在图的下方
    legend.title = element_blank(),  # 去掉图例的名称
    legend.direction = "horizontal",  # 将图例水平排列
    legend.box = "horizontal",  # 将图例框设置为水平
    legend.spacing.x = unit(0.5, 'cm'),  # 调整图例项之间的间距
    legend.key.width = unit(2, "lines"),  # 调整图例项的宽度
    strip.text = element_text(face = "bold.italic", size = 20),  # 设置分面内字体为加粗斜体，并增大字号
    strip.background = element_rect(color = "white", fill = "grey90", size = 0),  # 设置分面边框颜色和填充颜色
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)  # 设置图的边框颜色和粗细
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE), linetype = guide_legend(nrow = 2, byrow = TRUE))  # 设置图例为两行
ggsave("Plots4_SI_2M.jpeg", ghgplotssp2M, width = 25, height = 16, dpi = 300)
#over

#SSP2-L
df_ghg <- read_excel(filepath_ghg, sheet = 4)  # 读取数据
df_ghg_ok <- df_ghg  %>%
  group_by(scenario, region, Year) %>%
  summarise(`CHP(CRs)` = sum(`CHP(CRs)`), `BF(CRs)`	= sum(`BF(CRs)`), `Bg(CRs)`	= sum(`Bg(CRs)`), `Be(CRs)`	= sum(`Be(CRs)`),
            `BP(CRs)`	= sum(`BP(CRs)`), `DH(CRs)`	= sum(`DH(CRs)`), `CFPG(CRs)`	= sum(`CFPG(CRs)`), `BgPG(LEs)`	= sum(`BgPG(LEs)`),
            `Cp(LEs)`	= sum(`Cp(LEs)`), `Bg(LEs)`	= sum(`Bg(LEs)`),`GPG(FRs)` = sum(`GPG(FRs)`), `BF(FRs)`	= sum(`BF(FRs)`),
            `CFPG(FRs)`	= sum(`CFPG(FRs)`),`RJF(FRs)`= sum(`RJF(FRs)`))
# 将数据转为长格式
df_long <- df_ghg_ok %>% 
  pivot_longer(cols = -c(scenario, region, Year), 
               names_to = "Technology", 
               values_to = "GHG_Reduction")
# 应用地区的缩写和排序
df_long <- df_long %>% mutate(region = factor(region, levels = names(region_abbreviations), labels = region_abbreviations),
                              region = factor(region, levels = region_order))
# 应用技术的排序
df_long <- df_long %>% mutate(Technology = factor(Technology, levels = technology_order))
# 创建区域名称标注的数据框
label_data <- data.frame(
  x = c(1, 6, 17.3, 23.8, 27, 29.2), y = rep(250, 6),  # 在每个面板上放置相同的y位置
  label = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
)
# 绘制堆叠柱状图
ghgplotssp2L <- ggplot(df_long, aes(x = region, y = GHG_Reduction, fill = Technology, linetype = Technology)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.6) +  # 堆叠柱状图，带边框
  geom_vline(xintercept = c(5.5, 16.5, 22.5, 26.5, 27.5), color = "gray40", linetype = "dashed", linewidth = 0.5) +  # 添加灰色竖线分隔
  geom_text(data = label_data, aes(x = x, y = y, label = label), 
            color = "gray40", size = 6, fontface = "bold.italic", inherit.aes = FALSE) +  # 使用数据框添加区域名称标注
  scale_fill_manual(values = custom_colors, labels = custom_labels) +  # 使用自定义颜色和图例标签
  scale_linetype_manual(values = line_types, labels = custom_labels) +  # 使用自定义线型和图例标签
  facet_wrap(~ Year, ncol = 1) +  # 根据年份创建三个子图
  labs(y = "GHGs mitigation (Mt)") +  # 仅设置y轴标签，去掉x轴标签
  theme_bw(base_size = 26) +  # 简约主题，设置字体大小
  theme(
    panel.grid = element_blank(),  # 去掉网格
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),  # 设置x轴刻度标注为黑色并加粗
    axis.text.y = element_text(color = "black"),  # 设置y轴刻度标注为黑色
    axis.title.x = element_blank(),  # 去掉x轴标题
    legend.position = "bottom",  # 将图例放在图的下方
    legend.title = element_blank(),  # 去掉图例的名称
    legend.direction = "horizontal",  # 将图例水平排列
    legend.box = "horizontal",  # 将图例框设置为水平
    legend.spacing.x = unit(0.5, 'cm'),  # 调整图例项之间的间距
    legend.key.width = unit(2, "lines"),  # 调整图例项的宽度
    strip.text = element_text(face = "bold.italic", size = 20),  # 设置分面内字体为加粗斜体，并增大字号
    strip.background = element_rect(color = "white", fill = "grey90", size = 0),  # 设置分面边框颜色和填充颜色
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)  # 设置图的边框颜色和粗细
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE), linetype = guide_legend(nrow = 2, byrow = TRUE))  # 设置图例为两行
ggsave("Plots4_SI_2L.jpeg", ghgplotssp2L, width = 25, height = 16, dpi = 300)
#over

#SSP4-M
df_ghg <- read_excel(filepath_ghg, sheet = 5)  # 读取数据
df_ghg_ok <- df_ghg  %>%
  group_by(scenario, region, Year) %>%
  summarise(`CHP(CRs)` = sum(`CHP(CRs)`), `BF(CRs)`	= sum(`BF(CRs)`), `Bg(CRs)`	= sum(`Bg(CRs)`), `Be(CRs)`	= sum(`Be(CRs)`),
            `BP(CRs)`	= sum(`BP(CRs)`), `DH(CRs)`	= sum(`DH(CRs)`), `CFPG(CRs)`	= sum(`CFPG(CRs)`), `BgPG(LEs)`	= sum(`BgPG(LEs)`),
            `Cp(LEs)`	= sum(`Cp(LEs)`), `Bg(LEs)`	= sum(`Bg(LEs)`),`GPG(FRs)` = sum(`GPG(FRs)`), `BF(FRs)`	= sum(`BF(FRs)`),
            `CFPG(FRs)`	= sum(`CFPG(FRs)`),`RJF(FRs)`= sum(`RJF(FRs)`))
# 将数据转为长格式
df_long <- df_ghg_ok %>% 
  pivot_longer(cols = -c(scenario, region, Year), 
               names_to = "Technology", 
               values_to = "GHG_Reduction")
# 应用地区的缩写和排序
df_long <- df_long %>% mutate(region = factor(region, levels = names(region_abbreviations), labels = region_abbreviations),
                              region = factor(region, levels = region_order))
# 应用技术的排序
df_long <- df_long %>% mutate(Technology = factor(Technology, levels = technology_order))
# 创建区域名称标注的数据框
label_data <- data.frame(
  x = c(1, 6, 17.3, 23.8, 27, 29.2), y = rep(500, 6),  # 在每个面板上放置相同的y位置
  label = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
)
# 绘制堆叠柱状图
ghgplotssp4M <- ggplot(df_long, aes(x = region, y = GHG_Reduction, fill = Technology, linetype = Technology)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.6) +  # 堆叠柱状图，带边框
  geom_vline(xintercept = c(5.5, 16.5, 22.5, 26.5, 27.5), color = "gray40", linetype = "dashed", linewidth = 0.5) +  # 添加灰色竖线分隔
  geom_text(data = label_data, aes(x = x, y = y, label = label), 
            color = "gray40", size = 6, fontface = "bold.italic", inherit.aes = FALSE) +  # 使用数据框添加区域名称标注
  scale_fill_manual(values = custom_colors, labels = custom_labels) +  # 使用自定义颜色和图例标签
  scale_linetype_manual(values = line_types, labels = custom_labels) +  # 使用自定义线型和图例标签
  facet_wrap(~ Year, ncol = 1) +  # 根据年份创建三个子图
  labs(y = "GHGs mitigation (Mt)") +  # 仅设置y轴标签，去掉x轴标签
  theme_bw(base_size = 26) +  # 简约主题，设置字体大小
  theme(
    panel.grid = element_blank(),  # 去掉网格
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),  # 设置x轴刻度标注为黑色并加粗
    axis.text.y = element_text(color = "black"),  # 设置y轴刻度标注为黑色
    axis.title.x = element_blank(),  # 去掉x轴标题
    legend.position = "bottom",  # 将图例放在图的下方
    legend.title = element_blank(),  # 去掉图例的名称
    legend.direction = "horizontal",  # 将图例水平排列
    legend.box = "horizontal",  # 将图例框设置为水平
    legend.spacing.x = unit(0.5, 'cm'),  # 调整图例项之间的间距
    legend.key.width = unit(2, "lines"),  # 调整图例项的宽度
    strip.text = element_text(face = "bold.italic", size = 20),  # 设置分面内字体为加粗斜体，并增大字号
    strip.background = element_rect(color = "white", fill = "grey90", size = 0),  # 设置分面边框颜色和填充颜色
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)  # 设置图的边框颜色和粗细
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE), linetype = guide_legend(nrow = 2, byrow = TRUE))  # 设置图例为两行
ggsave("Plots4_SI_4M.jpeg", ghgplotssp4M, width = 25, height = 16, dpi = 300)
#over

#SSP5-L
df_ghg <- read_excel(filepath_ghg, sheet = 6)  # 读取数据
df_ghg_ok <- df_ghg  %>%
  group_by(scenario, region, Year) %>%
  summarise(`CHP(CRs)` = sum(`CHP(CRs)`), `BF(CRs)`	= sum(`BF(CRs)`), `Bg(CRs)`	= sum(`Bg(CRs)`), `Be(CRs)`	= sum(`Be(CRs)`),
            `BP(CRs)`	= sum(`BP(CRs)`), `DH(CRs)`	= sum(`DH(CRs)`), `CFPG(CRs)`	= sum(`CFPG(CRs)`), `BgPG(LEs)`	= sum(`BgPG(LEs)`),
            `Cp(LEs)`	= sum(`Cp(LEs)`), `Bg(LEs)`	= sum(`Bg(LEs)`),`GPG(FRs)` = sum(`GPG(FRs)`), `BF(FRs)`	= sum(`BF(FRs)`),
            `CFPG(FRs)`	= sum(`CFPG(FRs)`),`RJF(FRs)`= sum(`RJF(FRs)`))
# 将数据转为长格式
df_long <- df_ghg_ok %>% 
  pivot_longer(cols = -c(scenario, region, Year), 
               names_to = "Technology", 
               values_to = "GHG_Reduction")
# 应用地区的缩写和排序
df_long <- df_long %>% mutate(region = factor(region, levels = names(region_abbreviations), labels = region_abbreviations),
                              region = factor(region, levels = region_order))
# 应用技术的排序
df_long <- df_long %>% mutate(Technology = factor(Technology, levels = technology_order))
# 创建区域名称标注的数据框
label_data <- data.frame(
  x = c(1, 6, 17.3, 23.8, 27, 29.2), y = rep(300, 6),  # 在每个面板上放置相同的y位置
  label = c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
)
# 绘制堆叠柱状图
ghgplotssp5L <- ggplot(df_long, aes(x = region, y = GHG_Reduction, fill = Technology, linetype = Technology)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.6) +  # 堆叠柱状图，带边框
  geom_vline(xintercept = c(5.5, 16.5, 22.5, 26.5, 27.5), color = "gray40", linetype = "dashed", linewidth = 0.5) +  # 添加灰色竖线分隔
  geom_text(data = label_data, aes(x = x, y = y, label = label), 
            color = "gray40", size = 6, fontface = "bold.italic", inherit.aes = FALSE) +  # 使用数据框添加区域名称标注
  scale_fill_manual(values = custom_colors, labels = custom_labels) +  # 使用自定义颜色和图例标签
  scale_linetype_manual(values = line_types, labels = custom_labels) +  # 使用自定义线型和图例标签
  facet_wrap(~ Year, ncol = 1) +  # 根据年份创建三个子图
  labs(y = "GHGs mitigation (Mt)") +  # 仅设置y轴标签，去掉x轴标签
  theme_bw(base_size = 26) +  # 简约主题，设置字体大小
  theme(
    panel.grid = element_blank(),  # 去掉网格
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),  # 设置x轴刻度标注为黑色并加粗
    axis.text.y = element_text(color = "black"),  # 设置y轴刻度标注为黑色
    axis.title.x = element_blank(),  # 去掉x轴标题
    legend.position = "bottom",  # 将图例放在图的下方
    legend.title = element_blank(),  # 去掉图例的名称
    legend.direction = "horizontal",  # 将图例水平排列
    legend.box = "horizontal",  # 将图例框设置为水平
    legend.spacing.x = unit(0.5, 'cm'),  # 调整图例项之间的间距
    legend.key.width = unit(2, "lines"),  # 调整图例项的宽度
    strip.text = element_text(face = "bold.italic", size = 20),  # 设置分面内字体为加粗斜体，并增大字号
    strip.background = element_rect(color = "white", fill = "grey90", size = 0),  # 设置分面边框颜色和填充颜色
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)  # 设置图的边框颜色和粗细
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE), linetype = guide_legend(nrow = 2, byrow = TRUE))  # 设置图例为两行
ggsave("Plots4_SI_5L.jpeg", ghgplotssp5L, width = 25, height = 16, dpi = 300)
#over



#fig5a--------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

file_path5a <- "/.../globalSDG.xlsx"
globalSDG <- read_excel(file_path5a, sheet = 1)

globalSDG1 <- globalSDG %>% 
  group_by(scenario) %>%
  summarise(
    SumHeatvalue_EJ = sum(SumHeatvalue_EJ),
    EnerHeatvalue_EJ = sum(EnerHeatvalue_EJ),
    `Population(million)` = sum(`Population(million)`),
    `GDP(billion USD2017)` = sum(`GDP(billion USD2017)`),
    `Pop_adult` = sum(`Pop_adult`),
    `Pop_low` = sum(`Pop_low`),
    `Pop_lowincome(10 000)` = sum(`Pop_lowincome(10 000)`),
    `finalenergydemand(EJ)` = sum(`finalenergydemand(EJ)`),
    X1.1 = sum(`1/`) * 0.6 / sum(`Pop_lowincome(10 000)`) / 10000,
    X2.1 = sum(`2/`) / sum(`Pop_adult`) / 1000000,
    X3.1 = sum(`3/`) / sum(`Population(million)`) / 1000000,
    X6.1 = sum(`4/`) / sum(`Population(million)`) / 1000000,
    X7_1 = sum(`5/`) / sum(`finalenergydemand(EJ)`) / 1000000000000,
    X7_2 = sum(`6/`) / sum(`Population(million)`) / 1000000,
    X8.1 = sum(`7/`) / sum(`Pop_adult`) / 1000000,
    X9.1 = sum(`8/`) / sum(`Population(million)`) / 1000000,
    X10.1 = sum(`9/`) * 0.6 / sum(`Pop_lowincome(10 000)`) / 10000,
    X11_1 = sum(`10/`) / sum(`Population(million)`) / 1000000,
    X11_2 = sum(`11/`) / sum(`Population(million)`) / 1000000,
    X12.1 = sum(`12/`) / sum(`Population(million)`) / 1000000,
    X13.1 = sum(`13/`) / sum(`Population(million)`) / 1000000,
    X14.1 = sum(`14/`) / sum(`Population(million)`) / 1000000,
    X15_1 = sum(`15/`) / sum(`Population(million)`) / 1000000,
    X15_2 = sum(`16/`) / sum(`Population(million)`) / 1000000,
    X15_3 = sum(`17/`) / sum(`Population(million)`) / 1000000
    
  )

globalSDG2 <- globalSDG1 %>%
  mutate(S1 = pmin(X1.1 / 11614.90 * 100, 100)) %>%
  mutate(S2 = pmin(X2.1 / 0.001420 * 100, 100)) %>%
  mutate(S3 = pmin(X3.1 / 5.57 * 100, 100)) %>%
  mutate(S6 = pmin(X6.1 / 0.1746 * 100, 100)) %>%
  mutate(S7_1 = pmin(X7_1 / 0.2402 * 100, 100)) %>%
  mutate(S7_2 = pmin(X7_2 / 55.12 * 100, 100)) %>%
  mutate(S7 = S7_1/2 + S7_2/2 ) %>%
  mutate(S8 = pmin(X8.1 / 0.001420 * 100, 100)) %>%
  mutate(S9 = pmin(X9.1 / 128.25 * 100, 100)) %>%
  mutate(S10 = pmin(X10.1 / 11614.90 * 100, 100)) %>%
  mutate(S11_1 = pmin(X11_1 / 2989.89 * 100, 100)) %>%
  mutate(S11_2 = pmin(X11_2 / 5.57 * 100, 100)) %>%
  mutate(S11 = S11_1/2 + S11_2/2 ) %>%
  mutate(S12 = pmin(X12.1 / 7.13 * 100, 100)) %>%
  mutate(S13 = pmin(X13.1 / 1.13 * 100, 100)) %>%
  mutate(S14 = pmin(X14.1 / 0.04187 * 100, 100)) %>%
  mutate(S15_1 = pmin(X15_1 / 0.1746 * 100, 100)) %>%
  mutate(S15_2 = pmin(X15_2 / 1.14 * 100, 100)) %>%
  mutate(S15_3 = pmin(X15_3 / 2977.62 * 100, 100)) %>%
  mutate(S15 = S15_1/3 + S15_2/3 + S15_3/3) %>%
  mutate(sdg1 = pmin(S1 /7/17, 100/7/17)) %>%
  mutate(sdg2 = pmin(S2 /8/17, 100/8/17)) %>%
  mutate(sdg3 = pmin(S3 /13/17, 100/13/17)) %>%
  mutate(sdg6 = pmin(S6 /8/17, 100/8/17)) %>%
  mutate(sdg7 = pmin(S7 /5/17, 100/5/17)) %>%
  mutate(sdg8 = pmin(S8 / 12/17, 100/12/17)) %>%
  mutate(sdg9 = pmin(S9 / 8/17, 100/8/17)) %>%
  mutate(sdg10 = pmin(S10 /10/17, 100/10/17)) %>%
  mutate(sdg11 = pmin(S11 /10/17, 100/10/17)) %>%
  mutate(sdg12 = pmin(S12 / 10/17, 100/10/17)) %>%
  mutate(sdg13 = pmin(S13 /5/17, 100/5/17)) %>%
  mutate(sdg14 = pmin(S14 /10/17, 100/10/17)) %>%
  mutate(sdg15 = pmin(S15 /12/17, 100/12/17)) %>%
  mutate(SDG = sdg1+ sdg2+ sdg3+ sdg6+ sdg7+ sdg8+ sdg9+ sdg10+ sdg11+ sdg12+ sdg13+ sdg14+ sdg15) %>%
  mutate(SDG_100 = (S1+ S2+ S3+ S6+ S7+ S8+ S9+ S10+ S11+ S12+ S13+ S14+ S15)/13)

globalSDG2_long <- globalSDG2 %>%
  select(scenario, contains("sdg"), -SDG,-SDG_100) %>%
  pivot_longer(cols = -scenario, names_to = "SDG_target", values_to = "value")

sdg_order <- c("sdg1", "sdg2", "sdg3", "sdg6", "sdg7", "sdg8", "sdg9", "sdg10", "sdg11", "sdg12", "sdg13", "sdg14", "sdg15")

globalSDG2_long$SDG_target <- factor(globalSDG2_long$SDG_target, levels = sdg_order)
write.csv(globalSDG2_long,"/Users/noginger/Library/CloudStorage/OneDrive-个人/课题/第二项研究/收集数据/作图数据/globalSDG2_long.csv")

y_labels <- c("SDG1", "SDG2", "SDG3", "SDG6", "SDG7", "SDG8", 
              "SDG9", "SDG10", "SDG11", "SDG12", "SDG13", 
              "SDG14", "SDG15")

x_labels <- c("SSP1-H", "SSP1-M", "SSP2-M", "SSP2-L", "SSP4-M", "SSP5-L")

globalsdgplot <- ggplot(globalSDG2_long, aes(y = SDG_target, x = scenario, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "SDG score")+
  scale_y_discrete(labels = y_labels) +  # 使用自定义的 y 轴标签
  scale_x_discrete(labels = x_labels) +  # 使用自定义的 x 轴标签
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", angle = 270, hjust = 1, size = 14),  # 自定义 x 轴标签字体大小
    axis.text.y = element_text(color = "black", size = 14),  # 自定义 y 轴标签字体大小
    legend.text = element_text(size = 14),  # 自定义图例文本字体大小
    legend.title = element_blank(),  # 自定义图例标题字体大小
    legend.key.width = unit(2, "cm"),  # 设置图例项的宽度为2cm
    legend.key.height = unit(0.5, "cm"),  # 设置图例项的高度为0.5cm
    panel.grid = element_blank(),  # 去掉灰色网格
    axis.title.x = element_blank(),  # 去掉 x 轴标题
    axis.title.y = element_blank(),  # 去掉 y 轴标题
    plot.title = element_blank(),  # 去掉图标题
    legend.position = "bottom",  # 将图例放在图的下方
    legend.direction = "horizontal"  # 将图例水平排列
  )

globalsdgplot <- ggplot(globalSDG2_long, aes(x = SDG_target, y = scenario, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "SDG score", alpha = 1) +
  scale_x_discrete(labels = y_labels) +  # 将 y_labels 用于 x 轴
  scale_y_discrete(labels = x_labels) +  # 将 x_labels 用于 y 轴
  theme_minimal() +
  theme(
    axis.text.x = element_text(color = "black", angle = 0, vjust = 1, size = 14),  # 自定义 x 轴标签字体大小
    axis.text.y = element_text(color = "black", size = 14, vjust = 1, hjust = 0),  # 自定义 y 轴标签字体大小
    legend.text = element_text(size = 14),  # 自定义图例文本字体大小
    legend.title = element_blank(),  # 自定义图例标题字体大小
    legend.key.width = unit(2, "cm"),  # 设置图例项的宽度为2cm
    legend.key.height = unit(0.5, "cm"),  # 设置图例项的高度为0.5cm
    panel.grid = element_blank(),  # 去掉灰色网格
    axis.title.x = element_blank(),  # 去掉 x 轴标题
    axis.title.y = element_blank(),  # 去掉 y 轴标题
    plot.title = element_blank(),  # 去掉图标题
    legend.position = "bottom",  # 将图例放在图的下方
    legend.direction = "horizontal"  # 将图例水平排列
  )

print(globalsdgplot)
#over

#fig5b------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggforce)
library(readxl)

file_path5b <- "/Users/noginger/Library/CloudStorage/OneDrive-个人/课题/第二项研究/收集数据/作图数据/图4-SDG/地区层面SDG.xlsx"
regionSDG <- read_excel(file_path5b, sheet = 1)

region_abbreviations <- c(
  "USA" = "USA",
  "Taiwan" = "TWN",
  "Southeast Asia" = "SEA",
  "South Korea" = "KOR",
  "South Asia" = "SAS",
  "South America_Southern" = "SAMS",
  "South America_Northern" = "SAMN",
  "South Africa" = "ZAF",
  "Russia" = "RUS",
  "Pakistan" = "PAK",
  "Middle East" = "MEA",
  "Mexico" = "MEX",
  "Japan" = "JPN",
  "Indonesia" = "IDN",
  "India" = "IND",
  "European Free Trade Association" = "EFTA",
  "Europe_Non_EU" = "NEU",
  "Europe_Eastern" = "EEU",
  "EU-15" = "EU15",
  "EU-12" = "EU12",
  "Colombia" = "COL",
  "China" = "CHN",
  "Central Asia" = "CAS",
  "Central America and Caribbean" = "CAC",
  "Canada" = "CAN",
  "Brazil" = "BRA",
  "Australia_NZ" = "ANZ",
  "Argentina" = "ARG",
  "Africa_Western" = "WAF",
  "Africa_Southern" = "SAF",
  "Africa_Northern" = "NAF",
  "Africa_Eastern" = "EAF"
)

continent_mapping <- c(
  "EAF" = "Africa", "NAF" = "Africa", "SAF" = "Africa", "WAF" = "Africa",
  "ARG" = "South America", "ANZ" = "Oceania", "BRA" = "South America", "CAN" = "North America",
  "CAC" = "North America", "CAS" = "Asia", "CHN" = "Asia", "COL" = "South America",
  "EU12" = "Europe", "EU15" = "Europe", "EEU" = "Europe", "NEU" = "Europe",
  "EFTA" = "Europe", "IND" = "Asia", "IDN" = "Asia", "JPN" = "Asia",
  "MEX" = "North America", "MEA" = "Asia", "PAK" = "Asia", "RUS" = "Europe",
  "ZAF" = "Africa", "SAMN" = "South America", "SAMS" = "South America",
  "SAS" = "Asia", "KOR" = "Asia", "SEA" = "Asia", "TWN" = "Asia", "USA" = "North America"
)

regionSDG_long <- regionSDG %>%
  pivot_longer(cols = starts_with("ss"), names_to = "variable", values_to = "value")

regionSDG_summary <- regionSDG_long %>%
  group_by(region, scenario) %>%
  summarise(total_value = sum(value))

regionSDG_long <- regionSDG_long %>%
  left_join(regionSDG_summary, by = c("region", "scenario"))

regionSDG_long <- regionSDG_long %>%
  mutate(region = recode(region, !!!region_abbreviations))

regionSDG_long <- regionSDG_long %>%
  mutate(continent = recode(region, !!!continent_mapping))

region_levels <- regionSDG_long %>%
  arrange(continent, region) %>%
  pull(region) %>%
  unique()

regionSDG_pie <- regionSDG_long %>%
  group_by(region, scenario) %>%
  mutate(start = cumsum(lag(value, default = 0))/total_value,
         end = cumsum(value)/total_value)
regionSDG_pie_copy <- regionSDG_pie

custom_colors <- c(
  "ss1" = "#E5243B", "ss2" = "#DDA63A", "ss3" = "#4C9F38", "ss6" = "#26BDE2", 
  "ss7" = "#FCC30B", "ss8" = "#A21942",
  "ss9" = "#FD6925", "ss10" = "#DD1367", "ss11" = "#FD9D24", "ss12" = "#BF8B2E",
  "ss13" = "#3F7E44", "ss14" = "#0A97D9", "ss15" = "#56C02B")


continent_colors <- c(
  "Africa" = "#d9f0a0",
  "Asia" = "#a6bddb",
  "Europe" = "#fdcc8a",
  "North America" = "#fdae6b",
  "Oceania" = "#bcbddc",
  "South America" = "#fbb4b9"
)


regionSDG_pie1 <- regionSDG_pie %>% filter(continent %in% c("Africa", "Asia"))

region_levels <- regionSDG_pie1 %>%
  filter(continent %in% c("Africa", "Asia")) %>%
  arrange(continent, region) %>%
  pull(region) %>%
  unique()

x_labels <- c("SSP1-H", "SSP1-M", "SSP2-M", "SSP2-L", "SSP4-M", "SSP5-L")

AAplot <- ggplot(regionSDG_pie) +
  geom_rect(data = distinct(regionSDG_pie, continent, region) %>% filter(continent %in% c("Africa", "Asia")),
            aes(xmin = 0.5, xmax = 6.5, ymin = as.numeric(factor(region, levels = region_levels)) - 0.5,
                ymax = as.numeric(factor(region, levels = region_levels)) + 0.5,
                fill = continent), alpha = 0.05)+
  geom_arc_bar(aes(x0 = as.numeric(factor(scenario)), y0 = as.numeric(factor(region, levels = region_levels)),
                   r0 = 0, r = sqrt(total_value) / 5,
                   start = 2 * pi * start, end = 2 * pi * end,
                   fill = variable), color = "black", size = 0.05) +
  scale_fill_manual(values = c(custom_colors, continent_colors)) +
  scale_x_continuous(breaks = 1:length(unique(regionSDG_pie$scenario)), 
                     labels = x_labels) +
  scale_y_continuous(breaks = 1:length(region_levels), 
                     labels = region_levels) +
  coord_fixed(ratio = 1) +
  labs(title = "SDG Scores by Region and Scenario", x = "Scenario", y = "Region") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # 去掉网格
    axis.text.x = element_text(color = "black", size = 14, angle = 0, hjust = 0.5, vjust = 0.5, margin = margin(t = -10)),
    axis.text.y = element_text(color = "black", size = 14),  # 设置y轴标注字体大小
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",  # 移除图例
    plot.title = element_blank(),  # 移除默认标题
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0)  # 减少图形四周的边距
  )+
  annotate("point", x = 5.8, y = 1.5, size = 24.4, shape = 21, fill = "white", color = "black")+  # 添加圆形
  annotate("point", x = 5.8, y = 2.8, size = 20, shape = 21, fill = "white", color = "black") + # 添加圆形
  annotate("point", x = 5.8, y = 3.8, size = 10, shape = 21, fill = "white", color = "black")+  # 添加圆形
  annotate("text", x = 5.8, y = 3.8, label = "1", color = "black", size = 5)+
  annotate("text", x = 5.8, y = 2.8, label = "4", color = "black", size = 5)+
  annotate("text", x = 5.8, y = 1.5, label = "6", color = "black", size = 5)+
  annotate("text", x = 5.8, y = 4.3, label = "SDG scores", color = "black", size = 5)
print(AAplot)

regionSDG_pie2 <- regionSDG_pie_copy %>% filter(continent %in% c("Europe" , "North America", "Oceania", "South America"))

region_levels2 <- regionSDG_pie2 %>%
  filter(continent %in% c("Europe" , "North America", "Oceania", "South America")) %>%
  arrange(continent, region) %>%
  pull(region) %>%
  unique()

BBplot <- ggplot(regionSDG_pie_copy) +
  geom_rect(data = distinct(regionSDG_pie_copy, continent, region) %>% filter(continent %in% c("Europe", "North America", "Oceania", "South America")),
            aes(xmin = 0.5, xmax = 6.5, ymin = as.numeric(factor(region, levels = region_levels2)) - 0.5,
                ymax = as.numeric(factor(region, levels = region_levels2)) + 0.5,
                fill = continent), alpha = 0.05)+
  geom_arc_bar(aes(x0 = as.numeric(factor(scenario)), y0 = as.numeric(factor(region, levels = region_levels2)),
                   r0 = 0, r = sqrt(total_value) / 5,
                   start = 2 * pi * start, end = 2 * pi * end,
                   fill = variable), color = "black", size = 0.05) +
  scale_fill_manual(values = c(custom_colors, continent_colors)) +
  scale_x_continuous(breaks = 1:length(unique(regionSDG_pie_copy$scenario)), 
                     labels = x_labels) +
  scale_y_continuous(breaks = 1:length(region_levels2), 
                     labels = region_levels2) +
  coord_fixed(ratio = 1) +
  labs(title = "SDG Scores by Region and Scenario", x = "Scenario", y = "Region") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # 去掉网格

    axis.text.x = element_text(color = "black", size = 14, angle = 0, hjust = 0.5, vjust = 0.5, margin = margin(t = -10)),
    axis.text.y = element_text(color = "black", size = 14),  # 设置y轴标注字体大小
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none",  # 移除图例
    plot.title = element_blank(),  # 移除默认标题
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0))

print(BBplot)

library(cowplot)
AB_plot <- plot_grid(AAplot, BBplot, ncol = 2, rel_widths = c(1, 1))
library(ggplot2)
library(gridExtra)
custom_colors <- c(
  "ss1" = "#E5243B", "ss2" = "#DDA63A", "ss3" = "#4C9F38", "ss6" = "#26BDE2", 
  "ss7" = "#FCC30B", "ss8" = "#A21942","ss9" = "#FD6925", "ss10" = "#DD1367", 
  "ss11" = "#FD9D24", "ss12" = "#BF8B2E", "ss13" = "#3F7E44", "ss14" = "#0A97D9",
  "ss15" = "#56C02B"
)

continent_colors <- c(
  "Africa" = "#d9f0a0",
  "Asia" = "#a6bddb",
  "Europe" = "#fdcc8a",
  "North America" = "#fdae6b",
  "Oceania" = "#bcbddc",
  "South America" = "#fbb4b9"
)

names(custom_colors) <- gsub("^ss", "SDG", names(custom_colors))

sdg_legend_data <- data.frame(
  SDG = factor(names(custom_colors), levels = names(custom_colors)),
  x = 1,
  y = 1:length(custom_colors)
)

sdg_legend_plot <- ggplot(sdg_legend_data, aes(x, y, fill = SDG)) +
  geom_point(shape = 22, size = 5) +
  scale_fill_manual(values = custom_colors) +
  theme_void() +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.text = element_text(size = 14)  # 设置图例字体大小
  ) +
  guides(fill = guide_legend(ncol = 7, byrow = TRUE)) # 两列排列，每列7个图例项

legendsdg <- get_legend(sdg_legend_plot)
continent_legend_data <- data.frame(
  Continent = factor(names(continent_colors), levels = names(continent_colors)),
  x = 1,
  y = 1:length(continent_colors)
)

continent_legend_plot <- ggplot(continent_legend_data, aes(x, y, fill = Continent)) +
  geom_point(shape = 22, size = 5, color = NA) +
  scale_fill_manual(values = continent_colors) +
  theme_void() +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.text = element_text(size = 14)  # 设置图例字体大小
  ) +
  guides(fill = guide_legend(ncol = 6, byrow = TRUE)) # 两列排列，每列3个图例项

print(continent_legend_plot)
legendcontinent <- get_legend(continent_legend_plot)

library(cowplot)
combined_legend_plot <- plot_grid(legendsdg, legendcontinent, nrow = 2, rel_heights  = c( 2, 1))

print(combined_legend_plot)
plot5b <- plot_grid(AB_plot,combined_legend_plot,nrow = 2,rel_heights = c(5,0.3))
plot5_labeled <- plot_grid(
  plot_grid(globalsdgplot, labels = "a", label_size = 20, label_x = -0.02, label_y = 1, label_fontface = "bold", label_colour = "black"),
  plot_grid(plot5b, labels = "b", label_size = 20,label_x = -0.02, label_y = 1, label_fontface = "bold", label_colour = "black"),
  nrow = 2,
  rel_heights = c(1, 5.3)
) + theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "in"))



sdg_legend_plot <- ggplot(sdg_legend_data, aes(x, y, fill = SDG)) +
  geom_point(shape = 22, size = 5) +
  scale_fill_manual(values = custom_colors) +
  theme_void() +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.text = element_text(size = 14),  # 设置图例字体大小
    legend.spacing.x = unit(0.5, "cm"),  # 增加水平间距
    legend.spacing.y = unit(0.5, "cm")   # 增加垂直间距
  ) +
  guides(fill = guide_legend(ncol = 5, byrow = TRUE))  # 更改图例的列数


AB_plot <- plot_grid(AAplot, BBplot, ncol = 2, rel_widths = c(1, 1), align = "v")


combined_legend_plot <- plot_grid(legendsdg, legendcontinent, nrow = 2, rel_heights = c(2, 1), align = "v", axis = "tb")


plot5b <- plot_grid(AB_plot, combined_legend_plot, nrow = 2, rel_heights = c(5, 0.5), 
                    rel_widths = c(1, 1), align = "v", axis = "tb", 
                    labels = c("b"), label_size = 20, label_x = -0.02, label_y = 1, label_fontface = "bold")


plot5_labeled <- plot_grid(
  plot_grid(globalsdgplot, labels = "a", label_size = 20, label_x = -0.02, label_y = 1, label_fontface = "bold", label_colour = "black"),
  plot5b,
  nrow = 2,
  rel_heights = c(1, 5.5)
) + theme(plot.margin = unit(c(0.2, 0.2, 0.4, 0.2), "in"))  # 增加底部边距

ggsave("Fig5.jpeg", plot5_labeled, width = 12, height = 18, dpi = 300)

#over

#fig6-----------------------

file_path6 <- "/Users/noginger/Library/CloudStorage/OneDrive-个人/课题/第二项研究/收集数据/农业废弃物预测数据核算/bio_SDG_raw_总分更新版.xlsx"
bio_SDG_raw <- read_excel(file_path6, sheet = "bio_SDG_raw_总分更新版")
library(ggplot2)
library(readxl)
library(dplyr)
library(cowplot)
library(ggforce)
regionSDG_total <- bio_SDG_raw %>% group_by(region, scenario, Year) %>%
  summarise(
    SDG = sum(SDG),
    EnerHeatvalue_EJ = sum(EnerHeatvalue_EJ)*1000000000, #单位EJ变GJ
    SumHeatvalue_EJ = sum(SumHeatvalue_EJ)*1000000000, #单位EJ变GJ
    `Population.million.` = sum(`Population(million)`) * 1e6
  )

regionSDG_total$perEner = regionSDG_total$EnerHeatvalue_EJ/regionSDG_total$Population.million.
regionSDG_total$perSum = regionSDG_total$SumHeatvalue_EJ/regionSDG_total$Population.million.

regionSDG_total <- regionSDG_total %>%
  filter(!(scenario %in% c("SSP226", "SSP460")))


regionSDG_total <- regionSDG_total %>%
  mutate(color_group = case_when(
    region == "USA" ~ "USA",
    region == "China" ~ "CHN",
    region == "Canada" ~ "CAN",
    region == "Africa_Western" ~ "WAF",
    region == "Japan" ~ "JPN",
    region == "Australia_NZ" ~ "ANZ",
    region == "Europe_Eastern" ~ "EEU",
    region == "Pakistan" ~ "PAK",
    region == "South Asia" ~ "SAS",
    region == "European Free Trade Association" ~ "EFTA",
    TRUE ~ "Other"
  ))

write.csv(regionSDG_total,"/Users/noginger/Library/CloudStorage/OneDrive-个人/课题/第二项研究/收集数据/作图数据/SDG_region_percapia.csv")


custom_colors <- c("USA" = "#0066CC", "CHN" = "red", "CAN" = "#006633", "WAF" = "#CC33FF","EFTA" = "lightblue",
                   "JPN" = "#FFCC00", "ANZ" = "#FF00CC","EEU" = "#FF6600", "PAK" = "brown","SAS" = "lightgreen",
                   "Other" = "grey")


regionSDG_total <- regionSDG_total %>%
  arrange(desc(color_group == "Other"))


main_plot <- ggplot(regionSDG_total, aes(x = perEner, y = SDG, shape = scenario, color = color_group, alpha = as.factor(Year))) +

  geom_rect(aes(xmin = 0, xmax = 3.3, ymin = 0, ymax = 2.3), fill = "#FFCCCC", color = NA, alpha = 0.5) +  # 添加矩形
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = -3.2, color = "black", linetype = "solid", size = 1) +  # 添加斜率为1的斜线
  scale_shape_manual(
    values = c(16, 21, 17, 2, 15, 18),  # Custom shapes
    labels = c("SSP1-H", "SSP1-M", "SSP2-M", "SSP2-L", "SSP4-M", "SSP5-L")  # Custom labels
  )+
  scale_color_manual(values = custom_colors) +  # 使用自定义颜色
  scale_alpha_manual(values = seq(0.15, 1, length.out = length(unique(regionSDG_total$Year)))) +  # 使用透明度表示年份
  labs(
    x = "Per capita AFRs(GJ/person)",
    y = "SDG scores",
    shape = "Scenario",
    color = "Region",
    alpha = "Year") +
  coord_cartesian(xlim = c(0, 15), ylim = c(0, 7)) +  # 设置主图的x和y轴坐标范围
  geom_vline(xintercept = 5.2, color = "black", linetype = "dashed", size = 0.7) +  # 添加竖直虚线
  geom_hline(yintercept = 2.9, color = "black", linetype = "dashed", size = 0.7) +  # 添加水平虚线
  theme_bw(base_size = 18) +  # 设置所有字体大小为18，黑色
  theme(
    legend.position = "right",
    plot.title = element_blank(),  # 去掉图标题
    panel.border = element_rect(color = "black", size = 1.5),  # 设置图边框粗细
    panel.grid = element_blank(),  # 去掉网格
    axis.text = element_text(size = 18, color = "black"),  # x和y轴刻度的字体大小和颜色
    axis.title = element_text(size = 18, color = "black"),  # x和y轴标签的字体大小和颜色
    legend.text = element_text(size = 14, color = "black"),  # 图例文本的字体大小和颜色
    legend.title = element_text(size = 18, color = "black")  # 图例标题的字体大小和颜色
  )


inset_plot_large <- ggplot(regionSDG_total, aes(x = perEner, y = SDG, shape = scenario, color = color_group, alpha = as.factor(Year))) +

  geom_rect(aes(xmin = 0, xmax = 3.3, ymin = 0, ymax = 2.3), fill = "#FFCCCC", color = NA, alpha = 0.5) +  # 添加矩形
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = -5, color = "black", linetype = "solid", size = 1) +  # 添加斜率为1的斜线
  scale_shape_manual(values = c(16, 21, 17, 2, 15, 18)) +  # 自定义形状
  scale_color_manual(values = custom_colors) +  # 使用自定义颜色
  scale_alpha_manual(values = seq(0.15, 1, length.out = length(unique(regionSDG_total$Year)))) +  # 使用透明度表示年份
  coord_cartesian(xlim = c(0, 40), ylim = c(0, 10)) +  # 设置更大范围的小图的x和y轴坐标范围
  labs(x = NULL, y = NULL) +  # 去掉标签
  geom_vline(xintercept = 5.2, color = "black", linetype = "dashed", size = 0.7) +  # 添加竖直虚线
  geom_hline(yintercept = 2.9, color = "black", linetype = "dashed", size = 0.7) +  # 添加水平虚线
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, color = "black"),  # x和y轴刻度的字体大小和颜色
    panel.grid = element_blank(),  # 去掉网格
    legend.position = "none")  # 去掉图例

inset_plot <- ggplot(regionSDG_total, aes(x = perEner, y = SDG, shape = scenario, color = color_group, alpha = as.factor(Year))) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = -1, color = "black", linetype = "solid", size = 1) +  # 添加斜率为1的斜线
  scale_shape_manual(values = c(16, 21, 17, 2, 15, 18)) +  # 自定义形状
  scale_color_manual(values = custom_colors) +  # 使用自定义颜色
  scale_alpha_manual(values = seq(0.15, 1, length.out = length(unique(regionSDG_total$Year)))) +  # 使用透明度表示年份
  coord_cartesian(xlim = c(0, 3), ylim = c(0, 2)) +  # 放大左下角区域
  labs(x = NULL, y = NULL) +  # 去掉标签
  theme_bw() +
  theme(
    axis.text = element_text(size = 12, color = "black"),  # x和y轴刻度的字体大小和颜色
    panel.grid = element_blank(),  # 去掉网格
    legend.position = "none")  # 去掉图例

plot6 <- ggdraw() +
  draw_plot(main_plot) +
  draw_plot(inset_plot_large, x = 0.05, y = 0.58, width = 0.4, height = 0.4) +  # 调整大范围小图的位置和大小，放在左上角
  draw_plot(inset_plot, x = 0.57, y = 0.1, width = 0.3, height = 0.3)  # 调整原有小图的位置和大小，放在右下角

ggsave("Fig6.jpeg", plot6, width = 20, height = 12, dpi = 300)


#over






















#figs in  SI-------------------------
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

file_path25 <- "/.../economicregion.xlsx"
regionGDP <- read_excel(file_path25, sheet = 1)

region_info <- data.frame(
  region = c("USA", "Taiwan", "Southeast Asia", "South Korea", "South Asia", 
             "South America_Southern", "South America_Northern", "South Africa", 
             "Russia", "Pakistan", "Middle East", "Mexico", "Japan", "Indonesia", 
             "India", "European Free Trade Association", "Europe_Non_EU", 
             "Europe_Eastern", "EU-15", "EU-12", "Colombia", "China", "Central Asia", 
             "Central America and Caribbean", "Canada", "Brazil", "Australia_NZ", 
             "Argentina", "Africa_Western", "Africa_Southern", "Africa_Northern", 
             "Africa_Eastern"),
  abbreviation = c("USA", "TWN", "SEA", "KOR", "SAS", "SAMS", "SAMN", "ZAF", 
                   "RUS", "PAK", "MEA", "MEX", "JPN", "IDN", "IND", "EFTA", 
                   "NEU", "EEU", "EU15", "EU12", "COL", "CHN", "CAS", "CAC", 
                   "CAN", "BRA", "ANZ", "ARG", "WAF", "SAF", "NAF", "EAF"),
  continent = c("North America", "Asia", "Asia", "Asia", "Asia", "South America", 
                "South America", "Africa", "Europe", "Asia", "Asia", "North America", 
                "Asia", "Asia", "Asia", "Europe", "Europe", "Europe", "Europe", 
                "Europe", "South America", "Asia", "Asia", "North America", 
                "North America", "South America", "Oceania", "South America", 
                "Africa", "Africa", "Africa", "Africa")
)

sorted_region_info <- region_info %>%
  arrange(continent, region)

region_order <- sorted_region_info$abbreviation

regionGDP <- regionGDP %>%
  mutate(region = recode(region, !!!setNames(region_info$abbreviation, region_info$region))) %>%
  mutate(region = factor(region, levels = region_order))

regionGDP1 <- regionGDP %>%
  group_by(scenario, region, Development, Income_level) %>%
  summarise(
    GDP_dollar = sum(GDP_美元),
    Profit_dollar = sum(利润_美元)
  ) %>%
  pivot_longer(cols = c(GDP_dollar, Profit_dollar), names_to = "Type", values_to = "Dollar")

regionGDP1 <- regionGDP1 %>%
  filter(Type == "GDP_dollar")

GDPplot2060 <- ggplot(regionGDP1, aes(x = region, y = Dollar / 1e9, color = scenario)) +
  geom_point(position = position_jitter(width = 0, height = 0), shape = 17, size = 6, alpha = 0.8) +
  scale_color_manual(values = c("SSP126" = "#CAB82E", "SSP145" = "#BD4146", "SSP245" = "#4590BF", "SSP260" = "#6D3E99", "SSP445" = "#F47D1F", "SSP560" = "#199C77"),
                     labels = c("SSP126" = "SSP1-H", "SSP145" = "SSP1-M", "SSP245" = "SSP2-M",
                                "SSP260" = "SSP2-L", "SSP445" = "SSP4-M", "SSP560" = "SSP5-L")) +  # 自定义图例标签
  labs(x = NULL, y = "Industrial added value (Billion USD)", color = NULL) +  # 设置x轴标题为NULL，y轴单位变为Billion, 去掉图例标题
  theme_minimal() +  # 改为简约主题，移除灰色背景
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"),  # 设置x轴字体
    axis.text.y = element_text(size = 16, color = "black"),  # 设置y轴字体
    axis.title.y = element_text(size = 16),  # 设置y轴标题字体大小
    axis.title.x = element_blank(),  # 去掉x轴标题
    legend.position = c(0.9, 0.85),  # 图例放置在图片内部右上角
    legend.text = element_text(size = 12),  # 设置图例文字大小为12
    plot.title = element_blank(),  # 去掉图标题
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # 添加黑色边框
  )

regionGDP <- read_excel(file_path25, sheet = 2)

region_order <- sorted_region_info$abbreviation

regionGDP <- regionGDP %>%
  mutate(region = recode(region, !!!setNames(region_info$abbreviation, region_info$region))) %>%
  mutate(region = factor(region, levels = region_order))

regionGDP1 <- regionGDP %>%
  group_by(scenario, region, Development, Income_level) %>%
  summarise(
    GDP_dollar = sum(GDP_美元),
    Profit_dollar = sum(利润_美元)
  ) %>%
  pivot_longer(cols = c(GDP_dollar, Profit_dollar), names_to = "Type", values_to = "Dollar")

regionGDP1 <- regionGDP1 %>%
  filter(Type == "GDP_dollar")

GDPplot2025 <- ggplot(regionGDP1, aes(x = region, y = Dollar / 1e9, color = scenario)) +
  geom_point(position = position_jitter(width = 0, height = 0), shape = 17, size = 6, alpha = 0.8) +
  scale_color_manual(values = c("SSP126" = "#CAB82E", "SSP145" = "#BD4146", "SSP245" = "#4590BF", "SSP260" = "#6D3E99", "SSP445" = "#F47D1F", "SSP560" = "#199C77"),
                     labels = c("SSP126" = "SSP1-H", "SSP145" = "SSP1-M", "SSP245" = "SSP2-M",
                                "SSP260" = "SSP2-L", "SSP445" = "SSP4-M", "SSP560" = "SSP5-L")) +  # 自定义图例标签
  labs(x = NULL, y = "Industrial added value (Billion USD)", color = NULL) +  # 设置x轴标题为NULL，y轴单位变为Billion, 去掉图例标题
  theme_minimal() +  # 改为简约主题，移除灰色背景
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"),  # 设置x轴字体
    axis.text.y = element_text(size = 16, color = "black"),  # 设置y轴字体
    axis.title.y = element_text(size = 16),  # 设置y轴标题字体大小
    axis.title.x = element_blank(),  # 去掉x轴标题
    legend.position = c(0.9, 0.85),  # 图例放置在图片内部右上角
    legend.text = element_text(size = 12),  # 设置图例文字大小为12
    plot.title = element_blank(),  # 去掉图标题
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # 添加黑色边框
  )

print(GDPplot2025)

regionGDP <- read_excel(file_path25, sheet = 3)

region_order <- sorted_region_info$abbreviation

regionGDP <- regionGDP %>%
  mutate(region = recode(region, !!!setNames(region_info$abbreviation, region_info$region))) %>%
  mutate(region = factor(region, levels = region_order))

regionGDP1 <- regionGDP %>%
  group_by(scenario, region, Development, Income_level) %>%
  summarise(
    GDP_dollar = sum(GDP_美元),
    Profit_dollar = sum(利润_美元)
  ) %>%
  pivot_longer(cols = c(GDP_dollar, Profit_dollar), names_to = "Type", values_to = "Dollar")

regionGDP1 <- regionGDP1 %>%
  filter(Type == "GDP_dollar")

GDPplot2040 <- ggplot(regionGDP1, aes(x = region, y = Dollar / 1e9, color = scenario)) +
  geom_point(position = position_jitter(width = 0, height = 0), shape = 17, size = 6, alpha = 0.8) +
  scale_color_manual(values = c("SSP126" = "#CAB82E", "SSP145" = "#BD4146", "SSP245" = "#4590BF", "SSP260" = "#6D3E99", "SSP445" = "#F47D1F", "SSP560" = "#199C77"),
                     labels = c("SSP126" = "SSP1-H", "SSP145" = "SSP1-M", "SSP245" = "SSP2-M",
                                "SSP260" = "SSP2-L", "SSP445" = "SSP4-M", "SSP560" = "SSP5-L")) +  # 自定义图例标签
  labs(x = NULL, y = "Industrial added value (Billion USD)", color = NULL) +  # 设置x轴标题为NULL，y轴单位变为Billion, 去掉图例标题
  theme_minimal() +  # 改为简约主题，移除灰色背景
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black"),  # 设置x轴字体
    axis.text.y = element_text(size = 16, color = "black"),  # 设置y轴字体
    axis.title.y = element_text(size = 16),  # 设置y轴标题字体大小
    axis.title.x = element_blank(),  # 去掉x轴标题
    legend.position = c(0.9, 0.85),  # 图例放置在图片内部右上角
    legend.text = element_text(size = 12),  # 设置图例文字大小为12
    plot.title = element_blank(),  # 去掉图标题
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # 添加黑色边框
  )

library(cowplot)

GDPplot2025_labeled <- ggdraw() + 
  draw_plot(GDPplot2025) + 
  draw_label("a", x = 0.02, y = 0.98, size = 24, fontface = "bold", color = "black", hjust = 0, vjust = 1)+
  draw_label("2025", x = 0.05, y = 0.98, size = 20, fontface = "italic", color = "black", hjust = 0, vjust = 1)

GDPplot2040_labeled <- ggdraw() + 
  draw_plot(GDPplot2040) + 
  draw_label("b", x = 0.02, y = 0.98, size = 24, fontface = "bold", color = "black", hjust = 0, vjust = 1)+
  draw_label("2040", x = 0.05, y = 0.98, size = 20, fontface = "italic", color = "black", hjust = 0, vjust = 1)

GDPplot2060_labeled <- ggdraw() + 
  draw_plot(GDPplot2060) + 
  draw_label("c", x = 0.02, y = 0.98, size = 24, fontface = "bold", color = "black", hjust = 0, vjust = 1)+
  draw_label("2060", x = 0.05, y = 0.98, size = 20, fontface = "italic", color = "black", hjust = 0, vjust = 1)

GDPplot_SI <- plot_grid(GDPplot2025_labeled, GDPplot2040_labeled, GDPplot2060_labeled, 
                           ncol = 1, align = "v", axis = "lr")

print(GDPplot_SI)

ggsave("GDPplot_SI.jpeg", GDPplot_SI, width = 15, height = 25, dpi = 300)

#over

#Fig-SI-Jobs----------------------------
library(ggplot2)
library(readxl)
library(dplyr)
library(patchwork)

region_info <- data.frame(
  region = c("USA", "Taiwan", "Southeast Asia", "South Korea", "South Asia", 
             "South America_Southern", "South America_Northern", "South Africa", 
             "Russia", "Pakistan", "Middle East", "Mexico", "Japan", "Indonesia", 
             "India", "European Free Trade Association", "Europe_Non_EU", 
             "Europe_Eastern", "EU-15", "EU-12", "Colombia", "China", "Central Asia", 
             "Central America and Caribbean", "Canada", "Brazil", "Australia_NZ", 
             "Argentina", "Africa_Western", "Africa_Southern", "Africa_Northern", 
             "Africa_Eastern"),
  abbreviation = c("USA", "TWN", "SEA", "KOR", "SAS", "SAMS", "SAMN", "ZAF", 
                   "RUS", "PAK", "MEA", "MEX", "JPN", "IDN", "IND", "EFTA", 
                   "NEU", "EEU", "EU15", "EU12", "COL", "CHN", "CAS", "CAC", 
                   "CAN", "BRA", "ANZ", "ARG", "WAF", "SAF", "NAF", "EAF"),
  continent = c("North America", "Asia", "Asia", "Asia", "Asia", "South America", 
                "South America", "Africa", "Europe", "Asia", "Asia", "North America", 
                "Asia", "Asia", "Asia", "Europe", "Europe", "Europe", "Europe", 
                "Europe", "South America", "Asia", "Asia", "North America", 
                "North America", "South America", "Oceania", "South America", 
                "Africa", "Africa", "Africa", "Africa")
)

sorted_region_info <- region_info %>%
  arrange(continent, region)

region_order <- sorted_region_info$abbreviation

file_path26 <- "/.../regionjob.xlsx"
regionJob <- read_excel(file_path26, sheet = 1)

regionJob <- regionJob %>%
  mutate(region = recode(region, !!!setNames(region_info$abbreviation, region_info$region))) %>%
  mutate(region = factor(region, levels = region_order))

regionJob1 <- regionJob %>% 
  group_by(scenario, region, Development, Income_level) %>%
  summarise(
    `就业岗位数量` = sum(`就业岗位数量`),
    `占待就业比重` = sum(`占待就业比重`)
  )

custom_labels <- c("SSP126" = "SSP1-H", "SSP145" = "SSP1-M", "SSP245" = "SSP2-M",
                   "SSP260" = "SSP2-L", "SSP445" = "SSP4-M", "SSP560" = "SSP5-L")

plot_job_scenario <- function(data, scenario_name, is_bottom = FALSE, index) {
  p <- ggplot(data, aes(x = region)) +
    geom_col(aes(y = `就业岗位数量`), fill = "#BD4146") +  # 绘制柱状图表示就业岗位数量
    geom_point(aes(y = `占待就业比重` * 1e6 / 0.015), color = "#3C7BB0", size = 5) +  # 绘制点图表示占待就业比重
    geom_vline(xintercept = c(5.5, 16.5, 22.5, 26.5, 27.5), color = "gray", linetype = "dashed") +  # 添加灰色竖线分隔
    annotate("text", x = 1.5, y = 95000, label = custom_labels[[scenario_name]], color = "black", size = 5, fontface = "bold.italic") + 
    scale_y_continuous(
      name = NULL,  # 不显示左侧 y 轴标签
      limits = c(0, 1e5),
      sec.axis = sec_axis(
        ~ . * 0.015 / 1e5, 
        name = NULL,
        labels = custom_y_labels  # 使用自定义的 y 轴右侧标签
      )
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = if (is_bottom) "black" else NA),
      axis.text.y = element_text(color = "black", size = 16),  # y轴左侧标签为红色
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),  # 移除左侧 y 轴标题
      axis.text.y.right = element_text(color = "black", size = 16),  # y轴右侧标签为蓝色
      axis.title.y.right = element_blank(),  # 移除右侧 y 轴标题
      plot.title = element_blank(),
      panel.grid = element_blank(),  # 去掉网格
      panel.border = element_rect(color = "black", size = 1.5),  # 添加黑色边框
      axis.text = element_text(size = 16)  # 设置x和y轴标签字体大小
    ) 
  
  if (!is_bottom) {
    p <- p + theme(axis.text.x = element_blank())  # 隐藏除最后一个图以外的 x 轴标签
  }
  
  return(p)
}

custom_y_labels <- c("0%", "0.05%", "0.10%", "0.15%")

scenarios <- unique(regionJob1$scenario)

plots <- lapply(seq_along(scenarios), function(i) {
  scenario_data <- regionJob1 %>% filter(scenario == !!scenarios[i])
  is_bottom <- i == 6  # 最后一个图有 x 轴
  plot_job_scenario(scenario_data, scenarios[i], is_bottom = is_bottom, index = i)
})

JOBplot2060 <- wrap_plots(plots, ncol = 1, nrow = 6)
print(JOBplot2060)

ggsave("JOBplot2060.jpeg", JOBplot2060, width = 20, height = 25, dpi = 300)

file_path26 <- "/.../regionjob.xlsx"
regionJob <- read_excel(file_path26, sheet = 2)

regionJob <- regionJob %>%
  mutate(region = recode(region, !!!setNames(region_info$abbreviation, region_info$region))) %>%
  mutate(region = factor(region, levels = region_order))

regionJob1 <- regionJob %>% 
  group_by(scenario, region, Development, Income_level) %>%
  summarise(
    `就业岗位数量` = sum(`就业岗位数量`),
    `占待就业比重` = sum(`占待就业比重`)
  )

custom_labels <- c("SSP126" = "SSP1-H", "SSP145" = "SSP1-M", "SSP245" = "SSP2-M",
                   "SSP260" = "SSP2-L", "SSP445" = "SSP4-M", "SSP560" = "SSP5-L")

plot_job_scenario <- function(data, scenario_name, is_bottom = FALSE, index) {
  p <- ggplot(data, aes(x = region)) +
    geom_col(aes(y = `就业岗位数量`), fill = "#BD4146") +  # 绘制柱状图表示就业岗位数量
    geom_point(aes(y = `占待就业比重` * 1e6 / 0.015), color = "#3C7BB0", size = 5) +  # 绘制点图表示占待就业比重
    geom_vline(xintercept = c(5.5, 16.5, 22.5, 26.5, 27.5), color = "gray", linetype = "dashed") +  # 添加灰色竖线分隔
    annotate("text", x = 1.5, y = 95000, label = custom_labels[[scenario_name]], color = "black", size = 5, fontface = "bold.italic") + 
    scale_y_continuous(
      name = NULL,  # 不显示左侧 y 轴标签
      limits = c(0, 1e5),
      sec.axis = sec_axis(
        ~ . * 0.015 / 1e5, 
        name = NULL,
        labels = custom_y_labels  # 使用自定义的 y 轴右侧标签
      )
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = if (is_bottom) "black" else NA),
      axis.text.y = element_text(color = "black", size = 16),  # y轴左侧标签为红色
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),  # 移除左侧 y 轴标题
      axis.text.y.right = element_text(color = "black", size = 16),  # y轴右侧标签为蓝色
      axis.title.y.right = element_blank(),  # 移除右侧 y 轴标题
      plot.title = element_blank(),
      panel.grid = element_blank(),  # 去掉网格
      panel.border = element_rect(color = "black", size = 1.5),  # 添加黑色边框
      axis.text = element_text(size = 16)  # 设置x和y轴标签字体大小
    ) 
  
  if (!is_bottom) {
    p <- p + theme(axis.text.x = element_blank())  # 隐藏除最后一个图以外的 x 轴标签
  }
  
  return(p)
}

custom_y_labels <- c("0%", "0.05%", "0.10%", "0.15%")

scenarios <- unique(regionJob1$scenario)

plots <- lapply(seq_along(scenarios), function(i) {
  scenario_data <- regionJob1 %>% filter(scenario == !!scenarios[i])
  is_bottom <- i == 6  # 最后一个图有 x 轴
  plot_job_scenario(scenario_data, scenarios[i], is_bottom = is_bottom, index = i)
})

JOBplot2025 <- wrap_plots(plots, ncol = 1, nrow = 6)
print(JOBplot2025)

ggsave("JOBplot2025.jpeg", JOBplot2025, width = 20, height = 25, dpi = 300)
#over


regionJob <- read_excel(file_path26, sheet = 3)

regionJob <- regionJob %>%
  mutate(region = recode(region, !!!setNames(region_info$abbreviation, region_info$region))) %>%
  mutate(region = factor(region, levels = region_order))

regionJob1 <- regionJob %>% 
  group_by(scenario, region, Development, Income_level) %>%
  summarise(
    `就业岗位数量` = sum(`就业岗位数量`),
    `占待就业比重` = sum(`占待就业比重`)
  )

custom_labels <- c("SSP126" = "SSP1-H", "SSP145" = "SSP1-M", "SSP245" = "SSP2-M",
                   "SSP260" = "SSP2-L", "SSP445" = "SSP4-M", "SSP560" = "SSP5-L")

plot_job_scenario <- function(data, scenario_name, is_bottom = FALSE, index) {
  p <- ggplot(data, aes(x = region)) +
    geom_col(aes(y = `就业岗位数量`), fill = "#BD4146") +  # 绘制柱状图表示就业岗位数量
    geom_point(aes(y = `占待就业比重` * 1e6 / 0.015), color = "#3C7BB0", size = 5) +  # 绘制点图表示占待就业比重
    geom_vline(xintercept = c(5.5, 16.5, 22.5, 26.5, 27.5), color = "gray", linetype = "dashed") +  # 添加灰色竖线分隔
    annotate("text", x = 1.5, y = 95000, label = custom_labels[[scenario_name]], color = "black", size = 5, fontface = "bold.italic") + 
    scale_y_continuous(
      name = NULL,  # 不显示左侧 y 轴标签
      limits = c(0, 1e5),
      sec.axis = sec_axis(
        ~ . * 0.015 / 1e5, 
        name = NULL,
        labels = custom_y_labels  # 使用自定义的 y 轴右侧标签
      )
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = if (is_bottom) "black" else NA),
      axis.text.y = element_text(color = "black", size = 16),  # y轴左侧标签为红色
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),  # 移除左侧 y 轴标题
      axis.text.y.right = element_text(color = "black", size = 16),  # y轴右侧标签为蓝色
      axis.title.y.right = element_blank(),  # 移除右侧 y 轴标题
      plot.title = element_blank(),
      panel.grid = element_blank(),  # 去掉网格
      panel.border = element_rect(color = "black", size = 1.5),  # 添加黑色边框
      axis.text = element_text(size = 16)  # 设置x和y轴标签字体大小
    ) 
  
  if (!is_bottom) {
    p <- p + theme(axis.text.x = element_blank())  # 隐藏除最后一个图以外的 x 轴标签
  }
  
  return(p)
}

custom_y_labels <- c("0%", "0.05%", "0.10%", "0.15%")

scenarios <- unique(regionJob1$scenario)

plots <- lapply(seq_along(scenarios), function(i) {
  scenario_data <- regionJob1 %>% filter(scenario == !!scenarios[i])
  is_bottom <- i == 6  # 最后一个图有 x 轴
  plot_job_scenario(scenario_data, scenarios[i], is_bottom = is_bottom, index = i)
})

JOBplot2040 <- wrap_plots(plots, ncol = 1, nrow = 6)
print(JOBplot2040)

ggsave("JOBplot2040.jpeg", JOBplot2040, width = 20, height = 25, dpi = 300)
#over



#fig-SI-Midpoint environmental mitigation-----
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
				
file_path_si1 <- "/.../environmentregion.xlsx"
region_envir_mid0 <- read_excel(file_path_si1, sheet = 2)
region_envir_mid <- region_envir_mid0 %>% group_by(region, scenario) %>%
  summarise(
    `Global Warming (Mt CO2-eq)` = -sum(`Global Warming_Mt-eq`),
    `Stratospheric Ozone Depletion (t CFC11-eq)` = -sum(`Stratospheric Ozone Depletion_t-eq`),
    `Photochemical Ozone Formation-human damage (t NOx-eq)` = -sum(`Photochemical Ozone Formation-human damage_t-eq`),
    `Fine Particulate Matter Formation (t PM2.5-eq)` = -sum(`Fine Particulate Matter Formation_t-eq`),
    `Photochemical Ozone Formation-ecosystem damage (t NOx-eq)` = -sum(`Photochemical Ozone Formation-ecosystem damage_t-eq`),
    `Terrestrial Acidification (Mt SO2-eq)` = -sum(`Terrestrial Acidification_Mt-eq`),
    `Freshwater Eutrophication (t P-eq. to freshwater)` = -sum(`Freshwater Eutrophication_t-eq`),
    `Marine Eutrophication (t N-eq to marine water)` = -sum(`Marine Eutrophication_t-eq`),
    `Fossil Resource Scarcity (Mt oil-eq)` = -sum(`Fossil Resource Scarcity_Mt-eq`),
  )

region_info <- data.frame(
  region = c("USA", "Taiwan", "Southeast Asia", "South Korea", "South Asia", 
             "South America_Southern", "South America_Northern", "South Africa", 
             "Russia", "Pakistan", "Middle East", "Mexico", "Japan", "Indonesia", 
             "India", "European Free Trade Association", "Europe_Non_EU", 
             "Europe_Eastern", "EU-15", "EU-12", "Colombia", "China", "Central Asia", 
             "Central America and Caribbean", "Canada", "Brazil", "Australia_NZ", 
             "Argentina", "Africa_Western", "Africa_Southern", "Africa_Northern", 
             "Africa_Eastern"),
  abbreviation = c("USA", "TWN", "SEA", "KOR", "SAS", "SAMS", "SAMN", "ZAF", 
                   "RUS", "PAK", "MEA", "MEX", "JPN", "IDN", "IND", "EFTA", 
                   "NEU", "EEU", "EU15", "EU12", "COL", "CHN", "CAS", "CAC", 
                   "CAN", "BRA", "ANZ", "ARG", "WAF", "SAF", "NAF", "EAF"),
  continent = c("North America", "Asia", "Asia", "Asia", "Asia", "South America", 
                "South America", "Africa", "Europe", "Asia", "Asia", "North America", 
                "Asia", "Asia", "Asia", "Europe", "Europe", "Europe", "Europe", 
                "Europe", "South America", "Asia", "Asia", "North America", 
                "North America", "South America", "Oceania", "South America", 
                "Africa", "Africa", "Africa", "Africa")
)

sorted_region_info <- region_info %>%
  arrange(continent, region)

region_order <- sorted_region_info$abbreviation

region_envir_mid_filtered <- region_envir_mid %>%
  left_join(region_info, by = "region") %>%
  filter(scenario %in% c("SSP1-H", "SSP2-M", "SSP4-M", "SSP5-L")) %>%
  mutate(region = factor(abbreviation, levels = region_order))

create_lollipop_plot <- function(data, variable, y_label) {
  ggplot(data, aes(x = region, y = !!sym(variable), color = scenario)) +
    geom_segment(aes(x = region, xend = region, y = 0, yend =!!sym(variable) ), 
                 size = 1, linetype = "dashed", color = "white") +  # 调整线条为虚线
    geom_segment(aes(x = as.numeric(region) + (as.numeric(factor(scenario)) - 2.5) * 0.15, 
                     xend = as.numeric(region) + (as.numeric(factor(scenario)) - 2.5) * 0.15, 
                     y = 0, yend = !!sym(variable)), 
                 size = 1) +
    geom_point(aes(x = as.numeric(region) + (as.numeric(factor(scenario)) - 2.5) * 0.15),  # 圆形大小不变
               size = 4) +  # 固定大小
    scale_color_manual(values = c("SSP1-H" = "#CC0033", "SSP2-M" = "#0066CC" , 
                                  "SSP4-M" = "#339900", "SSP5-L" = "#FF6600")) +
    labs(x = NULL, y = y_label) +
    theme_minimal() +  # 使用简约主题
    theme(
      panel.grid.major = element_line(color = "white", linetype = "dashed"),  # 改为黑色虚线网格
      axis.text.x = element_text(angle = 45, hjust = 1, size = 20, color = "black"),  # 旋转x轴标签并设置大小
      axis.text.y = element_text(size = 20, color = "black"),  # 设置y轴标签字体大小
      axis.title.y = element_text(size = 16),  # 设置y轴标题字体大小
      axis.title.x = element_blank(),  # 去掉x轴标题
      panel.border = element_rect(color = "black", fill = NA, size = 1.5),  # 添加黑色边框
      legend.title = element_blank(),  # 移除图例标题
      legend.position = c(0.93, 0.999),  # 将图例放置在图片下方中间
      legend.justification = c("center", "top"),  # 图例放在下方中心
      legend.direction = "vertical",  # 图例水平排列
      legend.text = element_text(size = 18),  # 设置图例字体大小
      legend.background = element_rect(fill = NA, color = NA)  # 设置图例底色为白色
    ) +
    scale_y_continuous(expand = c(0.01, 0))  # y轴从0开始，不要空白
}

region_envir_mid_filtered <- region_envir_mid_filtered %>%
  mutate(region = factor(region, levels = region_order))

plot_list <- lapply(names(region_envir_mid_filtered)[3:11], function(variable) {
  create_lollipop_plot(region_envir_mid_filtered, variable, variable)
})

library(cowplot)
library(grid)  

plot_list <- lapply(plot_list, function(p) {
  p + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # 设置上、右、下、左的边距为 1 cm
})

combined_plot <- plot_grid(plotlist = plot_list, ncol = 3, nrow = 3, align = 'hv')

ggsave("Plot_SI_Midpoint.jpeg", combined_plot, width = 44, height = 22, dpi = 300)
