# Figure 2A ---------------------------------------------------------------
library(sf)
load("0.demo_data/GBD_map.Rdata")
load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")
load("0.demo_data/Quantile.Rdata")

variable = "val_Discrete"
guide_name = paste0(ASR_suffix," in ",StartYear,"\n(per 100,000)")

GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))
GBD_data_nation =  GBD_data %>% filter(age_name=='Age-standardized',measure_name==measure_name_filter,sex_name=="Both",metric_name== 'Rate',location_id %in% nation_region_ids,year%in%c(StartYear,EndYear)) %>% 
  select(c( "location_id","location_name", "year", "val", "upper", "lower")) %>% rename(ASR = val,ASR_upper = upper,ASR_lower = lower)
fwrite(GBD_data_nation,file.path(Figure_folder,paste0("Figure2A_world_map_",ASR_suffix,".csv")))
maxN = max(GBD_data_nation$ASR)

GBD_data_nation_StartYear = GBD_data_nation %>% filter(year==StartYear)

GBD_data_nation_StartYear$val_Discrete = Quantile_Number(GBD_data_nation_StartYear$ASR,n = 8,maxN)

GBD_data_location_id_map = read.xlsx("0.demo_data/GBD_data_location_id_map.xlsx") %>% filter(nation_region=="nation") %>% select(location_id,GBD_location_map)

GBD_data_map = left_join(GBD_data_nation_StartYear,GBD_data_location_id_map,by = "location_id")


GBD_data_map$GBDvar <- GBD_data_map[[variable]]

worldData <- map_data("world")
GBD_nation_small_map_df = read.xlsx("0.demo_data/GBD_location_small_map.xlsx") %>% select(c("location_id", "region"))
small_map_data <- left_join(GBD_data_map,GBD_nation_small_map_df,by = "location_id")

small_map_data_df = small_map_data[,c( "region", "year", "ASR","ASR_upper", "ASR_lower" )] %>% arrange(ASR)



small_map_data <- full_join(worldData, small_map_data, by = "region")

small_map_data <- small_map_data[which(!is.na(small_map_data[,
                                                             which(names(small_map_data) %in% variable)])),]



# map ----------------------------------------------------------------------
main_map_plot <- small_map_data %>%
  ggplot() +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = GBDvar
    ),
    color = 'black',
    size = 0.01
  ) +
  theme_void() +
  labs(x = NULL, y = NULL, title = NULL, fill = guide_name) +
  theme(
    legend.position = c(0.08, 0.2),
    legend.title = element_text(color = 'black', size = legend.title.size),,
    legend.text = element_text(color = 'black', size = legend.text.size),
    legend.key.size = unit(0.4, "cm"),
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0, unit = "pt")
  ) +
  scale_fill_manual(values = map_color_ASR)+
  guides(fill = guide_legend(ncol = 1)) +
  scale_x_continuous(expand = c(0.001, 0.001)) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1)))




small_map_plot <- small_map_data %>%
  ggplot() +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = GBDvar
    ),
    color = 'black',
    size = 0.1
  ) +
  theme_bw() +
  theme(
    legend.position = 'none',
    legend.title = element_blank(),
    plot.title = element_text(color = 'black', size = 10),
    legend.text = element_text(color = 'black', size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = 'black',
      fill = NA,
      size = 0.5
    ),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )  +
  scale_fill_manual(values = map_color_ASR)


p2 <-
  small_map_plot + labs(x = " ", y = "", title = "North America") +
  coord_cartesian(xlim = c(-120, -70), ylim = c(30, 60))



p3 <-
  small_map_plot + labs(x = " ", y = "", title = "Eastern Europe") +
  coord_cartesian(xlim = c(25, 50), ylim = c(40, 60))


p4 <-
  small_map_plot + labs(x = " ", y = "", title = "South Africa") +
  coord_cartesian(xlim = c(20, 35), ylim = c(-30, -10))


p5 <-
  small_map_plot + labs(x = " ", y = "", title = "Central Asia") +
  coord_cartesian(xlim = c(55, 80), ylim = c(35, 55))


p6 <-
  small_map_plot + labs(x = " ", y = "", title = "West Africa") +
  coord_cartesian(xlim = c(-17, -7), ylim = c(7, 20))


p7 <-
  small_map_plot + labs(x = " ", y = "", title = "Oceania") +
  coord_cartesian(xlim = c(130, 145), ylim = c(-35, 0))
p8 <-
  small_map_plot + labs(x = " ", y = "", title = "South America") +
  coord_cartesian(xlim = c(-75, -45), ylim = c(-20, 5))

A = (p6 | p7) / p8


plot <- main_map_plot / 
  (p2 + p3 + p4 + p5 + A + 
     plot_layout(ncol = 5, widths = c(1, 1, 1, 1, 1))) +
  plot_layout(ncol = 1, heights = c(7, 3)) +  
  plot_annotation(theme = theme(plot.margin = margin(-20, 0, 0, 0)))

ggsave(filename = file.path(Figure_folder,paste0("Figure2A_world_map_",ASR_suffix,"_StartYear.pdf")), plot, width = 9, height = 6)




# Figure 2B ---------------------------------------------------------------

library(sf)
load("0.demo_data/GBD_map.Rdata")
load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")
load("0.demo_data/Quantile.Rdata")

variable = "val_Discrete"
guide_name = paste0(ASR_suffix," in ",EndYear,"\n(per 100,000)")
GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))
GBD_data_nation =  GBD_data %>% filter(age_name=='Age-standardized',measure_name==measure_name_filter,sex_name=="Both",metric_name== 'Rate',location_id %in% nation_region_ids,year%in%c(StartYear,EndYear)) %>% 
  select(c( "location_id","location_name", "year", "val", "upper", "lower")) %>% rename(ASR = val,ASR_upper = upper,ASR_lower = lower)
maxN = max(GBD_data_nation$ASR)

GBD_data_nation_EndYear = GBD_data_nation %>% filter(year==EndYear)
GBD_data_nation_EndYear$val_Discrete = Quantile_Number(GBD_data_nation_EndYear$ASR,n = 8,maxN)

GBD_data_location_id_map = read.xlsx("0.demo_data/GBD_data_location_id_map.xlsx") %>% filter(nation_region=="nation") %>% select(location_id,GBD_location_map)

GBD_data_map = left_join(GBD_data_nation_EndYear,GBD_data_location_id_map,by = "location_id")


GBD_data_map$GBDvar <- GBD_data_map[[variable]]

worldData <- map_data("world")
GBD_nation_small_map_df = read.xlsx("0.demo_data/GBD_location_small_map.xlsx") %>% select(c("location_id", "region"))
small_map_data <- left_join(GBD_data_map,GBD_nation_small_map_df,by = "location_id")

small_map_data_df = small_map_data[,c( "region", "year", "ASR","ASR_upper", "ASR_lower" )] %>% arrange(ASR)
small_map_data <- full_join(worldData, small_map_data, by = "region")

small_map_data <- small_map_data[which(!is.na(small_map_data[,
                                                             which(names(small_map_data) %in% variable)])),]


# map ----------------------------------------------------------------------
main_map_plot <- small_map_data %>%
  ggplot() +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = GBDvar
    ),
    color = 'black',
    size = 0.01
  ) +
  theme_void() +
  labs(x = NULL, y = NULL, title = NULL, fill = guide_name) +
  theme(
    legend.position = c(0.08, 0.2),
    legend.title = element_text(color = 'black', size = legend.title.size),,
    legend.text = element_text(color = 'black', size = legend.text.size),
    legend.key.size = unit(0.4, "cm"),
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0, unit = "pt")
  ) +
  scale_fill_manual(values = map_color_ASR)+
  guides(fill = guide_legend(ncol = 1)) +
  scale_x_continuous(expand = c(0.001, 0.001)) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1)))




small_map_plot <- small_map_data %>%
  ggplot() +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = GBDvar
    ),
    color = 'black',
    size = 0.1
  ) +
  theme_bw() +
  theme(
    legend.position = 'none',
    legend.title = element_blank(),
    plot.title = element_text(color = 'black', size = 10),
    legend.text = element_text(color = 'black', size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = 'black',
      fill = NA,
      size = 0.5
    ),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )  +
  scale_fill_manual(values = map_color_ASR)


p2 <-
  small_map_plot + labs(x = " ", y = "", title = "North America") +
  coord_cartesian(xlim = c(-120, -70), ylim = c(30, 60))




p3 <-
  small_map_plot + labs(x = " ", y = "", title = "Eastern Europe") +
  coord_cartesian(xlim = c(25, 50), ylim = c(40, 60))


p4 <-
  small_map_plot + labs(x = " ", y = "", title = "South Africa") +
  coord_cartesian(xlim = c(20, 35), ylim = c(-30, -10))


p5 <-
  small_map_plot + labs(x = " ", y = "", title = "Central Asia") +
  coord_cartesian(xlim = c(55, 80), ylim = c(35, 55))


p6 <-
  small_map_plot + labs(x = " ", y = "", title = "West Africa") +
  coord_cartesian(xlim = c(-17, -7), ylim = c(7, 20))


p7 <-
  small_map_plot + labs(x = " ", y = "", title = "Oceania") +
  coord_cartesian(xlim = c(130, 145), ylim = c(-35, 0))
p8 <-
  small_map_plot + labs(x = " ", y = "", title = "South America") +
  coord_cartesian(xlim = c(-75, -45), ylim = c(-20, 5))

A = (p6 | p7) / p8


plot <- main_map_plot / 
  (p2 + p3 + p4 + p5 + A + 
     plot_layout(ncol = 5, widths = c(1, 1, 1, 1, 1))) +
  plot_layout(ncol = 1, heights = c(7, 3)) +  
  plot_annotation(theme = theme(plot.margin = margin(-20, 0, 0, 0)))

ggsave(filename = file.path(Figure_folder,paste0("Figure2B_world_map_",ASR_suffix,"_EndYear.pdf")), plot, width = 9, height = 6)



# Figure 2C ---------------------------------------------------------------

library(sf)
load("0.demo_data/GBD_map.Rdata")
load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")
load("0.demo_data/Quantile.Rdata")
source("0.GBD_EAPC.R")

# variable = "EAPC"
variable = "val_Discrete"
guide_name = paste0("EAPC of ",ASR_suffix," (% per year)")

GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))
GBD_data_nation =  GBD_data %>% filter(age_name=='Age-standardized',measure_name==measure_name_filter,sex_name=="Both",metric_name== 'Rate',location_id %in% nation_region_ids) 


GBD_data_location_id_map = read.xlsx("0.demo_data/GBD_data_location_id_map.xlsx") %>% filter(nation_region=="nation") %>% select(location_id,GBD_location_map)

GBD_data_nation_EAPC = GBD_EAPC(data = GBD_data_nation,rei = F,EAPC_95CI = T,digits = 3,sep = ",")

fwrite(GBD_data_nation_EAPC,file.path(Figure_folder,"Figure2C_world_map_EAPC.csv"))

GBD_data_nation_EAPC$val_Discrete = Quantile_neg_pos(GBD_data_nation_EAPC$EAPC,n = 5,precision = 2)

GBD_data_map = left_join(GBD_data_nation_EAPC,GBD_data_location_id_map,by = "location_id")

GBD_data_map$GBDvar <- GBD_data_map[[variable]]

# EAPC_map_color = c("#21528AFF",  "#3280B5FF", "#A4D3EE", "#A1D99B", "#FFB90F" )
worldData <- map_data("world")
GBD_nation_small_map_df = read.xlsx("0.demo_data/GBD_location_small_map.xlsx") %>% select(c("location_id", "region"))
small_map_data <- left_join(GBD_data_map,GBD_nation_small_map_df,by = "location_id")

small_map_data_df = small_map_data %>% select(c("region", "EAPC", "LCI", "UCI"))%>% arrange(EAPC)

small_map_data <- full_join(worldData, small_map_data, by = "region")

small_map_data <- small_map_data[which(!is.na(small_map_data[,
                                                             which(names(small_map_data) %in% variable)])),]

# map ----------------------------------------------------------------------

main_map_plot <- small_map_data %>%
  ggplot() +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = GBDvar
    ),
    color = 'black',
    size = 0.01
  ) +
  theme_void() +
  labs(x = NULL, y = NULL, title = NULL, fill = guide_name) +
  theme(
    legend.position = c(0.12, 0.2),
    legend.title = element_text(color = 'black', size = legend.title.size),,
    legend.text = element_text(color = 'black', size = legend.text.size),
    legend.key.size = unit(0.4, "cm"),
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0, unit = "pt")
  ) +
  scale_fill_manual(values = EAPC_map_color)+
  guides(fill = guide_legend(ncol = 1)) +
  scale_x_continuous(expand = c(0.001, 0.001)) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1)))




small_map_plot <- small_map_data %>%
  ggplot() +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = GBDvar
    ),
    color = 'black',
    size = 0.1
  ) +
  theme_bw() +
  theme(
    legend.position = 'none',
    legend.title = element_blank(),
    plot.title = element_text(color = 'black', size = 10),
    legend.text = element_text(color = 'black', size = 12),
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = 'black',
      fill = NA,
      linewidth = 0.5
    ),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )  +
  scale_fill_manual(values = EAPC_map_color)


p2 <-
  small_map_plot + labs(x = " ", y = "", title = "North America") +
  coord_cartesian(xlim = c(-120, -70), ylim = c(30, 60))




p3 <-
  small_map_plot + labs(x = " ", y = "", title = "Eastern Europe") +
  coord_cartesian(xlim = c(25, 50), ylim = c(40, 60))


p4 <-
  small_map_plot + labs(x = " ", y = "", title = "South Africa") +
  coord_cartesian(xlim = c(20, 35), ylim = c(-30, -10))


p5 <-
  small_map_plot + labs(x = " ", y = "", title = "Central Asia") +
  coord_cartesian(xlim = c(55, 80), ylim = c(35, 55))


p6 <-
  small_map_plot + labs(x = " ", y = "", title = "West Africa") +
  coord_cartesian(xlim = c(-17, -7), ylim = c(7, 20))


p7 <-
  small_map_plot + labs(x = " ", y = "", title = "Oceania") +
  coord_cartesian(xlim = c(130, 145), ylim = c(-35, 0))
p8 <-
  small_map_plot + labs(x = " ", y = "", title = "South America") +
  coord_cartesian(xlim = c(-75, -45), ylim = c(-20, 5))

A = (p6 | p7) / p8


plot <- main_map_plot / 
  (p2 + p3 + p4 + p5 + A + 
     plot_layout(ncol = 5, widths = c(1, 1, 1, 1, 1))) +
  plot_layout(ncol = 1, heights = c(7, 3)) +  
  plot_annotation(theme = theme(plot.margin = margin(-20, 0, 0, 0)))

ggsave(filename = file.path(Figure_folder,"Figure2C_world_map_EAPC.pdf"), plot, width = 9, height = 6)


