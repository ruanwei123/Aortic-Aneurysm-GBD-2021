# Figure1A ----------------------------------------------------------------

load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")

GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))
location_name_level = c("Global",SDI_region_level)

SDI_region_Mortality = GBD_data %>% filter(age_name=='All ages',measure_name==measure_name_filter,sex_name=="Both",metric_name== 'Number',location_name %in% location_name_level) %>% 
  select(c( "location_id","location_name", "year", "val", "upper", "lower")) %>% rename(Number = val,Number_upper = upper,Number_lower = lower) %>% arrange(location_name,year)

fwrite(SDI_region_Mortality,file = file.path(Figure_folder,paste0("Figure1A_SDI_region_",measure_name_suffix
,".csv")))
SDI_region_Mortality = SDI_region_Mortality %>% select(c( "location_name", "year", "Number"))
# Convert 'Number' to thousands (K)
SDI_region_Mortality[, Number := Number / 1000]

# Create a separate data frame for 'Global' data
global_data <- SDI_region_Mortality %>%
  filter(location_name == "Global") %>%
  arrange(year)

other_data <- SDI_region_Mortality %>%
  filter(location_name != "Global") %>%
  arrange(year)


# --------------------------------------------------------------------
library(plotrix)

pdf(file = file.path(Figure_folder,paste0("Figure1A_SDI_region_",measure_name_suffix
,".pdf")), width = 6, height = 5)

par(mar = c(2.3, 4.2, 0.3, 4.2))

ylim_left_min = 0
ylim_left <- c(ylim_left_min, 80)

ylim_right_min = 80
ylim_right <- c(60,180)

break_point_right <- 65

plot(other_data$year, other_data$Number, type = "n", 
     xlab = "", ylab = "", ylim = ylim_left, 
     xaxt = "n", yaxt = "n", bty = "n")

par(mgp = c(2, 0.8, 0))
axis(1, at = seq(StartYear, EndYear, by = 5), las = 1)

axis(2, at = seq(0, max(ylim_left), length.out = 5), las = 1)

# mtext("Year", side = 1, line = 3)
mtext(text = Figure1A_ylab_text1, side = 2, line = 3, cex = 1.2, font = 2)

lines(c(par("usr")[1], par("usr")[2]), c(par("usr")[3], par("usr")[3]), xpd = TRUE)  
lines(c(par("usr")[1], par("usr")[2]), c(par("usr")[4], par("usr")[4]), xpd = TRUE)  
lines(c(par("usr")[1], par("usr")[1]), c(par("usr")[3], par("usr")[4]), xpd = TRUE)  
lines(c(par("usr")[2], par("usr")[2]), c(par("usr")[3], par("usr")[4]), xpd = TRUE) 

for(i in 1:length(SDI_region_level)) {
  region_data <- other_data[other_data$location_name == SDI_region_level[i],]
  region_data <- region_data[order(region_data$year),]
  lines(region_data$year, region_data$Number, col = SDI_colors[i], lwd = 2)
  points(region_data$year, region_data$Number, col = SDI_colors[i], pch = 16, cex = 0.7)
}

par(new = TRUE)

plot(global_data$year, global_data$Number, type = "n", 
     xaxt = "n", yaxt = "n", xlab = "", ylab = "", ylim = ylim_right, bty = "n")

axis(4, at = seq(ylim_right_min, max(ylim_right), length.out = 6), las = 1)

axis.break(4, breakpos = break_point_right, style = "slash")

mtext(text = Figure1A_ylab_text2, side = 4, line = 3, cex = 1.2, font = 2)

lines(global_data$year, global_data$Number, col = "black", lwd = 2)
points(global_data$year, global_data$Number, col = "black", pch = 16, cex = 0.7)

dev.off()


# Figure1B ----------------------------------------------------------------

load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")

GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))
location_name_level = c("Global",SDI_region_level)

# Function to generate plot for a specific sex_name
generate_ASR_plot <- function(data, measure = "Incidence", ASR_suffix = "ASIR", region = "Global",mycolrs = "red",StartYear=1990,EndYear=2021) {
  # Filter and process data for each sex
  SDI_region_ASR <- data %>% 
    filter(age_name == 'Age-standardized',
           measure_name == measure,
           metric_name == 'Rate',
           location_name %in% region,
           sex_name %in% c( "Both")) %>%
    select(c( "location_name","sex_name", "year", "val", "upper", "lower")) %>%
    rename(ASR = val,ASR_upper = upper,ASR_lower = lower)%>% arrange(location_name,year)
  
  fwrite(SDI_region_ASR, file = file.path(Figure_folder, paste0("Figure1B_SDI_region_", ASR_suffix, ".csv")))
  SDI_region_ASR = SDI_region_ASR %>% select(c( "location_name","sex_name", "year", "ASR"))
  Global_SDI_level = c("Global", "High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI")
  SDI_region_ASR$location_name = factor(SDI_region_ASR$location_name, levels = Global_SDI_level)
  
    p <- ggplot(data = SDI_region_ASR, aes(x = year, y = ASR, color = location_name, group = location_name)) +
        geom_line(linewidth = 0.7) +
        geom_point(size = 1) +
        # facet_wrap(~ sex_name, scales = "fixed", nrow = 1) +  
        labs(
          x = NULL,
          y = paste0(ASR_suffix," (per 100,000)"),
          color = "SDI Level",
          shape = "SDI Level"
        ) +
      scale_y_continuous(breaks = seq(0, 6, by = 1), limits = c(0, 6)) +
      scale_x_continuous(breaks = seq(StartYear, EndYear, by = 5), limits = c(StartYear, EndYear)) +
      theme_minimal() +
  scale_color_manual(values = mycolrs) +
  scale_shape_manual(values = c(19, 9, 17, 25, 15, 8)) +
  theme(
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(0.2, "cm"), 
    # strip.text = element_text(size = 18, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 16,face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.5),
    legend.position = "left",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),  
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.5, "lines")
  ) +
  coord_cartesian(clip = "off")  
  return(p)
}

ASMR_plot <- generate_ASR_plot(data = GBD_data, measure = measure_name_filter, ASR_suffix = ASR_suffix, region = location_name_level,mycolrs = Global_SDI_color,StartYear,EndYear)

ggsave2(filename = file.path(Figure_folder,paste0("Figure1B_SDI_region_",ASR_suffix
,".pdf")),plot = ASMR_plot,width = 7,height = 5)




# Figure1C ----------------------------------------------------------------

load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")

GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))

GBD_region_ASR <- GBD_data %>% 
  filter(age_name == 'Age-standardized',
         measure_name == measure_name_filter,
         metric_name == 'Rate',
         location_id %in% GBD_region_ids,
         sex_name %in% c("Both")) %>%
  select(c("location_id", "location_name", "year", "val", "upper", "lower")) %>%
  rename(ASR = val,ASR_upper = upper,ASR_lower = lower)

GBD_region_Number <-
  GBD_data %>% filter(
    age_name == 'All ages',
    measure_name == measure_name_filter,
    metric_name == 'Number',
    location_id %in% GBD_region_ids,
    sex_name %in% c("Both") ) %>%
  select(c( "location_id", "location_name", "year", "val", "upper", "lower")) %>%
  rename(Number = val,Number_upper = upper,Number_lower = lower)

GBD_region_ASR_Number = left_join(GBD_region_ASR,GBD_region_Number,by = c("location_id","location_name","year"))%>% arrange(location_id,year)
fwrite(GBD_region_ASR_Number,file.path(Figure_folder,"Figure1C_GBD_region_ASR_Number.csv"))

GBD_region_ASR_Number = GBD_region_ASR_Number %>% select(c("location_name", "year", "ASR", "Number"))

GBD_region_ASR_Number$Number = GBD_region_ASR_Number$Number/1000
GBD_region_ASR_Number$location_name = factor(GBD_region_ASR_Number$location_name,levels = rev(GBD_region_level))

Figure1C = ggplot(GBD_region_ASR_Number, aes(x = year, y = location_name, size = ASR, color = Number)) +
  geom_point(alpha = 0.7) +
  scale_color_gradientn(colors = Bubble_color)+
  scale_size_continuous(range = c(0.5, 8)) +  
  labs(title = Figure1C_title,
       x = NULL,
       y = NULL,
       size = paste0(ASR_suffix," (per 100,000)"),
       color = Figure1C_legend) +
  theme_minimal() +
  scale_x_continuous( expand = c(0.015, 0.1)) +
  # scale_y_continuous(expand = expansion(mult = c(0.02, 0.01)))+
  theme(axis.text = element_text(size = 14,color = "black"),
        plot.title = element_text(color = "black", size = 16, hjust = 0.5, face = "bold.italic"),
        panel.spacing.x = unit(0, "lines"),  
        plot.margin = margin(10, 1, 0, 1),
        axis.ticks.length = unit(0.1, "cm"),
        legend.position = "bottom") +
  guides(size = guide_legend(order = 1),
         color = guide_colorbar(order = 2))

ggsave2(filename = file.path(Figure_folder,paste0("Figure1C_GBD_region_",ASR_suffix
,".pdf")),plot = Figure1C,width = 12,height = 8)




# Table S4 ----------------------------------------------------------------
load("0.demo_data/age_location_level.Rdata")
SDI_region_Mortality = fread(file = file.path(Figure_folder,paste0("Figure1A_SDI_region_",measure_name_suffix
,".csv")))
SDI_region_ASR = fread(file = file.path(Figure_folder, paste0("Figure1B_SDI_region_", ASR_suffix, ".csv")))%>% select(-c("sex_name"))
GBD_region_ASR_Number = fread(file = file.path(Figure_folder,"Figure1C_GBD_region_ASR_Number.csv"))%>% arrange("location_id","year")

SDI_region_Mortality_ASR = left_join(SDI_region_Mortality,SDI_region_ASR,by = c("location_name", "year")) %>% arrange("location_id")
TableS4 = bind_rows(SDI_region_Mortality_ASR,GBD_region_ASR_Number)
TableS4 = TableS4 %>% Precision(column_names = c( "Number", "Number_upper", "Number_lower", 
"ASR", "ASR_upper", "ASR_lower"),precision = 2,precisionC = 2) %>% 
mutate(Number_UI = sprintf("%s (%s,%s)", Number, Number_lower, Number_upper),
       ASR_UI = sprintf("%s (%s,%s)", ASR, ASR_lower, ASR_upper)) %>%select(c("location_id","location_name", "year","Number_UI","ASR_UI"))
write.xlsx(TableS4,file = file.path(Table_folder,paste0(TableS4_name,".xlsx")),sheetName = TableS4_name,overwrite = TRUE)
