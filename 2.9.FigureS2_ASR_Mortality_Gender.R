# Figure1A ----------------------------------------------------------------

load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")

GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))
location_name_level = c("Global",SDI_region_level)


SDI_region = read.xlsx("0.demo_data/GBD_data_location_id.xlsx") %>%filter(nation_region%in%c("Global","SDI_region")) %>%  select(location_name,nation_region) %>% .$location_name

SDI_region_Mortality = GBD_data %>% filter(age_name=='All ages',measure_name==measure_name_filter,sex_name=="Male",metric_name== 'Number',location_name %in% location_name_level) %>% 
  select(c( "location_name", "year", "val", "upper", "lower")) %>% rename(Number = val) %>% arrange(location_name,year)


fwrite(SDI_region_Mortality,file = file.path(Figure_folder,"FigureS2A_SDI_region_Male.csv"))
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
library(plotrix)

pdf(file = file.path(Figure_folder,"FigureS2A_SDI_region_Male.pdf"), width = 6, height = 5.2)

par(mar = c(2.3, 4.2, 1.9,  4.2))

ylim_left_min = 0
ylim_left <- c(ylim_left_min, round(max(other_data$Number)/900)*1000)

ylim_right_min = round(min(global_data$Number)/1000)*1000
ylim_right <- c(ylim_right_min*0.9, round(max(global_data$Number)/1000)*1000)   

break_point_right <- ylim_right_min*0.92

plot(other_data$year, other_data$Number, type = "n", 
     xlab = "", ylab = "", ylim = ylim_left, 
     xaxt = "n", yaxt = "n", bty = "n")

par(mgp = c(2, 0.8, 0))
axis(1, at = seq(StartYear, EndYear, by = 5), las = 1)

axis(2, at = seq(0, max(ylim_left), length.out = 5), las = 1)

mtext(text = Figure1A_ylab_text1, side = 2, line = 3, cex = 1.3, font = 2)

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

axis(4, at = seq(ylim_right_min, max(ylim_right), length.out = 5), las = 1)

axis.break(4, breakpos = break_point_right, style = "slash")

mtext(text = Figure1A_ylab_text2,side = 4, line = 3, cex = 1.3, font = 2)



lines(global_data$year, global_data$Number, col = "black", lwd = 2)
points(global_data$year, global_data$Number, col = "black", pch = 16, cex = 0.7)

mtext("Male", side = 3, line = 0.3, cex = 1.6, font = 2)

dev.off()

rm(list = ls())


# Figure1B ----------------------------------------------------------------

load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")

GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))
location_name_level = c("Global",SDI_region_level)


SDI_region = read.xlsx("0.demo_data/GBD_data_location_id.xlsx") %>%filter(nation_region%in%c("Global","SDI_region")) %>%  select(location_name,nation_region) %>% .$location_name

SDI_region_Mortality = GBD_data %>% filter(age_name=='All ages',measure_name==measure_name_filter,sex_name=="Female",metric_name== 'Number',location_name %in% location_name_level) %>% 
  select(c( "location_name", "year", "val", "upper", "lower")) %>% rename(Number = val) %>% arrange(location_name,year)

fwrite(SDI_region_Mortality,file = file.path(Figure_folder,"FigureS2B_SDI_region_Female.csv"))
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

library(plotrix)

pdf(file = file.path(Figure_folder,"FigureS2B_SDI_region_Female.pdf"), width = 6, height = 5.2)

par(mar = c(2.3, 4.2, 1.9,  4.2))

ylim_left_min = 0
ylim_left <- c(ylim_left_min, round(max(other_data$Number)/90)*100)

ylim_right_min = round(min(global_data$Number)/100)*100
ylim_right <- c(ylim_right_min*0.9, round(max(global_data$Number)/1000)*1000)   

break_point_right <- ylim_right_min*0.92

plot(other_data$year, other_data$Number, type = "n", 
     xlab = "", ylab = "", ylim = ylim_left, 
     xaxt = "n", yaxt = "n", bty = "n")

par(mgp = c(2, 0.8, 0))
axis(1, at = seq(StartYear, EndYear, by = 5), las = 1)

axis(2, at = seq(0, max(ylim_left), length.out = 5), las = 1)

mtext(text = Figure1A_ylab_text1, side = 2, line = 3, cex = 1.3, font = 2)

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

axis(4, at = seq(ylim_right_min, max(ylim_right), length.out = 5), las = 1)

axis.break(4, breakpos = break_point_right, style = "slash")

mtext(text = Figure1A_ylab_text2, side = 4, line = 3, cex = 1.3, font = 2)



lines(global_data$year, global_data$Number, col = "black", lwd = 2)
points(global_data$year, global_data$Number, col = "black", pch = 16, cex = 0.7)
mtext("Female", side = 3, line = 0.3, cex = 1.6, font = 2)
dev.off()


# Figure1C ----------------------------------------------------------------

load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")

cause_name = read.xlsx("0.variable_df.xlsx") %>% .$cause_name

# Read data
GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))
location_name_level = c("Global",SDI_region_level)

# Function to generate plot for a specific sex_name
generate_ASR_plot <- function(data, measure = "Incidence", suffix = "ASIR", region = "Global",mycolrs = "red",StartYear=1990,EndYear=2021) {
  # Filter and process data for each sex
  SDI_region_ASR <- data %>% 
    filter(age_name == 'Age-standardized',
           measure_name == measure,
           metric_name == 'Rate',
           location_name %in% region,
           sex_name %in% c( "Male","Female")) %>%
    select(c( "location_name","sex_name", "year", "val", "upper", "lower")) %>%
    rename(ASR = val)%>% arrange(location_name,year)
  
  fwrite(SDI_region_ASR, file = file.path(Figure_folder, paste0("FigureS2C_SDI_region_", suffix, ".csv")))
  SDI_region_ASR = SDI_region_ASR %>% select(c( "location_name","sex_name", "year", "ASR"))
  Global_SDI_level = c("Global", "High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI")
  SDI_region_ASR$location_name = factor(SDI_region_ASR$location_name, levels = Global_SDI_level)
  
    p <- ggplot(data = SDI_region_ASR, aes(x = year, y = ASR, color = location_name, group = location_name)) +
        geom_line(linewidth = 0.7) +
        geom_point(size = 1) +
        facet_wrap(~ sex_name, scales = "fixed", nrow = 1) +  # 使用固定刻度
        labs(
          x = NULL,
          y = paste0(suffix," (per 100,000)"),
          color = "SDI Level",
          shape = "SDI Level"
        ) +
      scale_x_continuous(breaks = seq(StartYear, EndYear, by = 5), limits = c(StartYear, EndYear)) +
      theme_minimal() +
  scale_color_manual(values = Global_SDI_color) +
  scale_shape_manual(values = c(19, 9, 17, 25, 15, 8)) +
  theme(
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm"), 
    strip.text = element_text(size = 12, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12,face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.5),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),  
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.5, "lines")
  ) +
      # guides(color = guide_legend(nrow = 1))+  #
  coord_cartesian(clip = "off")  
  return(p)
}



# Generate combined plot for the specified measure and region
ASMR_plot <- generate_ASR_plot(data = GBD_data, measure = measure_name_filter, suffix = ASR_suffix, region = location_name_level,mycolrs = Global_SDI_color,StartYear,EndYear)

ggsave2(filename = file.path(Figure_folder,"FigureS2C_SDI_region_ASR.pdf"),plot = ASMR_plot,width = 7,height = 4)



# Figure1D ----------------------------------------------------------------

load("0.demo_data/age_location_level.Rdata")

# Read data
GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))
location_name_level = c("Global",SDI_region_level)

Sex_Ratio_Number_df <- GBD_data %>%
  filter(age_name == 'All ages',
         measure_name == measure_name_filter,
         metric_name == 'Number',
         location_name %in% location_name_level,
         sex_name %in% c("Male", "Female")) %>%
  select(c("location_name", "sex_name", "year", "val")) %>%
  rename(Number = val) %>%
  pivot_wider(names_from = sex_name, values_from = Number) %>%
  mutate(Ratio = Male / Female) %>%
  select(location_name, year, Ratio) %>%
  arrange(location_name, year)

fwrite(Sex_Ratio_Number_df, file = file.path(Figure_folder, "FigureS2D_Sex_Ratio_Number.csv"))


Sex_Ratio_ASR_df <- GBD_data %>%
  filter(age_name == 'Age-standardized',
         measure_name == measure_name_filter,
         metric_name == 'Rate',
         location_name %in% location_name_level,
         sex_name %in% c("Male", "Female")) %>%
  select(c("location_name", "sex_name", "year", "val")) %>%
  rename(ASR = val) %>%
  pivot_wider(names_from = sex_name, values_from = ASR) %>%
  mutate(Ratio = Male / Female) %>%
  select(location_name, year, Ratio) %>%
  arrange(location_name, year)
fwrite(Sex_Ratio_ASR_df, file = file.path(Figure_folder, "FigureS2E_Sex_Ratio_ASR.csv"))

Sex_Ratio_Number_df$location_name = factor(Sex_Ratio_Number_df$location_name,levels = location_name_level)
Sex_Ratio_ASR_df$location_name = factor(Sex_Ratio_ASR_df$location_name,levels = location_name_level)

FigureS2D = ggplot(data = Sex_Ratio_Number_df, aes(x = year, y = Ratio, color = location_name, group = location_name)) +
        geom_line(linewidth = 0.7) +
        geom_point(size = 1) +
        labs(
          x = NULL,
          y = paste0("Ratio of Male to Female ",measure_name_filter),
          color = "SDI Level",
          shape = "SDI Level"
        )+
        scale_x_continuous(breaks = seq(StartYear, EndYear, by = 5), limits = c(StartYear, EndYear)) +
      theme_minimal() +
  scale_color_manual(values = c("black", "red", "purple", "#FF8C00", "#0076B9","#00A087FF")) +
  scale_shape_manual(values = c(19, 9, 17, 25, 15, 8)) +
  theme(
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(0.2, "cm"), #
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12,face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.5),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),  
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.5, "lines")
  ) +
  coord_cartesian(clip = "off")  

FigureS2E = ggplot(data = Sex_Ratio_ASR_df, aes(x = year, y = Ratio, color = location_name, group = location_name)) +
        geom_line(linewidth = 0.7) +
        geom_point(size = 1) +
        labs(
          x = NULL,
          y = "Ratio of Male to Female ASMR",
          color = "SDI Level",
          shape = "SDI Level"
        )+
        scale_x_continuous(breaks = seq(StartYear, EndYear, by = 5), limits = c(StartYear, EndYear)) +
      theme_minimal() +
  scale_color_manual(values = c("black", "red", "purple", "#FF8C00", "#0076B9","#00A087FF")) +
  scale_shape_manual(values = c(19, 9, 17, 25, 15, 8)) +
  theme(
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(0.2, "cm"), 
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12,face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.5),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 9),  
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.5, "lines")
  ) +
  coord_cartesian(clip = "off")  

shared_legend <- get_legend(
  FigureS2D + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

# Combine the plots
ASMR_Sex_Ratio_plot <- (FigureS2D | FigureS2E) /
  shared_legend +
  plot_layout(heights = c(10, 1))  # Adjust the ratio between plots and legend

ggsave2(filename = file.path(Figure_folder,"FigureS2D_ASMR_Sex_Ratio.pdf"),plot = ASMR_Sex_Ratio_plot,width = 7,height = 4)



# TableS1 -----------------------------------------------------------------
load("0.demo_data/age_location_level.Rdata")
Mortality_Male = fread(file = file.path(Figure_folder,"FigureS2A_SDI_region_Male.csv")) %>% 
  setnames(c("location_name", "year", "Number_Male", "upper_Male", "lower_Male"))

Mortality_Female = fread(file = file.path(Figure_folder,"FigureS2B_SDI_region_Female.csv"))%>% 
  setnames(c("location_name", "year", "Number_Female", "upper_Female", "lower_Female"))

ASR_df = fread(file = file.path(Figure_folder, paste0("FigureS2C_SDI_region_", ASR_suffix, ".csv")))
ASR_Male  = ASR_df %>% filter(sex_name=="Male") %>% select(-sex_name) %>% 
  setnames(c("location_name", "year", "ASR_Male", "ASR_upper_Male", "ASR_lower_Male"))
ASR_Female  = ASR_df %>% filter(sex_name=="Female")%>% select(-sex_name) %>% 
  setnames(c("location_name", "year", "ASR_Female", "ASR_upper_Female", "ASR_lower_Female"))

TableS7 = Mortality_Male %>% 
  left_join(Mortality_Female,by = c("location_name", "year") )%>% 
  left_join(ASR_Male,by = c("location_name", "year") )%>% 
  left_join(ASR_Female,by = c("location_name", "year") )

precise_col = setdiff(names(TableS7),c("location_name", "year"))

TableS7 = TableS7 %>% 
  Precision(column_names = precise_col,precision = 2) %>% 
  mutate(Mortality_Male_UI = sprintf("%s (%s,%s)", Number_Male, lower_Male, upper_Male),
       Mortality_Female_UI = sprintf("%s (%s,%s)", Number_Female, lower_Female, upper_Female),
       ASR_Male_UI = sprintf("%s (%s,%s)", ASR_Male, ASR_lower_Male, ASR_upper_Male),
       ASR_Female_UI = sprintf("%s (%s,%s)", ASR_Female, ASR_lower_Female, ASR_upper_Female)) %>% 
  select(c( "location_name","Mortality_Male_UI","Mortality_Female_UI","ASR_Male_UI","ASR_Female_UI")) 
write.xlsx(TableS7,file = file.path(Table_folder,paste0(TableS7_name,".xlsx")),sheetName = TableS7_name,overwrite = TRUE)
