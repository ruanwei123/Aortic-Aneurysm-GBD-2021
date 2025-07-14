library(BAPC)
library(INLA)
library(epitools)
library(ggplot2)
load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/APC_df_names.Rdata")


####################################
Projection_Year = EndYear+npredict_year  
Projection_Year_ids = seq(EndYear+1,Projection_Year,1)

location_ids_filter = c(Global_region_ids,GBD_region_ids)

####################################
location_name_df = location_df %>% select(location_id, location_name) 


GBD_data_df <- fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv"))) %>% 
  filter(age_id %in% age_APCmodel_ids,
         metric_name == 'Number',
         location_id %in% location_ids_filter) %>% 
  select(c( "measure_name","location_name", "age_id",  "sex_name","cause_name","metric_name",  "year", "val","upper","lower"))

GBD_data_age_population = read.xlsx(xlsxFile = "0.demo_data/GBD_data_age_population.xlsx")
GBD_data_df = GBD_data_df %>% left_join(GBD_data_age_population,by = "age_id")

names(GBD_data_df) = c("measure", "location", "age_id", "sex", "cause", 
"metric", "year", "val", "upper", "lower", "age")

GBD_data_df = GBD_data_df[,c("measure", "location", "sex", "age", "cause", 
"metric", "year", "val", "upper", "lower")] %>% filter(val>0)

location_name_filter = unique(GBD_data_df$location)
cause_name_filter = unique(GBD_data_df$cause)
measure_name_filter =  unique(GBD_data_df$measure)

norpred_result = GBDnorpred_prediction(
  data = GBD_data_df,
  measure_name = measure_name_filter,
  cause_name = cause_name_filter,
  location_name = location_name_filter,
  rei_name = NULL,
  By_sex = F,
  predyear = Projection_Year,
  full_age_adjusted = F
)
write.xlsx(norpred_result,file = file.path(Table_folder,"norpred_result.xlsx"))




# Figure6 norpred ---------------------------------------------------------


library(BAPC)
library(INLA)
library(epitools)
load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")
load("0.demo_data/APC_df_names.Rdata")


Projection_Year = EndYear+npredict_year 

location_name_levels = c("Global",GBD_region_level)


# Norpred_result_age_specific_proj -------------------------------------------

Norpred_result_age_specific_proj = read.xlsx(xlsxFile = file.path(Table_folder,"norpred_result.xlsx"),sheet = "age_specific_projection") %>% 
  filter(location %in% location_name_levels,
         sex %in% c( "Both")) 
names(Norpred_result_age_specific_proj) = c("measure_name", "location_name", "cause", "sex_name", "age_name", "year", "pred_val"
)

Norpred_result_age_specific_proj = Norpred_result_age_specific_proj[,c( "measure_name", 
"location_name", "sex_name", "age_name", "year","pred_val")]

# Create a mapping of original age groups to new age groups
age_mapping <- list(
  "15-29 years" = c("15 to 19", "20 to 24", "25 to 29"),
  "30-44 years" = c("30 to 34", "35 to 39","40 to 44"),
  "45-59 years" = c("45 to 49", "50 to 54", "55 to 59"),
  "60-75 years" = c("60 to 64", "65 to 69", "70 to 74"),
  "75+ years" = c("75 to 79", "80 to 84", "85 to 89", "90 to 94")
)

# Function to map original age groups to new age groups
map_age_groups <- function(age) {
  for (new_group in names(age_mapping)) {
    if (age %in% age_mapping[[new_group]]) {
      return(new_group)
    }
  }
  return(age)  # Return original if not found in mapping
}

# Apply the mapping and aggregate the data
Norpred_result_age_strat <- Norpred_result_age_specific_proj %>%
  mutate(age_strat = sapply(age_name, map_age_groups)) %>%
  group_by(measure_name,location_name, sex_name, age_strat, year) %>%
  summarise(Number = sum(pred_val), .groups = 'drop') 


Norpred_result_age_strat$age_strat <- factor(Norpred_result_age_strat$age_strat, 
                                        levels = c("15-29 years", "30-44 years", "45-59 years", 
                                                   "60-75 years", "75+ years"))


fwrite(Norpred_result_age_strat,file.path(Figure_folder,"Figure6_Norpred_result_age_strat.csv"),sep = ",")
TableS15 = left_join(Norpred_result_age_strat,location_id_location_name,by = "location_name") %>% select(c("location_id", "location_name","age_strat", "year", "Number"))

write.xlsx(TableS15,file = file.path(Table_folder,paste0(TableS15_name,"_norpred.xlsx")),sheetName = TableS15_name,overwrite = TRUE)


# Norpred_result_ASR ---------------------------------------------------------
Norpred_result_ASR = read.xlsx(xlsxFile = file.path(Table_folder,"norpred_result.xlsx"),sheet = "ASR_Number") %>% 
  filter(
         location %in% location_name_levels,
         sex %in% c( "Both")) 

names(Norpred_result_ASR) = c("measure_name", "location_name", "cause", "sex_name", "year", "ASR", "crude_rate", "case")

Norpred_result_ASR = Norpred_result_ASR[,c( "measure_name", "location_name", "sex_name",  "year","ASR")]
fwrite(Norpred_result_ASR,file.path(Figure_folder,"Figure6_Norpred_result_ASR.csv"),sep = ",")

TableS16 = left_join(Norpred_result_ASR,location_id_location_name,by = "location_name") %>% 
  select(c( "location_id", "location_name","year", "ASR"))
write.xlsx(TableS16,file = file.path(Table_folder,paste0(TableS16_name,"_norpred.xlsx")),sheetName = TableS16_name,overwrite = TRUE)

#  ----------------------------------------------------------------------
plot_region <- function(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter, StartYear, Projection_Year, ASR_suffix,legend_position="none",left_y_lab = FALSE,right_y_lab = FALSE,interval_number = 1,stat_color = "red",left_y_lab_text = "Death cases (K)") {
  if (isFALSE(left_y_lab)) {
    left_y_lab_text = NULL
  } 
  if (right_y_lab) {
    right_y_lab = paste0(ASR_suffix," (per 100,000)")
  } else {
    right_y_lab = NULL
  }
  stacked_data <- Norpred_result_age_strat %>%
    filter(location_name == location_name_filter) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    group_by(year, age_strat) %>%
    summarize(deaths = sum(Number), .groups = 'drop') 
  
  line_data <- Norpred_result_ASR %>%
    filter(location_name == location_name_filter) %>%
    mutate(year = as.numeric(as.character(year)))
  
  max_deaths <- stacked_data %>%
    group_by(year) %>%
    summarize(total_deaths = sum(deaths)) %>%
    pull(total_deaths) %>%
    max()
  max_asr <- max(line_data$ASR)
  
  
  
  p <- ggplot() +
    geom_col(data = stacked_data, aes(x = year, y = deaths, fill = age_strat), position = "stack") +
    geom_line(data = line_data, aes(x = year, y = ASR * (max_deaths / max_asr)), color = "black", linewidth = 0.5) +
    geom_point(data = line_data, aes(x = year, y = ASR * (max_deaths / max_asr)), color = "black", size = 0.5) +
     geom_vline(xintercept = 2021.5, color = "red", linetype = "dashed", size = 0.5) + 
    labs(title = location_name_filter,
         x = NULL,
         fill = "Age groups") +
    scale_x_continuous(breaks = seq(StartYear, Projection_Year, by = 5), expand = c(0.008, 0.008)) +
    scale_y_continuous(name = left_y_lab_text,
                       expand = expansion(mult = c(0.01, 0.15)),
                       labels = scales::label_number(scale = 1e-3),
                       sec.axis = sec_axis(~ . * (max_asr / max_deaths), 
                                           name = right_y_lab,
                                           breaks = seq(0, ceiling(max_asr / interval_number) * interval_number, by = interval_number))) +
    theme_minimal() +
    scale_fill_manual(values = stat_color) +
    theme(legend.position = legend_position,
          legend.title = element_blank(),
          plot.title = element_text(color = "black", size = 16, hjust = 0.5, face = "bold.italic"),
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(0.2, "cm"),
          axis.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.border = element_rect(colour = "black", fill = NA, size = 1),
          axis.title.y.right = element_text(angle = 90, vjust = 0.5, color = "black"),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank())
  
  return(p)
}




Figure6_1 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[1], StartYear, Projection_Year, ASR_suffix,left_y_lab = TRUE,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab)
Figure6_2 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[2], StartYear, Projection_Year, ASR_suffix,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab,interval_number = 2)
Figure6_3 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[3], StartYear, Projection_Year, ASR_suffix,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab,interval_number = 2)
Figure6_4 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[4], StartYear, Projection_Year, ASR_suffix,right_y_lab = TRUE,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab,interval_number = 2)

Figure6_5 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[5], StartYear, Projection_Year, ASR_suffix,left_y_lab = TRUE,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab,interval_number = 2)
Figure6_6 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[6], StartYear, Projection_Year, ASR_suffix,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab,interval_number = 2)
Figure6_7 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[7], StartYear, Projection_Year, ASR_suffix,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab,interval_number = 0.5)
Figure6_8 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[8], StartYear, Projection_Year, ASR_suffix,right_y_lab = TRUE,interval_number = 1,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab)

Figure6_9 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[9], StartYear, Projection_Year, ASR_suffix,left_y_lab = TRUE,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab)
Figure6_10 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[10], StartYear, Projection_Year, ASR_suffix,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab)
Figure6_11 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[11], StartYear, Projection_Year, ASR_suffix,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab)
Figure6_12 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[12], StartYear, Projection_Year, ASR_suffix,right_y_lab = TRUE,interval_number = 2,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab)

Figure6_13 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[13], StartYear, Projection_Year, ASR_suffix,left_y_lab = TRUE,interval_number = 2,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab)
Figure6_14 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[14], StartYear, Projection_Year, ASR_suffix,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab,interval_number = 0.5)
Figure6_15 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[15], StartYear, Projection_Year, ASR_suffix,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab)
Figure6_16 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[16], StartYear, Projection_Year, ASR_suffix,right_y_lab = TRUE,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab,interval_number = 2)

Figure6_17 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[17], StartYear, Projection_Year, ASR_suffix,left_y_lab = TRUE,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab)
Figure6_18 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[18], StartYear, Projection_Year, ASR_suffix,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab,interval_number = 0.5)
Figure6_19 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[19], StartYear, Projection_Year, ASR_suffix,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab)
Figure6_20 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[20], StartYear, Projection_Year, ASR_suffix,right_y_lab = TRUE,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab)

Figure6_21 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[21], StartYear, Projection_Year, ASR_suffix,left_y_lab = TRUE,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab)
Figure6_22 = plot_region(Norpred_result_age_strat, Norpred_result_ASR, location_name_filter=location_name_levels[22], StartYear, Projection_Year, ASR_suffix,right_y_lab = TRUE,stat_color = age_strat_color,left_y_lab_text = Figure6_ylab)



legend_plot <- plot_region(Norpred_result_age_strat, Norpred_result_ASR, 
                           location_name_filter=location_name_levels[1], 
                           StartYear, Projection_Year, ASR_suffix, 
                           legend_position="right",stat_color = age_strat_color,left_y_lab_text = Figure6_ylab) +
  theme(legend.box.margin = margin(0, 0, 0, 12),
        legend.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"))

shared_legend <- get_legend(legend_plot)

legend_grob <- ggplot() + 
  theme_void() +
  annotation_custom(shared_legend)


combined_plot <- (Figure6_1 | Figure6_2 | Figure6_3 | Figure6_4) /
                 (Figure6_5 | Figure6_6 | Figure6_7 | Figure6_8) /
                 (Figure6_9 | Figure6_10 | Figure6_11 | Figure6_12) /
                 (Figure6_13 | Figure6_14 | Figure6_15 | Figure6_16) /
                 (Figure6_17 | Figure6_18 | Figure6_19 | Figure6_20) /
                 (Figure6_21 | Figure6_22 | legend_grob  | plot_spacer())


ggsave(file.path(Figure_folder,"Figure6_Norpred.pdf"), combined_plot, width = 14, height = 18)




