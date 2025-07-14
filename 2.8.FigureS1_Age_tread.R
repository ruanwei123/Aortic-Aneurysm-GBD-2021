# FigureS1 ----------------------------------------------------------------

load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")


# Read data
GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))

location_ids_filter = c(Global_region_ids,SDI_region_ids)
location_name_level = c("Global" ,SDI_region_level)

SDI_region_age_strat <- GBD_data %>% 
  filter(age_id %in% age_strat_ids,
         measure_name == measure_name_filter,
         metric_name == 'Number',
         location_id %in% location_ids_filter,
         sex_name %in% c( "Both")) %>%
  select(location_id,location_name, sex_name, age_name, year, val) %>%
  rename(Number = val)


SDI_region_age_strat$age_name <- factor(SDI_region_age_strat$age_name, 
                                        levels = rev(age_strat_name))
SDI_region_age_strat$location_name <- factor(SDI_region_age_strat$location_name, 
                                        levels = location_name_level)

plot_data <- SDI_region_age_strat 
plot_data$Number = plot_data$Number/1000


max_value <- max(plot_data$Number)

plot_data_df = plot_data %>% select(c( "location_name", "age_name", "year", "Number"))

fwrite(plot_data_df,file = file.path(Figure_folder,"FigureS1A_SDI_region_age_strat.csv"))


# FigureS1A ----------------------------------------------------------------

global_plot <- plot_data %>%
  filter(location_name == "Global") %>%
  ggplot(aes(x = year, y = Number, fill = age_name)) +
  geom_area() +
  scale_fill_manual(values = age_group_color) +
  labs(x = NULL, y = Figure1C_legend) +
  facet_wrap(~ location_name, scales = "free_y") +  # Add facet for Global
  theme_minimal() +
  scale_x_continuous(breaks = seq(StartYear, EndYear, by = 10), expand = c(0.008, 0.008)) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        # axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"))

# Create the plot for other regions
other_regions_plot <- plot_data %>%
  filter(location_name != "Global") %>%
  ggplot(aes(x = year, y = Number, fill = age_name)) +
  geom_area() +
  # facet_wrap(~location_name, scales = "free_y") +
  facet_wrap(~location_name, nrow = 1) +
  scale_fill_manual(values = age_group_color) +
  labs(x = NULL, y = NULL, fill = "Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        legend.title = element_blank(),
        panel.spacing.x = unit(0.01, "lines"),  
        plot.margin = margin(1, 1, 1, 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"))


dummy_data <- data.frame(
  x = 1,
  y = 1,
  age_name = unique(plot_data$age_name)
)

legend_plot <- ggplot(dummy_data, aes(x = x, y = y, fill = age_name)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = age_group_color) +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.8, "cm"))

legend_grob <- get_legend(legend_plot)

FigureS1A <- global_plot + other_regions_plot + legend_grob +  plot_layout(widths = c(2, 10, 1))
ggsave2(filename = file.path(Figure_folder,"FigureS1A_SDI_region_age_strat.pdf"),plot = FigureS1A,width = 12,height = 7)



# FigureS1B ----------------------------------------------------------------
GBD_region_ids = c(Global_region_ids,SDI_region_ids,GBD_region_ids)
location_region_level = c("Global" ,SDI_region_level,GBD_region_level)

GBD_region_age_strat_df <- GBD_data %>% 
  filter(age_id %in% age_strat_ids,
         measure_name == measure_name_filter,
         metric_name == 'Number',
         location_id %in% GBD_region_ids,
         sex_name %in% c( "Both")) %>%
  select(location_id,location_name, sex_name, age_name, year, val) %>%
  rename(Number = val)


# Create a mapping of original age groups to new age groups
age_mapping <- list(
  "15-29 years" = c("15-19 years", "20-24 years", "25-29 years"),
  "30-44 years" = c("30-34 years", "35-39 years", "40-44 years"),
  "45-59 years" = c("45-49 years", "50-54 years", "55-59 years"),
  "60-75 years" = c("60-64 years", "65-69 years", "70-74 years"),
  "75+ years" = c("75-79 years", "80-84 years", "85-89 years","90-94 years","95+ years")
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
GBD_region_age_strat <- GBD_region_age_strat_df %>%
  mutate(age_strat = sapply(age_name, map_age_groups)) %>%
  group_by(location_name, sex_name, age_strat, year) %>%
  summarise(Number = sum(Number), .groups = 'drop') 

age_strat_levels = rev(c("15-29 years", "30-44 years", "45-59 years","60-75 years", "75+ years"))

plot_data_age_strat <- GBD_region_age_strat %>%
  filter(year %in% c(StartYear, EndYear))  %>%
  group_by(location_name, year) %>%
  mutate(total_Number = sum(Number)) %>% 
  ungroup() %>%
  mutate(proportion = Number / total_Number * 100) %>% 
  mutate(location_name = factor(location_name,levels =rev(location_region_level)),
         age_strat = factor(age_strat,levels =rev(age_strat_levels)))

plot_data_age_strat_df = plot_data_age_strat %>% arrange(location_name)
fwrite(plot_data_age_strat_df,file = file.path(Figure_folder,"FigureS1B_SDI_region_age_strat.csv"))

TableS6 = plot_data_age_strat_df %>% arrange(rev(location_name),age_strat,year) %>% select(c("location_name",  "age_strat", "year", "Number", "proportion"))
write.xlsx(TableS6,file = file.path(Table_folder,paste0(TableS6_name,".xlsx")),sheetName = TableS6_name,overwrite = TRUE)

FigureS1B = ggplot(plot_data_age_strat, aes(x = location_name, y = proportion, fill = age_strat)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = . %>% filter(age_strat %in% age_strat_levels[1:3]),
            aes(label = sprintf("%.1f%%", proportion)), 
            position = position_stack(vjust = 0.5), 
            size = 2.5, color = "white") +
  facet_wrap(~year, ncol = 2) +
  scale_fill_manual(values = age_strat_color, guide = guide_legend(reverse = TRUE))+
  scale_x_continuous( expand = c(0.008, 0.008)) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.01)))+
  # scale_fill_viridis_d() +
  coord_flip() +
  labs(x = "", y = FigureS1_title_name, fill = "Age") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        strip.text = element_text( size = 16), 
        axis.text = element_text(size = 10), 
        axis.title.x = element_text(size = 12, margin = margin(t = 8, b = -10)), 
                  axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(0.1, "cm"),
        # strip.placement = "outside",
        # strip.background = element_blank(),
        # panel.grid.major.y = element_blank(),
        # axis.ticks.y = element_blank(),
        panel.spacing.x = unit(0, "lines"),  
        plot.margin = margin(0, 0, 0, 0),
        axis.text.y = element_text(hjust = 0), # Move the labels to the right side
        legend.box = "horizontal"
        )+
  scale_x_discrete(position = "top") # Flip the x-axis labels to the top

# Combine the plots with the legend

ggsave2(filename = file.path(Figure_folder,"FigureS1B_SDI_region_age_strat.pdf"),plot = FigureS1B,width = 12,height = 7)

