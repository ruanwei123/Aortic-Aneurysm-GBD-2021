# Figure5A ----------------------------------------------------------------

load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")

#  ------------------------------------------------------------------
GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))

# Filter and select location names for SDI regions
location_df = location_df %>%   select(nation_region,location_id, location_name) 

location_ids_filter = c(Global_region_ids,SDI_region_ids,GBD_region_ids)

location_name_level = c("Global",SDI_region_level,GBD_region_level)

GBD_data_df <- GBD_data %>% 
  filter(age_id %in% age_strat_ids,
         year %in% c(StartYear,EndYear),
         location_id %in% location_ids_filter) %>% select(measure_name, location_id, location_name,  sex_name, age_id, age_name,metric_name, year, val)

#  ------------------------------------------------------------------

population_df = fread(file.path(Population_folder,"population_data_GBD_location.csv")) %>% 
  filter(age_id %in% age_strat_ids,
         year %in% c(StartYear,EndYear),
         location_id %in% location_ids_filter)%>% select(location_id,sex_name, age_id, age_name,year, val)


######## decomposition calculation######
decomposition_name <- c("location_id",'sex_name','overll_difference','a_effect','p_effect','r_effect','a_percent',
                        'p_percent','r_percent')
decomposition_data <- as.data.frame(matrix(nrow=0,ncol=length(decomposition_name))) 
names(decomposition_data) <- decomposition_name




#  --------------------------------------------------------------------
location_id_list = GBD_data_df$location_id %>% unique()
sex_name_list = GBD_data_df$sex_name %>% unique()

parater_list <- expand.grid(location_id = location_id_list, sex_name = sex_name_list)

start.time = Sys.time()
for (index in 1:nrow(parater_list)) {
  sex_filter = parater_list$sex_name[index]
  location_id_filter = parater_list$location_id[index]
  
  GBD_population_StartYear <- population_df %>% 
  filter(sex_name == sex_filter, year == StartYear,location_id == location_id_filter ) 
  GBD_StartYear <- sum(GBD_population_StartYear$val) ## 
  GBD_population_StartYear$percent <- GBD_population_StartYear$val/GBD_StartYear 
  
  GBD_population_EndYear <- population_df %>% 
  filter(sex_name == sex_filter,year == EndYear,location_id == location_id_filter ) 
  GBD_EndYear <- sum(GBD_population_EndYear$val) ## 
  GBD_population_EndYear$percent <- GBD_population_EndYear$val/GBD_EndYear 
  
  a_StartYear <- GBD_population_StartYear$percent
  a_EndYear <- GBD_population_EndYear$percent
  p_StartYear <- GBD_StartYear
  p_EndYear <- GBD_EndYear
  
  case_StartYear <- GBD_data_df %>% filter(year == StartYear,
                                 sex_name == sex_filter,
                                 location_id == location_id_filter,
                                 metric_name == 'Rate',
                                 measure_name == measure_name_filter)
  
  case_EndYear <- GBD_data_df %>% filter(year == EndYear,
                                 sex_name == sex_filter,
                                 location_id == location_id_filter,
                                 metric_name == 'Rate',
                                 measure_name == measure_name_filter)
  
  r_StartYear <- as.numeric(case_StartYear$val)/10^8 ##
  r_EndYear <- as.numeric(case_EndYear$val)/10^8 
  
  a_effect <- round((sum(a_EndYear*p_StartYear*r_StartYear) + sum(a_EndYear*p_EndYear*r_EndYear))/3 + 
                      (sum(a_EndYear*p_StartYear*r_EndYear) + sum(a_EndYear*p_EndYear*r_StartYear))/6 -
                      (sum(a_StartYear*p_StartYear*r_StartYear) + sum(a_StartYear*p_EndYear*r_EndYear))/3 -
                      (sum(a_StartYear*p_StartYear*r_EndYear) + sum(a_StartYear*p_EndYear*r_StartYear))/6,3)
  
  p_effect <- round((sum(a_StartYear*p_EndYear*r_StartYear) + sum(a_EndYear*p_EndYear*r_EndYear))/3 + 
                      (sum(a_StartYear*p_EndYear*r_EndYear) + sum(a_EndYear*p_EndYear*r_StartYear))/6 -
                      (sum(a_StartYear*p_StartYear*r_StartYear) + sum(a_EndYear*p_StartYear*r_EndYear))/3 -
                      (sum(a_StartYear*p_StartYear*r_EndYear) + sum(a_EndYear*p_StartYear*r_StartYear))/6,3)
  
  r_effect <- round((sum(a_StartYear*p_StartYear*r_EndYear) + sum(a_EndYear*p_EndYear*r_EndYear))/3 + 
                      (sum(a_StartYear*p_EndYear*r_EndYear) + sum(a_EndYear*p_StartYear*r_EndYear))/6 -
                      (sum(a_StartYear*p_StartYear*r_StartYear) + sum(a_EndYear*p_EndYear*r_StartYear))/3 -
                      (sum(a_StartYear*p_EndYear*r_StartYear) + sum(a_EndYear*p_StartYear*r_StartYear))/6,3)
  
  overll_differ <- round(a_effect + p_effect + r_effect,2)
  
  a_percent <- round(a_effect/overll_differ*100,2)
  p_percent <- round(p_effect/overll_differ*100,2)
  r_percent <- round(r_effect/overll_differ*100,2)
  
  temp <- data.frame(location_id_filters = location_id_filter,sex_filters = sex_filter,overll_differs = overll_differ,a_effects = a_effect,p_effects = p_effect,r_effects = r_effect,a_percents = a_percent,p_percents = p_percent,r_percents = r_percent)
  names(temp) <- decomposition_name
  decomposition_data <- rbind(decomposition_data,temp)
  
}



# startyear  ------------------------------------------------------------

num_StartYear <- GBD_data %>% filter(age_name == 'All ages',
                               location_id %in% location_ids_filter,
                               metric_name == 'Number',
                               year == StartYear,
                               measure_name == measure_name_filter) %>% 
  select(location_id,sex_name, val) %>% 
  rename(val_StartYear = val)

# EndYear---------------------------------------------------------------

num_EndYear <- GBD_data %>% filter(age_name == 'All ages',
                               location_id %in% location_ids_filter,
                               metric_name == 'Number',
                               year == EndYear,
                               measure_name == measure_name_filter) %>% 
  select(location_id,sex_name, val) %>% 
  rename(val_EndYear = val)



#  --------------------------------------------------------------------

decomposition_data <- left_join(decomposition_data,num_StartYear, by = c("location_id","sex_name")) %>% 
  left_join(num_EndYear, by = c("location_id","sex_name"))


decomposition_data$diff1 <- decomposition_data$val_EndYear - decomposition_data$val_StartYear

decomposition_data <- decomposition_data %>%
  mutate(across(3:ncol(decomposition_data), as.numeric))

decomposition_data = left_join(decomposition_data,location_df,by = "location_id")

decomposition_data = decomposition_data %>% filter(sex_name == "Both")

decomposition_data$location_name = factor(decomposition_data$location_name,levels = location_name_level)

decomposition_plot <- decomposition_data[,c("location_name","sex_name", "overll_difference", "a_effect", 
"p_effect", "r_effect")]

names(decomposition_plot) = c("location_name","sex_name", "overll_difference","Aging","Population","Epidemiological change")

decomposition_plot_df <- decomposition_plot%>%
  pivot_longer(4:6,
               names_to = "varname",
               values_to = "value") %>%
  mutate(value=as.numeric(value))


fwrite(decomposition_plot_df,file = file.path(Figure_folder,"Figure5A_decomposition.csv"))
TableS13 = left_join(decomposition_plot_df,location_id_location_name,by = "location_name") %>%
  arrange(location_id) %>% select(c("location_id","location_name", "overll_difference", "varname", "value"))
write.xlsx(TableS13,file = file.path(Table_folder,paste0(TableS13_name,".xlsx")),sheetName = TableS13_name,overwrite = TRUE)

Figure5A = ggplot(decomposition_plot_df, aes(x = location_name, y = value, fill = varname)) +
  geom_col(position = "stack") +
    geom_point(
    data = decomposition_data,
    aes(y = overll_difference),
    fill = 'black', color = 'black', size = 1.2
  ) +
  # facet_wrap(~ sex_name, scales = "fixed", nrow = 1) +
  labs(
    x = NULL,
    y = Figure5A_ylba_text,
    fill = NULL
  ) +
  # coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = decomposition_colors) +
  theme(legend.position = "bottom",
  axis.ticks = element_line(color = "black"),
  axis.ticks.length = unit(0.2, "cm"),
  strip.text = element_text(size = 18, face = "bold"),
  axis.line = element_line(color = "black"),
  axis.title = element_text(size = 14, face = "bold"),
  axis.text = element_text(size = 10),
  axis.text.x = element_text(angle = 45, hjust = 1), 
  axis.text.y = element_text(size = 12),
  panel.grid.major = element_line(color = "gray90", size = 0.5),
  panel.grid.minor = element_line(color = "gray90", size = 0.5),
  legend.title = element_blank(),
  legend.text = element_text(size = 12),
  panel.border = element_rect(colour = "black", fill = NA, size = 1),
  panel.spacing = unit(0.5, "lines")
)
ggsave2(filename = file.path(Figure_folder,"Figure5A_decomposition.pdf"),plot = Figure5A,width = 12,height = 7)
