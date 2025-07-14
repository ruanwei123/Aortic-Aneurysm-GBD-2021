# Figure5B ----------------------------------------------------------------

load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")

GBD_riskdata = fread(file.path(cause_folder,paste0(cause_name,"_Riskdata.csv")))

location_ids_filter = c(Global_region_ids,SDI_region_ids,GBD_region_ids)
location_name_level = c("Global" ,SDI_region_level,GBD_region_level)

GBD_risk_strat_df = read.xlsx(xlsxFile = "0.demo_data/GBD_risk_strat.xlsx") %>% filter(REI_level==2)
GBD_risk_strat_ids = GBD_risk_strat_df$REI_ID
GBD_risk_df <- GBD_riskdata %>% 
  filter(age_name =="Age-standardized",
         measure_name == measure_name_filter,
         metric_name == 'Rate',
         rei_id %in% GBD_risk_strat_ids,
         location_id %in% location_ids_filter,
         sex_name %in% c( "Both")
         ) %>%
  select(c( "location_id", "location_name", "rei_id",  "rei_name", "year", "val"))


riskrank_EndYear_Global_df <- GBD_risk_df %>%filter(location_name=="Global",year==EndYear) %>% 
  group_by(location_name) %>%
  arrange(desc(val), .by_group = TRUE) %>%
  slice_head(n = 8) %>%
  mutate(rank = row_number()) 

riskrank_Global_level = unique(riskrank_EndYear_Global_df$rei_name)
GBD_risk_df_filter = GBD_risk_df %>% mutate(
  rei_name = case_when(
    rei_name %in% riskrank_Global_level ~ rei_name,
    TRUE ~ "other risk"
  )
)%>%
  group_by(location_name, year, rei_name) %>%
  summarise(val = sum(val, na.rm = TRUE), .groups = "drop")

riskrank_risk_level = c(riskrank_Global_level,"other risk")

plot_GBD_risk_data <- GBD_risk_df_filter %>%
  group_by(location_name, year) %>%
  mutate(total_Number = sum(val)) %>% 
  ungroup() %>%
  mutate(proportion = val / total_Number * 100) %>% 
  mutate(location_name = factor(location_name,levels =rev(location_name_level)),
         rei_name = factor(rei_name,levels =rev(riskrank_risk_level)))

fwrite(plot_GBD_risk_data,file = file.path(Figure_folder,"Figure5B_risk_factor.csv"))

TableS14 = left_join(plot_GBD_risk_data,location_id_location_name,by = "location_name") %>% 
  select(c( "location_id","location_name", "year", "rei_name", "val", "total_Number", 
"proportion")) %>% arrange(location_id,year)
write.xlsx(TableS14,file = file.path(Table_folder,paste0(TableS14_name,".xlsx")),sheetName = TableS14_name,overwrite = TRUE)

# Figure5B ----------------------------------------------------------------
Figure5B = ggplot(plot_GBD_risk_data, aes(x = location_name, y = proportion, fill = rei_name)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = . %>% filter(rei_name %in% riskrank_risk_level[1:4]),
            aes(label = sprintf("%.1f%%", proportion)), 
            position = position_stack(vjust = 0.5), 
            size = 2.5, color = "white") +
  facet_wrap(~year, ncol = 2) +
  scale_fill_manual(values = risk_colors, guide = guide_legend(reverse = TRUE, nrow = 2,byrow = TRUE)) +  #
  scale_x_continuous( expand = c(0.008, 0.008)) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.01))) +
  coord_flip() +
  labs(x = "", y = Figure5B_title_name, fill = "rei_name") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        strip.text = element_text( size = 16), 
        axis.text = element_text(size = 10), #
        axis.title.x = element_text(size = 12, margin = margin(t = 8, b = -10)),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        panel.spacing.x = unit(0, "lines"),  
        plot.margin = margin(0, 0, 0, 0),
        axis.text.y = element_text(hjust = 0), # Move the labels to the right side
        legend.box = "horizontal"
        ) +
  scale_x_discrete(position = "top") # Flip the x-axis labels to the top



ggsave2(filename = file.path(Figure_folder,"Figure5B_risk_factor.pdf"),plot = Figure5B,width = 12,height = 7)

