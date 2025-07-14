## frontier analysis################
load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")

sex_name_filter ="Both"
age_name_filter = "Age-standardized"
metric_name_filter = "Rate"
GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))

location_id_filter = nation_region_ids

frontier_data<- GBD_data %>% filter(measure_name==measure_name_filter,sex_name ==sex_name_filter, age_name==age_name_filter, metric_name== metric_name_filter,location_id %in% location_id_filter) %>% select(location_id,location_name,year,val)

GBD_SDI <- fread('0.demo_data/GBD2021SDI_1950_2021.csv')
frontier_SDI <- left_join(frontier_data,GBD_SDI,by=c('location_id','year')) 



library(foreach)
library(doParallel)
num_cores <- 12
cl <- makeCluster(num_cores)
registerDoParallel(cl)

boostrap_num <- 100  # Number of bootstrap iterations

# Main parallel loop
results <- foreach(interation_number = 1:boostrap_num, .combine = rbind,) %dopar% {
  # Sample with replacement
  boot_sample <- frontier_SDI[sample(1:nrow(frontier_SDI), nrow(frontier_SDI), replace = TRUE),] %>% 
    arrange(SDI, desc(val))
  
  # Calculate super efficiency
  boot_sample <- boot_sample %>%
    mutate(
      cummin_val = cummin(val),
      next_duplicate = lead(location_name) == location_name & lead(year) == year,
      is_duplicate = (location_name == lag(location_name) & year == lag(year)),
      prev_cummin = lag(cummin_val, default = Inf)
    ) %>%
    mutate(
      super = case_when(
        row_number() == 1 ~ 0,
        next_duplicate ~ 0,
        is_duplicate & !next_duplicate & val == cummin_val ~ 1,
        val > cummin_val ~ 1,
        TRUE ~ 0
      )
    )
  
  # Filter and calculate frontier
  boot_sample_exclude <- boot_sample[boot_sample$super == 0,]
  boot_sample_exclude$frontier <- cummin(boot_sample_exclude$val)
  
  # Return results
  boot_sample_exclude
}


#  --------------------------------------------------------------------

# Summarize results
boostrap_DEA <- results %>%
  group_by(location_id, year, val, SDI) %>%
  summarize(frontier = mean(frontier),.groups = "drop")

# Print completion message
stopCluster(cl)


boostrap_DEA <- boostrap_DEA %>% 
  mutate(eff_diff = val - frontier)

names(boostrap_DEA) = c("location_id", "year", "val", "SDI", "frontier", "eff_diff")
boostrap_DEA = left_join(boostrap_DEA, location_id_GBD_location, by = "location_id") %>% rename(location_name=GBD_location_name)

boostrap_DEA_StartYear <- boostrap_DEA %>% 
  filter(year == StartYear)

boostrap_DEA_EndYear <- boostrap_DEA %>% 
  filter(year == EndYear) 

boostrap_DEA_EndYear$trend <- ifelse(boostrap_DEA_EndYear$val > boostrap_DEA_StartYear$val, "Increase",
                                  "Decrease")


TableS12 = boostrap_DEA %>% select(c("location_id", "location_name", "year", "val", "SDI", "frontier", "eff_diff"))
fwrite(TableS12,file = file.path(Figure_folder,"Figure4_fornitier.csv"))

write.xlsx(TableS12,file = file.path(Table_folder,paste0(TableS12_name,".xlsx")),sheetName = TableS12_name,overwrite = TRUE)

# Figure4A ----------------------------------------------------------------

Figure4A <- ggplot(boostrap_DEA, aes(SDI,val)) + geom_point(aes(color = year),size=1.8)+
  scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) +
  scale_y_continuous(limits = c(-0.5, 15),
                   breaks = seq(0, 15, by = 5),
                   expand = c(0, 0)) + ## 
  # scale_color_gradient(low='#62b1d0',high='#052C54') + ## 
  scale_color_gradientn(colors = frontier_continue_colors)+ ## 
  stat_smooth(data=boostrap_DEA, aes(SDI,frontier),colour='black',formula=y ~ poly(x, 1),
              method='loess',se=F,span=0.2,fullrange=T) + 
  theme_minimal() +
  labs(
    x = "Sociodemographic Index",
    y = ASR_suffix,
    color = "Year"  
  ) +
  theme(legend.position = c(0.9,0.85),
    axis.ticks = element_line(color = "black"), # 
    axis.ticks.length = unit(0.2, "cm"), # 
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.spacing = unit(0.5, "lines")
  )


# Figure 4B ---------------------------------------------------------------

SDI_strat_df = read.xlsx("0.demo_data/SDI_strat.xlsx")
High_SDI = SDI_strat_df$lower_bound[SDI_strat_df$location_name=="High SDI"] %>% as.numeric()
Low_SDI = SDI_strat_df$upper_bound[SDI_strat_df$location_name=="Low SDI"]%>% as.numeric()


blue <- subset(boostrap_DEA_EndYear,SDI< Low_SDI)[order(subset(boostrap_DEA_EndYear,SDI<Low_SDI)$eff_diff),][1:5,]   
red <- subset(boostrap_DEA_EndYear,SDI>High_SDI)[order(subset(boostrap_DEA_EndYear,SDI>High_SDI)$eff_diff,decreasing = T),][1:5,] 
black <- boostrap_DEA_EndYear[order(boostrap_DEA_EndYear$eff_diff,decreasing = T),][1:5,] 
black = setdiff(black,red)

Figure4B <- ggplot(boostrap_DEA_EndYear, aes(SDI,val)) + 
  geom_point(aes(color = trend),size=2.5)+
  scale_x_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.0),limits = c(0,1)) + 
  scale_y_continuous(limits = c(-0.5, 15),
                   breaks = seq(0, 15, by = 5),
                   expand = c(0, 0)) + 
  stat_smooth(data=boostrap_DEA, aes(SDI,frontier),colour='black',formula=y ~ poly(x, 1),
              stat = "smooth",method='loess',se=F,span=0.2,fullrange=T)  + #
  geom_text_repel(data=black,colour='black',aes(SDI,val, label = location_name),size=4,fontface= 'bold',max.overlaps = 200) + 
  geom_text_repel(data=red,colour=frontier_colors[4],aes(SDI,val, label = location_name),size=4,fontface= 'bold',max.overlaps = 200) + 
  geom_text_repel(data=blue,colour=frontier_colors[3],aes(SDI,val, label = location_name),size=4,fontface= 'bold',max.overlaps = 200)  + 
  theme_minimal() +
  scale_color_manual(values = frontier_colors[1:2])+
  labs(
    x = "Sociodemographic Index",
    y = ASR_suffix,
    color = "Trend"  
  ) +
  theme(legend.position = c(0.9,0.85),
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(0.2, "cm"), 
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.spacing = unit(0.5, "lines")
  )



#  --------------------------------------------------------------------

Figure4 <- (Figure4A) / (Figure4B)  +
  plot_layout(ncol = 1, heights = c( 1, 1)) 

ggsave2(filename = file.path(Figure_folder,"Figure4_fornitier.pdf"),plot = Figure4,width = 10,height = 14)
