source('0.source_apc.R')
source('0.function_year5.R')
load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/APC_df_names.Rdata")
#  ------------------------------------------------------------------
sex_process = "Both"

location_ids_filter = c(Global_region_ids,nation_region_ids)

GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))

location_name_df = location_df %>% 
  filter(location_id %in% location_ids_filter) %>% 
  select(location_id, location_name)

location_ids =  location_name_df$location_id

GBD_data_df <- GBD_data %>% 
  filter(age_id %in% age_APCmodel_ids,
         metric_name == 'Number',
         sex_name %in% sex_process,
         measure_name %in% measure_name_filter,
         year >= APC_StartYear,
         location_id %in% location_ids) %>% 
  select(c( "measure_name", "location_id", "location_name",  "sex_name", "age_id", "age_name",  "year", "val"))


#  ------------------------------------------------------------------

population_df = fread(file.path(Population_folder,"population_data_GBD_location.csv"))  %>% 
  filter(age_id %in% age_APCmodel_ids,
         metric_name == 'Number',
         sex_name %in% sex_process,
         year >= APC_StartYear,
         location_id %in% location_ids) %>% 
  select(c("location_id",   "sex_name", "age_id", "age_name",  "year", "val"))

#  --------------------------------------------------------------------
measure_names = GBD_data_df$measure_name %>% unique()
sex_names = GBD_data_df$sex_name %>% unique()
location_ids = GBD_data_df$location_id %>% unique()

parater_list <- expand.grid(measure_name = measure_names, sex_name = sex_names,location_id = location_ids)
parater_list = parater_list %>% left_join(location_name_df,by = "location_id") %>% select(c("measure_name","sex_name","location_id",  "location_name"))

for (index in 1:nrow(parater_list)) {

  measure_name_filter = parater_list$measure_name[index]
  sex_filter = parater_list$sex_name[index]
  location_id_filter = parater_list$location_id[index]
  location_name_filter = parater_list$location_name[index]
  
  ##GBD_data_filter
  GBD_data_filter<- GBD_data_df %>% filter(measure_name==measure_name_filter,sex_name ==sex_filter,location_id==location_id_filter) %>% select(age_name,year,val)
  GBD_data_n <- pivot_wider(data = GBD_data_filter,id_cols = age_name ,names_from = year, values_from =  val,  names_sort = TRUE)  
  
  GBD_data_g <- function_year5(table_name = GBD_data_n, start_year = APC_StartYear, end_year = EndYear, current_year = EndYear)
  rownames(GBD_data_g) <- GBD_data_n$age_name
  GBD_data_g = GBD_data_g %>% rownames_to_column(var = "age")
  
  #population_filter
  population_filter = population_df%>%filter(sex_name ==sex_filter,location_id==location_id_filter) %>% select(age_name,year,val)
  #
  # population_n <- dcast(data = population_filter,  age_name ~ year, value.var = "val") 
  population_n <- pivot_wider(data = population_filter,id_cols = age_name ,names_from = year, values_from =  val,  names_sort = TRUE) 
  population_g <- function_year5(table_name = population_n, start_year = APC_StartYear, end_year = EndYear, current_year = EndYear)
  rownames(population_g) <- population_n$age_name
  names(population_g) = paste0(names(population_g) ,"p")
  population_g = population_g %>% rownames_to_column(var = "age")
  
  common_age = intersect(GBD_data_g$age,population_g$age)
  GBD_data_g = GBD_data_g[GBD_data_g$age %in% common_age,]
  population_g = population_g[population_g$age %in% common_age,]
  GBD_data_population = left_join(GBD_data_g,population_g,by = "age")
  
  new_order <- c("age")
  for (i in 2:length(GBD_data_g)) {
    new_order <- c(new_order, names(GBD_data_g)[i], names(population_g)[i])
  }
  GBD_data_population = GBD_data_population[,new_order]

  GBD_data_population$age<-gsub(" years","",GBD_data_population$age)
  GBD_data_population$age<- factor(GBD_data_population$age, levels = unique(GBD_data_population$age))

  R <- prepare_rates(GBD_data_population[,-1],
                     StartYear=APC_StartYear,StartAge=StartAge,Interval=5,
                     fullname='',description='') 
  M <- apc2fit(R)


  temp = {}

  for (name in APC_df_all_names) {
    if (name %in% APC_df_rownames) {
      df <- as.data.frame(M[[name]]) %>% 
        rownames_to_column(var = name)
    } else {
      df <- as.data.frame(M[[name]])
    }
  
    df$location_id <- location_id_filter
    df$cause <- cause_name
    df$measure <- measure_name_filter
    df$sex <- sex_filter
    temp[[name]] <- df
    
  } 
    if (index == 1 ) {
      APC_result = temp
    } else {
    for (name in APC_df_all_names) {
      APC_result[[name]] <- rbind(APC_result[[name]], temp[[name]])
    }

  }


}

write.xlsx(APC_result,file = file.path(Table_folder,paste0(cause_name,"_GBD_APC_nation_result.xlsx")))


# Figure 2D ---------------------------------------------------------------

library(sf)
load("0.demo_data/GBD_map.Rdata")
load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/map_color.Rdata")
load("0.demo_data/Quantile.Rdata")
variable = "val_Discrete"
guide_name = paste0("Net Drift of ",ASR_suffix," (% per year)")

GBD_data_nation =  read.xlsx(file.path(Table_folder,paste0(cause_name,"_GBD_APC_nation_result.xlsx")),sheet = "NetDrift") %>% 
  filter(measure==measure_name_filter,sex=="Both",location_id %in% nation_region_ids) %>%
  select(c("location_id","Net.Drift.(%/year)" , "CILo", "CIHi"))

names(GBD_data_nation) = c("location_id","Net_Drift", "Net_Drift_lower" ,"Net_Drift_upper" )

fwrite(GBD_data_nation,file.path(Figure_folder,"Figure2D_world_map_netdrift.csv"))
#  --------------------------------------------------------------------

GBD_data_nation$val_Discrete = Quantile_neg_pos(GBD_data_nation$Net_Drift,n = 5,precision = 2)

GBD_data_location_id_map = read.xlsx("0.demo_data/GBD_data_location_id_map.xlsx") %>% filter(nation_region=="nation") %>% select(location_id,GBD_location_map)

GBD_data_map = left_join(GBD_data_nation,GBD_data_location_id_map,by = "location_id")


GBD_data_map$GBDvar <- GBD_data_map[[variable]]


worldData <- map_data("world")
GBD_nation_small_map_df = read.xlsx("0.demo_data/GBD_location_small_map.xlsx") %>% select(c("location_id", "region"))
small_map_data <- left_join(GBD_data_map,GBD_nation_small_map_df,by = "location_id")

small_map_data_df = small_map_data %>% select(c("region", "Net_Drift", "Net_Drift_lower", "Net_Drift_upper")) %>% arrange(Net_Drift)


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
  scale_fill_manual(values = netdrift_map_color)+
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
  scale_fill_manual(values = netdrift_map_color)


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


#  ----------------------------------------------------------------------

plot <- main_map_plot / 
  (p2 + p3 + p4 + p5 + A + 
     plot_layout(ncol = 5, widths = c(1, 1, 1, 1, 1))) +
  plot_layout(ncol = 1, heights = c(7, 3)) +  
  plot_annotation(theme = theme(plot.margin = margin(-20, 0, 0, 0)))

ggsave(filename = file.path(Figure_folder,"Figure2D_world_map_netdrift.pdf"), plot, width = 9, height = 6)




# TableS5 -----------------------------------------------------------------
load("0.demo_data/age_location_level.Rdata")
GBD_data_nation_ASR = fread(file = file.path(Figure_folder,paste0("Figure2A_world_map_",ASR_suffix,".csv")))
GBD_data_nation_ASR_StartYear = GBD_data_nation_ASR %>% filter(year==StartYear) %>% select(-year) 
names(GBD_data_nation_ASR_StartYear) = c("location_id", "location_name","ASR_StartYear","ASR_upper_StartYear", "ASR_lower_StartYear" )

GBD_data_nation_ASR_EndYear = GBD_data_nation_ASR %>% filter(year==EndYear) %>% select(-c("year","location_name"))
names(GBD_data_nation_ASR_EndYear) = c("location_id","ASR_EndYear","ASR_upper_EndYear", "ASR_lower_EndYear")
GBD_data_nation_EAPC = fread(file = file.path(Figure_folder,"Figure2C_world_map_EAPC.csv") ) %>% select(c("location_id", "location_name", "EAPC", "LCI", "UCI")) %>% rename(EAPC_lower = LCI,EAPC_upper = UCI)%>% select(-c("location_name"))
GBD_data_nation_netdrift = fread(file = file.path(Figure_folder,"Figure2D_world_map_netdrift.csv"))


TableS5 = GBD_data_nation_ASR_StartYear %>%
  left_join(GBD_data_nation_ASR_EndYear,by = "location_id") %>% 
  left_join(GBD_data_nation_EAPC,by = "location_id") %>% 
  left_join(GBD_data_nation_netdrift,by = "location_id") 
precise_col = setdiff(names(TableS5),c("location_id", "location_name"))

TableS5 = TableS5 %>% 
  Precision(column_names = precise_col,precision = 2) %>% 
  mutate(ASR_StartYear_UI = sprintf("%s (%s,%s)", ASR_StartYear, ASR_lower_StartYear,ASR_upper_StartYear),
       ASR_EndYear_UI = sprintf("%s (%s,%s)",ASR_EndYear, ASR_lower_EndYear,ASR_upper_EndYear),
       EAPC_CI = sprintf("%s (%s,%s)",EAPC,EAPC_lower, EAPC_upper),
       Netdrift_CI = sprintf("%s (%s,%s)",Net_Drift,Net_Drift_lower, Net_Drift_upper)) %>% 
  select(c("location_id", "location_name","ASR_StartYear_UI","ASR_EndYear_UI","EAPC_CI","Netdrift_CI")) %>% arrange(location_id)
write.xlsx(TableS5,file = file.path(Table_folder,paste0(TableS5_name,".xlsx")),sheetName = TableS5_name,overwrite = TRUE)

