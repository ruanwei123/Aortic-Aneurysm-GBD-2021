source('0.source_apc.R')
source('0.function_year5.R')
load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/APC_df_names.Rdata")
#  ------------------------------------------------------------------
GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))
location_ids_filter = c(Global_region_ids,SDI_region_ids,GBD_region_ids)

location_name_df = location_df %>% 
  filter(location_id %in% location_ids_filter) %>% 
  select(location_id, location_name)

location_ids =  location_name_df$location_id

GBD_data_df <- GBD_data %>% 
  filter(age_id %in% age_APCmodel_ids,
         metric_name == 'Number',
         year >= APC_StartYear,
         location_id %in% location_ids) %>% 
  select(c( "measure_name", "location_id", "location_name",  "sex_name", "age_id", "age_name",  "year", "val"))


# ------------------------------------------------------------------

population_df = fread(file.path(Population_folder,"population_data_GBD_location.csv"))  %>% 
  filter(age_id %in% age_APCmodel_ids,
         metric_name == 'Number',
         year >= APC_StartYear,
         location_id %in% location_ids) %>% 
  select(c("location_id",   "sex_name", "age_id", "age_name",  "year", "val"))


# --------------------------------------------------------------------
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

write.xlsx(APC_result,file = file.path(Table_folder,paste0(cause_name,"_GBD_APC_result.xlsx")))




# 1.2.table1 ------------------------------------------------------------

load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/APC_df_names.Rdata")

GBD_data = fread(file.path(cause_folder,paste0(cause_name,"_GBDdata.csv")))
location_ids_filter = c(Global_region_ids,GBD_region_ids,SDI_region_ids)

location_name_df = location_df %>% filter(location_id %in% location_ids_filter) %>% 
  select(location_id, location_name,nation_region)
location_name_level = c("Global" ,SDI_region_level,GBD_region_level)


data_Mortality_EndYear = GBD_data %>% filter(age_name=='All ages',year ==EndYear,measure_name== measure_name_filter,metric_name== 'Number',location_name %in% location_name_level,sex_name=="Both") %>% 
  select(location_name,sex_name,val, upper, lower)


data_Mortality_EndYear_UI <- data_Mortality_EndYear %>%
  mutate(across(c(val, lower, upper), ~ round(.x/1000, 2))) %>%
  Precision(c("val", "lower", "upper"), 2, 2) %>%
  mutate(UI = sprintf("%s (%s,%s)", val, lower, upper)) %>%
  select(location_name, sex_name, UI) %>%
  pivot_wider(names_from = sex_name, values_from = UI)

data_Mortality_EndYear_UI_xlsx = left_join(location_name_df,data_Mortality_EndYear_UI,by = "location_name") %>% mutate(location_name=factor(location_name,levels = location_name_level)) %>% arrange(location_name) %>% select(c( "location_name", "nation_region", "Both"))

names(data_Mortality_EndYear_UI_xlsx) = c("location_name", "nation_region", paste0(measure_name_suffix,"_EndYear"))



data_Mortality_EndYear_ASR = GBD_data %>% filter(age_name=='Age-standardized',year ==EndYear,measure_name==measure_name_filter,metric_name== 'Rate',location_name %in% location_name_level,sex_name=="Both") %>% 
  select(location_name,sex_name,val, upper, lower)


data_Mortality_EndYear_ASR_UI <- data_Mortality_EndYear_ASR  %>%
  mutate(across(c(val, lower, upper), ~ round(.x, 2)))%>%
  mutate(UI = sprintf("%s (%s,%s)", val, lower, upper)) %>%
  select(location_name, sex_name, UI) %>%
  pivot_wider(names_from = sex_name, values_from = UI)

data_Mortality_EndYear_ASR_UI_xlsx = left_join(location_name_df,data_Mortality_EndYear_ASR_UI,by = "location_name") %>% mutate(location_name=factor(location_name,levels = location_name_level)) %>% arrange(location_name) %>% select(c( "location_name", "nation_region", "Both"))
names(data_Mortality_EndYear_ASR_UI_xlsx) = c("location_name", "nation_region", "ASR_EndYear")




data_Mortality_StartYear = GBD_data %>% filter(age_name=='All ages',year == StartYear,measure_name== measure_name_filter,metric_name== 'Number',location_name %in% location_name_level,sex_name=="Both") %>% 
  select(location_name,sex_name,val, upper, lower)


data_Mortality_StartYear_UI <- data_Mortality_StartYear  %>%
  mutate(across(c(val, lower, upper), ~ round(.x/1000, 2)))%>%
  Precision(c("val", "lower", "upper"), 2, 2) %>%
  mutate(UI = sprintf("%s (%s,%s)", val, lower, upper)) %>%
  select(location_name, sex_name, UI) %>%
  pivot_wider(names_from = sex_name, values_from = UI)

data_Mortality_StartYear_UI_xlsx = left_join(location_name_df,data_Mortality_StartYear_UI,by = "location_name") %>% mutate(location_name=factor(location_name,levels = location_name_level)) %>% arrange(location_name) %>% select(c( "location_name", "nation_region", "Both"))
# data_Mortality_StartYear_UI_xlsx %>% names() %>% dput()
names(data_Mortality_StartYear_UI_xlsx) = c("location_name", "nation_region", paste0(measure_name_suffix,"_StartYear"))



data_Mortality_StartYear_ASR = GBD_data %>% filter(age_name=='Age-standardized',year ==StartYear,measure_name== measure_name_filter,metric_name== 'Rate',location_name %in% location_name_level,sex_name=="Both") %>% 
  select(location_name,sex_name,val, upper, lower)


data_Mortality_StartYear_ASR_UI <- data_Mortality_StartYear_ASR  %>%
  mutate(across(c(val, lower, upper), ~ round(.x, 2)))%>%
  mutate(UI = sprintf("%s (%s,%s)", val, lower, upper)) %>%
  select(location_name, sex_name, UI) %>%
  pivot_wider(names_from = sex_name, values_from = UI)

data_Mortality_StartYear_ASR_UI_xlsx = left_join(location_name_df,data_Mortality_StartYear_ASR_UI,by = "location_name") %>% mutate(location_name=factor(location_name,levels = location_name_level)) %>% arrange(location_name) %>% select(c( "location_name", "nation_region", "Both"))
names(data_Mortality_StartYear_ASR_UI_xlsx) = c("location_name", "nation_region", "ASR_StartYear")



# 生成EAPC ------------------------------------------------------------------
source("0.GBD_EAPC.R")
load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/APC_df_names.Rdata")

GBD_data_nation_ASR =  GBD_data %>% filter(age_name=='Age-standardized',measure_name== measure_name_filter,sex_name=="Both",metric_name== 'Rate',location_id %in% location_ids_filter) 
GBD_data_nation_ASR_EAPC = GBD_EAPC(data = GBD_data_nation_ASR,rei = F,EAPC_95CI = T,digits = 3,sep = ",") 
GBD_data_nation_ASR_EAPC = GBD_data_nation_ASR_EAPC[,c("location_name", "EAPC_95CI")]
names(GBD_data_nation_ASR_EAPC) = c( "location_name", "ASR_EAPC")
GBD_data_nation_ASR_EAPC_xlsx = left_join(location_name_df,GBD_data_nation_ASR_EAPC,by = "location_name") %>% mutate(location_name=factor(location_name,levels = location_name_level)) %>% arrange(location_name) %>% select(c( "location_name", "nation_region", "ASR_EAPC"))

# net drift ---------------------------------------------------------------
load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/APC_df_names.Rdata")
Net_drift_GBD = read.xlsx(file.path(Table_folder,paste0(cause_name,"_GBD_APC_result.xlsx")),sheet = "NetDrift") %>% filter(location_id %in% location_ids_filter,sex%in%c("Both"),measure%in%c(measure_name_filter)) 
names(Net_drift_GBD) = c("val", "lower", "upper", "location_id", "cause", 
"measure", "sex")
Net_drift_GBD_xlsx = left_join(Net_drift_GBD,location_df,by = "location_id") %>% select(c("location_name", "nation_region","val", "lower", "upper"))%>%
    mutate(across(c(val, lower, upper), ~ round(.x, 2)))%>%
  mutate(ASR_Net_drift = sprintf("%s (%s,%s)", val, lower, upper)) %>% select(c( "location_name", "nation_region", "ASR_Net_drift"))
# -------------------------------------------------------------------

GBD_data_table1 <- data_Mortality_StartYear_UI_xlsx %>%
  left_join(data_Mortality_StartYear_ASR_UI_xlsx, by = c("location_name", "nation_region")) %>%
  left_join(data_Mortality_EndYear_UI_xlsx, by = c("location_name", "nation_region")) %>%
  left_join(data_Mortality_EndYear_ASR_UI_xlsx, by = c("location_name", "nation_region")) %>%
  left_join(GBD_data_nation_ASR_EAPC_xlsx, by = c("location_name", "nation_region")) %>% 
  left_join(Net_drift_GBD_xlsx, by = c("location_name", "nation_region")) 


GBD_data_table1_xlsx = GBD_data_table1[,c("nation_region", "location_name", paste0(measure_name_suffix,"_StartYear"),"ASR_StartYear",  paste0(measure_name_suffix,"_EndYear"),  "ASR_EndYear", "ASR_EAPC","ASR_Net_drift")]

write.xlsx(GBD_data_table1_xlsx,file = file.path(Table_folder,"table1_GBD_data_EAPC_UI.xlsx"))
fwrite(GBD_data_table1_xlsx,file = file.path(Table_folder,"table1_GBD_data_EAPC_UI.csv"))
remove_UI <- function(x) gsub("\\s*\\([^\\)]+\\)", "", x)

GBD_data_table1_xlsx_delUI <- GBD_data_table1_xlsx %>%
  mutate(across(everything(), remove_UI)) %>%
  mutate(across(-c(nation_region, location_name), as.numeric))


GBD_data_table1_xlsx_delUI$location_name  = factor(GBD_data_table1_xlsx_delUI$location_name,levels =location_name_level)


GBD_data_table1_xlsx_delUI = GBD_data_table1_xlsx_delUI %>% arrange(location_name,desc(ASR_EAPC))

write.xlsx(GBD_data_table1_xlsx_delUI,file = file.path(Table_folder,"table1_GBD_data_EAPC_delUI.xlsx"))




# Figure3 ----------------------------------------------------------------

load("0.demo_data/age_location_level.Rdata")
load("0.demo_data/APC_df_names.Rdata")
load("0.demo_data/map_color.Rdata")


location_ids_filter = c(Global_region_ids,SDI_region_ids)

location_df = location_df %>% 
  filter(location_id %in% location_ids_filter) %>% 
  select(nation_region,location_id, location_name)

# Figure3A Local Drifts ----------------------------------------------------------------

APC_result_local_drift = read.xlsx(xlsxFile = file.path(Table_folder,paste0(cause_name,"_GBD_APC_result.xlsx")),sheet = "LocalDrifts") %>% left_join(location_df,by ="location_id" )

APC_result_local_drift_filter = APC_result_local_drift %>% filter(location_id %in% location_ids_filter,measure == measure_name_filter)

Global_SDI_level = c("Global" ,SDI_region_level)
APC_result_local_drift_filter$location_name = factor(APC_result_local_drift_filter$location_name, levels = Global_SDI_level)

names(APC_result_local_drift_filter)  = c("Age", "Percent_Change", "CILo", "CIHi", 
"location_id", "cause", "measure", "sex", "nation_region", "location_name"
)
APC_result_local_drift_df = APC_result_local_drift_filter %>% select(c("location_name","Age","sex",  "Percent_Change", "CILo", "CIHi"))
fwrite(APC_result_local_drift_df,file = file.path(Figure_folder,"Figure3A_Local_drifts.csv"))
TableS8 = APC_result_local_drift_df
write.xlsx(TableS8,file = file.path(Table_folder,paste0(TableS8_name,".xlsx")),sheetName = TableS8_name,overwrite = TRUE)
Figure3A <- ggplot(data = APC_result_local_drift_filter, aes(x = Age, y = Percent_Change, color = location_name, shape = location_name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ sex, scales = "fixed", nrow = 1) + 
  labs(
    x = NULL,
    y = "Local drifts\n Percent per year",
    color = "location_name",
    shape = "location_name"
  ) +
  theme_minimal() +
  scale_color_manual(values = Global_SDI_color) +
  scale_shape_manual(values = c(19, 9, 17, 25, 15, 8)) +
  scale_x_continuous(breaks = seq(15, 95, by = 10)) +
  theme(
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(0.2, "cm"), 
    strip.text = element_text(size = 18, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.5),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.5, "lines")
  ) +
  coord_cartesian(clip = "off")  




# Figure3B Age effects ----------------------------------------------------------------

APC_result_age = read.xlsx(xlsxFile = file.path(Table_folder,paste0(cause_name,"_GBD_APC_result.xlsx")),sheet = "LongAge") %>% left_join(location_df,by ="location_id" )

APC_result_age_filter = APC_result_age %>% filter(location_id %in% location_ids_filter,measure == measure_name_filter)

Global_SDI_level = c("Global" ,SDI_region_level)
APC_result_age_filter$location_name = factor(APC_result_age_filter$location_name, levels = Global_SDI_level)

APC_result_age_df = APC_result_age_filter %>% select(c("location_name","Age", "Rate", "CILo", "CIHi"))
fwrite(APC_result_age_df,file = file.path(Figure_folder,"Figure3B_Age_effect.csv"))
TableS9 = APC_result_age_df
write.xlsx(TableS9,file = file.path(Table_folder,paste0(TableS9_name,".xlsx")),sheetName = TableS9_name,overwrite = TRUE)

Figure3B <- ggplot(data = APC_result_age_filter, aes(x = Age, y = Rate, color = location_name, shape = location_name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ sex, scales = "fixed", nrow = 1) +  
  labs(
    x = NULL,
    y = paste0("Age effects\n",measure_name_suffix," rate (per 100,000)"),
    color = "location_name",
    shape = "location_name"
  ) +
  theme_minimal() +
  scale_color_manual(values = Global_SDI_color) +
  scale_shape_manual(values = c(19, 9, 17, 25, 15, 8)) +
  scale_x_continuous(breaks = seq(15, 95, by = 10)) +
  theme(
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(0.2, "cm"), 
    strip.text = element_text(size = 18, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14,face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.5),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.5, "lines")
  ) +
  coord_cartesian(clip = "off")  

# Figure3C ----------------------------------------------------------------
# Period effects -------------------------------------------------------------

APC_result_period = read.xlsx(xlsxFile = file.path(Table_folder,paste0(cause_name,"_GBD_APC_result.xlsx")),sheet = "PeriodRR") %>% left_join(location_df,by ="location_id" )

APC_result_period_filter = APC_result_period %>% filter(location_id %in% location_ids_filter,measure == measure_name_filter) %>% Format_names() %>% mutate(logratio = log2(rate.ratio))
APC_result_period_filter$location_name = factor(APC_result_period_filter$location_name, levels = Global_SDI_level)


APC_result_period_filter <- APC_result_period_filter %>%
  mutate(Year = as.integer(period - 0.5))
APC_result_period_df = APC_result_period_filter %>% select(c("location_name","period", "sex", "rate.ratio", "ci.lo", "ci.hi","logratio"))

fwrite(APC_result_period_df,file = file.path(Figure_folder,"Figure3C_period_effect.csv"))
TableS10 = APC_result_period_df
write.xlsx(TableS10,file = file.path(Table_folder,paste0(TableS10_name,".xlsx")),sheetName = TableS10_name,overwrite = TRUE)

Figure3C <-ggplot(data = APC_result_period_filter, aes(x = Year, y = logratio, color = location_name, shape = location_name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ sex, scales = "fixed", nrow = 1) +
  labs(
    x = NULL,
    y = paste0("Period effects\nLog2(",measure_name_suffix," rate ratio)"),
    color = "location_name",
    shape = "location_name"
  ) +
  theme_minimal() +
  scale_color_manual(values = Global_SDI_color) +
  scale_shape_manual(values = c(19, 9, 17, 25, 15, 8)) +
  scale_x_continuous(breaks = seq(StartYear, EndYear, by = 5), labels = seq(StartYear, EndYear, by = 5)) +
  theme(
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    strip.text = element_text(size = 18, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.5),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.5, "lines")
  ) +
  coord_cartesian(clip = "off")

# Figure3D ----------------------------------------------------------------
# Period effects -------------------------------------------------------------

APC_result_cohort = read.xlsx(xlsxFile = file.path(Table_folder,paste0(cause_name,"_GBD_APC_result.xlsx")),sheet = "CohortRR") %>% left_join(location_df,by ="location_id" )

APC_result_cohort_filter = APC_result_cohort %>% filter(location_id %in% location_ids_filter,measure == measure_name_filter) %>% Format_names() %>% mutate(logratio = log2(rate.ratio))
APC_result_cohort_filter$location_name = factor(APC_result_cohort_filter$location_name, levels = Global_SDI_level)

APC_result_cohort_df = APC_result_cohort_filter %>% select(c("location_name","cohort", "sex", "rate.ratio", "cilo", "cihi","logratio"))


fwrite(APC_result_cohort_df,file = file.path(Figure_folder,"Figure3D_cohort_effect.csv"))
TableS11 = APC_result_cohort_df
write.xlsx(TableS11,file = file.path(Table_folder,paste0(TableS11_name,".xlsx")),sheetName = TableS11_name,overwrite = TRUE)

Figure3D <-ggplot(data = APC_result_cohort_filter, aes(x = cohort, y = logratio, color = location_name, shape = location_name)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ sex, scales = "fixed", nrow = 1) +
  labs(
    x = NULL,
    y = paste0("Cohort effects\nLog2(",measure_name_suffix," rate ratio)"),
    color = "location_name",
    shape = "location_name"
  ) +
  theme_minimal() +
  scale_color_manual(values = Global_SDI_color) +
  scale_shape_manual(values = c(19, 9, 17, 25, 15, 8)) +
  scale_x_continuous(breaks = seq(1895, 2005, by = 15), limits = c(1895, 2005)) +
  theme(
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm"),
    strip.text = element_text(size = 18, face = "bold"),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.5),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.spacing = unit(0.5, "lines")
  ) +
  coord_cartesian(clip = "off")

#  --------------------------------------------------------------------

combined_plot <- (Figure3A)/(Figure3B) / (Figure3C) / (Figure3D) +
  plot_layout(ncol = 1, heights = c(1,1, 1, 1)) 

Figure3 <- combined_plot +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12,face = "bold"),   
        legend.box = "horizontal",  
        legend.key.size = unit(1, "cm"), 
        legend.margin = margin(t = 0, b = 0),  
        legend.spacing.x = unit(0.2, 'cm')) & 
  guides(color = guide_legend(nrow = 1))  

ggsave2(filename = file.path(Figure_folder,"Figure3_APC_model.pdf"),plot = Figure3,width = 10,height = 14)
