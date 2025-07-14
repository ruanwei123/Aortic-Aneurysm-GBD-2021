# 1.1.merge GBD data ----------------------------------------------------------

library(data.table)
library(openxlsx)
library(dplyr)
folder_input = "0.GBD_data_origin"

cause_name = read.xlsx("0.variable_df.xlsx") %>% .$cause_name
folder_output = "0.GBD_data"
if(!dir.exists(folder_output))dir.create(folder_output)
data_files = list.files(path = folder_input,full.names = T)  

data_list = lapply(data_files,fread)

GBDdata = rbindlist(data_list)

location_id_df = read.xlsx("0.demo_data/GBD_data_location_id.xlsx") %>% select(c("location_id","location_name"))
location_ids = location_id_df$location_id
GBDdata = GBDdata %>% filter(location_id %in% location_ids) %>% select(-location_name)

GBDdata = left_join(GBDdata,location_id_df,by = "location_id")
GBDdata = GBDdata %>% mutate(
  measure_name = case_when(
    measure_name=="DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
    TRUE ~ measure_name
  )
)
fwrite(GBDdata,file.path(folder_output,paste0(cause_name,"_GBDdata_origin.csv")))


library(data.table)
library(openxlsx)
library(dplyr)
Risk_folder_input = "0.GBD_data_risk"

cause_name = read.xlsx("0.variable_df.xlsx") %>% .$cause_name
folder_output = "0.GBD_data"
if(!dir.exists(folder_output))dir.create(folder_output)
Risk_data_files = list.files(path = Risk_folder_input,full.names = T)  

Risk_data_list = lapply(Risk_data_files,fread)

Risk_data = rbindlist(Risk_data_list)

location_id_df = read.xlsx("0.demo_data/GBD_data_location_id.xlsx") %>% select(c("location_id","location_name"))
location_ids = location_id_df$location_id
Risk_data = Risk_data %>% filter(location_id %in% location_ids) %>% select(-location_name)
Risk_data = left_join(Risk_data,location_id_df,by = "location_id")

Risk_data = Risk_data %>% mutate(
  measure_name = case_when(
    measure_name=="DALYs (Disability-Adjusted Life Years)" ~ "DALYs",
    TRUE ~ measure_name
  )
)
fwrite(Risk_data,file.path(folder_output,paste0(cause_name,"_Riskdata.csv")))


# age_location_level.Rdata ------------------------------------------------
cause_folder =  "0.GBD_data"
Table_folder = "1.Table"
Figure_folder = "2.Figure"
Population_folder = "0.population_data"
if(!dir.exists(Table_folder))dir.create(Table_folder)
if(!dir.exists(Figure_folder))dir.create(Figure_folder)
Global_region_ids = 1

age_group_df = read.xlsx("0.demo_data/GBD_data_age_strat.xlsx")
age_strat_ids = age_group_df %>% filter(age_group=="age_strat") %>% .$age_id
age_strat_name = age_group_df %>% filter(age_group=="age_strat") %>% .$age_name
age_APCmodel_df = age_group_df%>% filter(age_group=="age_strat",age_id != 235)
age_APCmodel_ids = age_APCmodel_df$age_id
age_APCmodel_name_level = age_APCmodel_df$age_name
  
location_df = read.xlsx("0.demo_data/GBD_data_location_id.xlsx")
location_id_location_name = location_df %>% select(c("location_id","location_name"))
location_id_GBD_location = location_df %>% select(c("location_id","GBD_location_name"))

SDI_region_ids = location_df %>% filter(nation_region=="SDI_region") %>% .$location_id
SDI_region_level = location_df %>% filter(nation_region=="SDI_region") %>% .$location_name
nation_region_ids = location_df %>% filter(nation_region=="nation") %>% .$location_id
GBD_region_ids = location_df %>% filter(nation_region=="GBD_region") %>% .$location_id
GBD_region_level = c("High-income North America", "Southern Latin America", "High-income Asia Pacific", 
"Australasia", "Western Europe", "East Asia", "Central Asia", 
"South Asia", "Southeast Asia", "Oceania", "Central Europe", 
"Eastern Europe", "Andean Latin America", "Central Latin America", 
"Tropical Latin America", "Caribbean", "North Africa and Middle East", 
"Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa", "Southern Sub-Saharan Africa", 
"Western Sub-Saharan Africa")


variable_df <- read.xlsx("0.variable_df.xlsx")
Table_names_df = read.xlsx("0.Table_names.xlsx")
library(purrr)
list2env(setNames(as.list(variable_df), names(variable_df)), .GlobalEnv)
list2env(setNames(as.list(Table_names_df), names(Table_names_df)), .GlobalEnv)

save.image(file = "0.demo_data/age_location_level.Rdata")


#  --------------------------------------------------------------------

load("0.demo_data/age_location_level.Rdata")
folder_output = "0.GBD_data"
GBDdata = fread(file = file.path(folder_output,paste0(cause_name,"_GBDdata_origin.csv")))
GBDdata_filter = GBDdata %>% filter(
  measure_name %in% measure_name_filter,
  location_id %in% c(Global_region_ids,SDI_region_ids,GBD_region_ids,nation_region_ids),
  age_id %in% c(age_strat_ids,22,27),
  year %in% StartYear:EndYear,
  
)
fwrite(GBDdata_filter,file.path(folder_output,paste0(cause_name,"_GBDdata.csv")))




#  ------------------------------------------------------------------
load("0.demo_data/age_location_level.Rdata")
folder_output = "0.population_data"
population_data = fread(file = file.path(folder_output,"GBDpredict_2017_2100.csv"))


population_data_filter = population_data %>% filter(
  location_id %in% c(Global_region_ids,SDI_region_ids,GBD_region_ids,nation_region_ids),
  age_id %in% c(age_strat_ids,22,27),
  year %in% seq(EndYear+1,EndYear+npredict_year)
)
fwrite(population_data_filter,file.path(folder_output,"Population_Projection_2017_2100_GBDregion.csv"))

#  ---------------------------------------------------------

map_color_ASR  = c("#CED9E5FF", "#9ECAE1", "#7EC0EE", "#A1D99B", "#CDCD00", "#FFB90F", 
"#E87B1E", "#FF0000")

netdrift_map_color = c("#21528AFF",  "#3280B5FF",  "#A1D99B", "#FFB90F", "#FF0000" )
EAPC_map_color = c("#21528AFF",  "#3280B5FF", "#A1D99B", "#FFB90F", "#FF0000" )

age_group_color <- c("#26456EFF", "#1C73B1FF", "#3280B5FF", "#4A93C1FF", "#67ADD4FF", 
                     "#74BFDDFF", "#90CCDFFF", "#B4D4DAFF", "#E0DBDCFF", "#F0C294FF", 
                     "#F9B376FF", "#FEA058FF", "#FD8938FF", "#F57120FF", "#E85A0BFF", 
                     "#D74401FF", "#B43802FF", "#7B3014FF")
age_strat_color = c( "#BC3C29FF", "#8582BD",  "#749B58FF","#E18727FF", "#0072B5FF")

Bubble_color = c("#4A708B", "#FFDD8E", "#EE0000")

Global_clor = "black"
SDI_colors <- c("red", "purple", "#FF8C00", "#0076B9","#00A087FF")
Global_SDI_color = c(Global_clor,SDI_colors)
frontier_continue_colors = c("#9BCD9B", "#FFB6C1", "#191970")
frontier_colors = c("#228B22", "#CD3700",'darkblue','darkred')

decomposition_colors <- c("#0076B9", "#00A087FF", "#FF8C00")
risk_colors = c("#D9DEE7", "#FBEA2E","#70CDBE", "#F8B072","#8582BD", "#4F99C9",  "#458A74", "#C74546", "#803E75")

#nation_map
legend.text.size = 8
legend.title.size = 10


save.image(file = "0.demo_data/map_color.Rdata")


#  -------------------------------------------------------
APC_df_rownames =  c( "Coefficients", "WaldTests", "CombinationTests")
APC_df_names = c(  "NetDrift", "AgeDeviations",  "PerDeviations",
"CohDeviations", "LongAge", "LongAgeRR", 
"CrossAge", "CrossAgeRR",  "Long2CrossRR", 
 "FittedTemporalTrends", "PeriodRR", 
"CohortRR",  "LocalDrifts")

APC_df_all_names <- c(APC_df_rownames, APC_df_names)

save.image(file = "0.demo_data/APC_df_names.Rdata")
rm(list = ls())

#  ---------------------------------------------------------
Quantile_Number <- function(data, n = 5, maxN) 
{
    breaks = round(seq(0, maxN, length.out = n + 1))
    
    labels <- character(n)
    for (z in 1:n) {
        if (z == 1) {
            labels[z] <- paste0("0 ~ ", breaks[z + 1])
        } else {
            labels[z] <- paste(breaks[z], breaks[z + 1], sep = " ~ ")
        }
    }
    
    result <- cut(data, breaks = breaks, labels = labels, 
                  include.lowest = TRUE, right = FALSE)
    
    return(result)
}

ceiling_with_precision <- function(x, digits = 0) {
  multiplier <- 10^digits
  
  if (x >= 0) {
    result <- ceiling(x * multiplier) / multiplier
  } else {
    result <- floor(x * multiplier) / multiplier
  }
  
  return(result)
}


Quantile_neg_pos <- function(data, n = 5, precision = 1) 
{
    # Adjust the minimum and maximum data values according to precision
    min_data <- ceiling_with_precision(min(data), precision)
    max_data <- ceiling_with_precision(max(data), precision)
    
    # Determine the absolute values of the minimum and maximum
    abs_min <- abs(min_data)
    abs_max <- abs(max_data)
    
    # Initialize the breaks variable
    breaks <- numeric(0)
    half_n = round(n/2)
    # Determine the proportion of segments for negative and positive values
    if (abs_min > abs_max) {
        # More negative values, so negative side has more segments
        neg_proportion <- half_n+1
        pos_proportion <- half_n
    } else {
        # More positive values, so positive side has more segments
        neg_proportion <- half_n
        pos_proportion <- half_n+1
    }
    
    # Create the negative side breaks
    neg_breaks <- seq(min_data, 0, length.out = neg_proportion + 1)
    # Create the positive side breaks
    pos_breaks <- seq(0, max_data, length.out = pos_proportion + 1)
    
    # Combine the breaks, ensuring 0 is not repeated
    breaks <- c(neg_breaks, pos_breaks[-1])
    
    # Create labels for the groups
    labels <- character(n)
    for (i in 1:n) {
        label_start <- sprintf("%.*f", precision, breaks[i])
        label_end <- sprintf("%.*f", precision, breaks[i + 1])
        
        # Replace "0.0" or similar with "0"
        label_start <- gsub("^0.0+$", "0", label_start)
        label_end <- gsub("^0.0+$", "0", label_end)
        
        labels[i] <- sprintf("%s to %s", label_start, label_end)
    }
    
    # Cut the data into groups based on the generated breaks
    result <- cut(data, breaks = breaks, labels = labels, include.lowest = TRUE, right = FALSE)
    
    # Update the levels to replace "0.0" or similar with "0"
    levels(result) <- gsub("^0.0+$", "0", levels(result))
    
    return(result)
}



save.image(file = "0.demo_data/Quantile.Rdata")
