
# BAPC ------------------------------------------------------------------

# Function to process GBD data
process_GBD_data_n <- function(GBD_data_filter, GBD_data_n_prediction, sex) {
  GBD_data_n <- GBD_data_filter %>% 
    filter(sex_name == sex) %>% 
    select(-sex_name) %>% 
    arrange(age_name, year)  %>% 
    pivot_wider(id_cols = year, names_from = age_name, values_from = val)
  
  GBD_data_n <- bind_rows(GBD_data_n, GBD_data_n_prediction) %>% 
    arrange(year) %>% 
    column_to_rownames(var = "year") %>% 
    apply(c(1, 2), as.numeric) %>% 
    apply(c(1, 2), round) %>% 
    as.data.frame()
  
  return(GBD_data_n)
}

# Function to process population data
process_population_n <- function(GBD_population_data_filter, sex) {
  population_n <- GBD_population_data_filter %>% 
    filter(sex_name == sex) %>% 
    select(-sex_name) %>% 
    arrange(age_name, year)  %>%  
    pivot_wider(id_cols = year, names_from = age_name, values_from = val) %>% 
    column_to_rownames(var = "year") %>% 
    apply(c(1, 2), as.numeric) %>% 
    apply(c(1, 2), round) %>% 
    as.data.frame()
  
  return(population_n)
}

# Function to fit BAPC model
fit_bapc_model <- function(GBD_data_n, population_n, npredict_year, age_stand_weight) {
  BAPC_model_input <- APCList(GBD_data_n, population_n, gf=5)
  BAPC_model_result <- BAPC(BAPC_model_input, 
                            predict=list(npredict= npredict_year, retro=FALSE), 
                            secondDiff=FALSE, 
                            stdweight= age_stand_weight, 
                            verbose=F)
  BAPC_model_result <- qapc(BAPC_model_result, percentiles = c(0.025, 0.975))
  
  return(BAPC_model_result)
}

# process_BAPC_model_result -----------------------------------------------

process_BAPC_model_result <- function(BAPC_model_result, population_n, age_name_level, StartYear, Projection_Year, age_stand_weight, sex) {
  
  years_range <- StartYear:Projection_Year
  ##calculate_asr_asp
  ASR = agestd.rate(x = BAPC_model_result) %>% as.data.frame() * 10^5
  names(ASR) = c("pred_val", "pred_sd","pred_low", "pred_up")
  ASR = ASR %>% select(pred_val, pred_low, pred_up) %>%
    mutate(year = years_range,
           sex = sex)
  
  ASP = agestd.proj(x = BAPC_model_result) %>% as.data.frame() 
  names(ASP) = c("pred_val", "pred_sd","pred_low", "pred_up")
  ASP = ASP %>% select(pred_val, pred_low, pred_up) %>%
    mutate(year = years_range,
           sex = sex)
  
  #calculate projections and rates
  create_df <- function() matrix(NA, nrow = Projection_Year - StartYear + 1, ncol = length(age_name_level)) %>% as.data.frame()
  proj_mean <- proj_low <- proj_up <- rate_mean <- rate_low <- rate_up <- create_df()

  for (i in 1:length(age_name_level)) {
    proj_mean[, i] <- agespec.proj(BAPC_model_result)[[i]][, 1]
    proj_low[, i] <- agespec.proj(BAPC_model_result)[[i]][, 3]
    proj_up[, i] <- agespec.proj(BAPC_model_result)[[i]][, 4]

    rate_mean[, i] <- agespec.rate(BAPC_model_result)[[i]][, 1] * 10^5
    rate_low[, i] <- agespec.rate(BAPC_model_result)[[i]][, 3] * 10^5
    rate_up[, i] <- agespec.rate(BAPC_model_result)[[i]][, 4] * 10^5
  }

  
  
  for (df_name in c("proj_mean", "proj_low", "proj_up", "rate_mean", "rate_low", "rate_up")) {
    df <- get(df_name)
    names(df) <- age_name_level
    rownames(df) <- years_range
    assign(df_name, df)
  }
  
  
  #calculate all age projections
  all_age_projection <- data.frame(
    pred_val = rowSums(proj_mean[, ]),
    pred_low = rowSums(proj_low[, ]),
    pred_up = rowSums(proj_up[, ]),
    year = years_range,
    sex = sex
  )

  #calculate all crude rates
  crude_rate <- matrix(nrow = Projection_Year - StartYear + 1, ncol = 3) %>% as.data.frame()
  names(crude_rate) <- c("pred_val", "pred_low", "pred_up")
  
  for (i in 1:(Projection_Year - StartYear + 1)) {
    crude_rate[i, 1] <- ageadjust.direct(count = proj_mean[i, ], pop = population_n[i, ], stdpop = age_stand_weight)[1] * 10^5
    crude_rate[i, 3] <- ageadjust.direct(count = proj_up[i, ], pop = population_n[i, ], stdpop = age_stand_weight)[1] * 10^5
    crude_rate[i, 2] <- ageadjust.direct(count = proj_low[i, ], pop = population_n[i, ], stdpop = age_stand_weight)[1] * 10^5
  }
  crude_rate$year <- years_range
  crude_rate$sex <- sex

  
  #calculate age-specific rate and projections
  rate_mean_long <- rate_mean %>% mutate(year = years_range)  %>% pivot_longer(age_name_level, 
      names_to = "age", values_to = "pred_val")
  rate_up_long <- rate_up %>% mutate(year = years_range) %>% pivot_longer(age_name_level, 
      names_to = "age", values_to = "pred_up")
  rate_low_long <- rate_low %>% mutate(year = years_range) %>% pivot_longer(age_name_level, 
      names_to = "age", values_to = "pred_low")
  
  age_specific_rate = left_join(rate_mean_long,rate_low_long,by = c("year","age")) %>% left_join(rate_up_long,by = c("year","age"))
  age_specific_rate$sex <- sex
  
  
  
  proj_mean_long <- proj_mean %>% mutate(year = years_range) %>% pivot_longer(age_name_level, 
      names_to = "age", values_to = "pred_val")
  proj_up_long <- proj_up %>% mutate(year = years_range) %>% pivot_longer(age_name_level, 
      names_to = "age", values_to = "pred_up")
  proj_low_long <- proj_low %>% mutate(year = years_range) %>% pivot_longer(age_name_level, 
      names_to = "age", values_to = "pred_low")
  age_specific_proj = left_join(proj_mean_long,proj_low_long,by = c("year","age")) %>% left_join(proj_up_long,by = c("year","age"))
  
  age_specific_proj$sex <- sex
  
  
  return(list(ASR = ASR, ASP = ASP,
              proj_mean = proj_mean, proj_low = proj_low, proj_up = proj_up,
              all_age_projection = all_age_projection, crude_rate = crude_rate,
              age_specific_rate = age_specific_rate,age_specific_proj = age_specific_proj))
}


# process_BAPC_result_Both ------------------------------------------------

process_BAPC_result_Both <- function(Male_result, Female_result, population_n_Both, 
                                    age_stand_weight, Projection_Year, StartYear, 
                                     age_name_level) {
  
  years_range <- StartYear:Projection_Year
  Male_proj_mean = Male_result$proj_mean
  Male_proj_up = Male_result$proj_up
  Male_proj_low = Male_result$proj_low
  Female_proj_mean = Female_result$proj_mean
  Female_proj_up = Female_result$proj_up
  Female_proj_low = Female_result$proj_low
  
  Both_proj_mean <- Male_proj_mean + Female_proj_mean
  Both_proj_up <- Male_proj_up + Female_proj_up
  Both_proj_low <- Male_proj_low + Female_proj_low
  
  Both_rate_mean <- Both_proj_mean/population_n_Both * 10^5
  Both_rate_up <- Both_proj_up/population_n_Both * 10^5
  Both_rate_low <- Both_proj_low/population_n_Both * 10^5
  
  Both_crude_rate = Both_ASR <- matrix(nrow = 0, ncol = 3) %>% as.data.frame()
  names(Both_crude_rate) = names(Both_ASR) = c("pred_val", "pred_low", "pred_up")
  
  for (i in 1:(Projection_Year - (StartYear - 1))) {
      Both_ASR[i, 1] <- ageadjust.direct(count = Both_proj_mean[i, 
          ], pop = population_n_Both[i, ], stdpop = age_stand_weight)[2] * 
          10^5
      Both_ASR[i, 3] <- ageadjust.direct(count = Both_proj_up[i, 
          ], pop = population_n_Both[i, ], stdpop = age_stand_weight)[2] * 
          10^5
      Both_ASR[i, 2] <- ageadjust.direct(count = Both_proj_low[i, 
          ], pop = population_n_Both[i, ], stdpop = age_stand_weight)[2] * 
          10^5
      
      Both_crude_rate[i, 1] <- ageadjust.direct(count = Both_proj_mean[i, 
          ], pop = population_n_Both[i, ], stdpop = age_stand_weight)[1] * 
          10^5
      Both_crude_rate[i, 3] <- ageadjust.direct(count = Both_proj_up[i, 
          ], pop = population_n_Both[i, ], stdpop = age_stand_weight)[1] * 
          10^5
      Both_crude_rate[i, 2] <- ageadjust.direct(count = Both_proj_low[i, 
          ], pop = population_n_Both[i, ], stdpop = age_stand_weight)[1] * 
          10^5
  }
  
  Both_all_age_projection <- Male_result$all_age_projection[, 1:3] + Female_result$all_age_projection[, 
      1:3]
  Both_all_age_projection$year = Both_crude_rate$year = Both_ASR$year <- years_range
  Both_all_age_projection$sex = Both_crude_rate$sex = Both_ASR$sex <- "Both"
  
  Both_rate_mean_long <- Both_rate_mean %>% mutate(year = years_range)  %>%
    pivot_longer(age_name_level, names_to = "age", values_to = "pred_val")
  Both_rate_up_long <- Both_rate_up %>% mutate(year = years_range) %>% 
    pivot_longer(age_name_level,names_to = "age", values_to = "pred_up")
  Both_rate_low_long <- Both_rate_low %>% mutate(year = years_range) %>%
    pivot_longer(age_name_level,names_to = "age", values_to = "pred_low")
  Both_age_specific_rate = left_join(Both_rate_mean_long,Both_rate_low_long,by = c("year","age")) %>% 
    left_join(Both_rate_up_long,by = c("year","age"))
  
  Both_proj_mean_long <- Both_proj_mean %>% mutate(year = years_range) %>% 
    pivot_longer(age_name_level,names_to = "age", values_to = "pred_val")
  Both_proj_up_long <- Both_proj_up %>% mutate(year = years_range) %>% 
    pivot_longer(age_name_level,names_to = "age", values_to = "pred_up")
  Both_proj_low_long <- Both_proj_low %>% mutate(year = years_range) %>% 
    pivot_longer(age_name_level,names_to = "age", values_to = "pred_low")
  Both_age_specific_proj = left_join(Both_proj_mean_long,Both_proj_low_long,by = c("year","age")) %>% 
    left_join(Both_proj_up_long,by = c("year","age"))
  
  Both_age_specific_rate$sex = Both_age_specific_proj$sex <- "Both"
  
  return(list(ASR = Both_ASR, 
              all_age_projection = Both_all_age_projection, 
              crude_rate = Both_crude_rate,
              age_specific_rate = Both_age_specific_rate,
              age_specific_proj = Both_age_specific_proj))
}


# process_BAPC_result_All -------------------------------------------------

process_BAPC_result_All =  function(Male_result, Female_result, Both_result,measure_name_filter,location_name_filter) {
    
    return(list(ASR = rbind(Male_result$ASR, Female_result$ASR, Both_result$ASR) %>% mutate(measure=measure_name_filter,location=location_name_filter), 
                ASP = rbind(Male_result$ASP, Female_result$ASP) %>% mutate(measure=measure_name_filter,location=location_name_filter),
                all_age_projection = rbind(Male_result$all_age_projection, Female_result$all_age_projection, Both_result$all_age_projection) %>% mutate(measure=measure_name_filter,location=location_name_filter),
                crude_rate = rbind(Male_result$crude_rate, Female_result$crude_rate, Both_result$crude_rate) %>% mutate(measure=measure_name_filter,location=location_name_filter),
                age_specific_rate = rbind(Male_result$age_specific_rate, Female_result$age_specific_rate, Both_result$age_specific_rate) %>% mutate(measure=measure_name_filter,location=location_name_filter),
                age_specific_proj = rbind(Male_result$age_specific_proj, Female_result$age_specific_proj, Both_result$age_specific_proj) %>% mutate(measure=measure_name_filter,location=location_name_filter)
                ))
    
  }
