GBD_EAPC <- function (data, rei, EAPC_95CI, digits, sep) 
{
      EAPC_cal <- as.data.frame(matrix(nrow = 0, ncol = 8))
      names(EAPC_cal) <- c("location_id","location_name", "measure_name", "EAPC", "LCI", "UCI")
      for (i in unique(data$location_name)) {
        for (j in unique(data$measure_name)) {
             a <- data %>% filter(location_name == i,measure_name == j)
             # subset(data, location_name == i,measure_name == j)
             location_id_filter = unique(a$location_id)
              a$y <- log(a$val)
              mod_simp_reg <- lm(y ~ year, data = a)
              estimate <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 
                1]) - 1) * 100
              low <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 
                1] - 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 
                2]) - 1) * 100
              high <- (exp(summary(mod_simp_reg)[["coefficients"]][2, 
                1] + 1.96 * summary(mod_simp_reg)[["coefficients"]][2, 
                2]) - 1) * 100
              eapc_df = data.frame(location_id = location_id_filter, location_name = i, measure_name = j, EAPC = estimate, LCI = low, UCI = high)
              EAPC_cal = rbind(EAPC_cal,eapc_df)
          } }
      
    EAPC_cal$EAPC_95CI <- paste(round(as.numeric(EAPC_cal$EAPC), 
        digits = digits), round(as.numeric(EAPC_cal$LCI), 
        digits = digits), sep = "(") %>% paste(round(as.numeric(EAPC_cal$UCI), 
        digits = digits), sep = sep) %>% paste0(")")
    EAPC_cal$EAPC <- as.numeric(EAPC_cal$EAPC)
    EAPC_cal$UCI <- as.numeric(EAPC_cal$UCI)
    EAPC_cal$LCI <- as.numeric(EAPC_cal$LCI)
    return(EAPC_cal)
}
