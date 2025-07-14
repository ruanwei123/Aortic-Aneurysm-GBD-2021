function_year5 <- function(table_name, start_year, end_year, current_year){
  
  remain <- current_year - floor((current_year - start_year)/5) * 5 
  year_names <- NULL
  for (i in start_year:end_year) {
    if((i - current_year)/5 - floor((i - current_year)/5) == 0){
      if(i == remain){
        temp <- paste(start_year, i, sep = '-')
        year_names <- append(year_names, temp)
      }
      else{
        temp <- paste(i-4, i, sep = '-')
        year_names <- append(year_names, temp)
      }
    }
  }
  
  table_name <- as.data.frame(table_name)
  new_years <- seq(start_year,end_year,1)
  new_table <- as.data.frame(matrix(data = rep(0, length(year_names)*nrow(table_name)), ncol = length(year_names), nrow = nrow(table_name)))  %>% as.data.frame()
  colnames(new_table) <- year_names
  
  j = 1
  for (i in 1:(end_year - start_year + 1)){
    if((new_years[i] - current_year)/5 - floor((new_years[i] - current_year)/5) != 0){
      new_table[, year_names[j]] <- new_table[,year_names[j]] + table_name[,as.character(new_years[i])]
    }
    else{
      if(j == 1){
        new_table[,year_names[j]] <- (new_table[,year_names[j]] + table_name[,as.character(new_years[i])]) / (remain - start_year + 1)
      }
      else{
        new_table[,year_names[j]] <- (new_table[,year_names[j]] + table_name[,as.character(new_years[i])]) / 5
      }
      j = j + 1
    }
  }
  return(new_table)
}
