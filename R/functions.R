library(COINr)

lengthen <- function(X, cols = NULL){
  
  # make df
  X <- as.data.frame(X)
  
  if(!is.null(cols)){
    
    stopifnot(all(cols %in% names(X)))
    X_ <- X[cols]
    X <- X[!(names(X) %in% cols)]
    X$V_to_pivot <- rownames(X)
    
  } else {
    X_ <- X
  }
  
  # stack and add names
  X1 <- cbind(utils::stack(X_), rownames(X_))
  names(X1) <- c("Value", "V2", "V1")
  X1$V2 <- as.character(X1$V2)
  X1 <- rev(X1)
  
  if(!is.null(cols)){
    X1 <- merge(X, X1, by.x = "V_to_pivot", by.y = "V1", all = TRUE)
    X1 <- X1[names(X1) != "V_to_pivot"]
    names(X1)[names(X1) == "V2"] <- "name"
  }
  
  X1
  
}


# Function to find the four closest conditions for every uCode
find_closest_conditions <- function(input_purse){
  # Initialize an empty data frame to store results
  result_table <- data.frame(uCode = integer(), year = integer(), condition_1 = integer(), condition_2 = integer(), condition_3 = integer(), condition_4 = integer())
  
  school_names <- get_data(input_purse,dset="Aggregated",iCodes = "conditions",also_get = "uName") %>%
    mutate(year=as.character(case_when(Time == 17~1,Time == 18~2,Time == 19~3,Time == 20~4,T~999))) %>%
    select(-conditions,-Time)
  
  years = c(1,2,3,4)
  
  for (year in years) { 
    data <- get_data(input_purse$coin[[year]],dset="Aggregated",iCodes = "conditions")
  # Iterate through each uCode
    for (target_uCode in unique(data$uCode)) {
      # Calculate absolute differences
      data$difference <- abs(data$conditions - data$conditions[data$uCode == target_uCode])
    
      # Sort the data frame based on differences
      sorted_data <- data[order(data$difference), ]
    
      # Get the top four closest conditions
      top_four_conditions <- head(sorted_data, 5)$uCode
    
      # Create a row for the result table
      result_row <- c(target_uCode, year, top_four_conditions)
      result_table <- rbind(result_table, result_row)

    }
    
    result_table <- rbind(result_table, result_row)
    
  }
  
  # Rename columns
  colnames(result_table) <- c("uCode", "year", "self", "peer_1", "peer_2", "peer_3", "peer_4")
  result_table <- result_table %>%
    select(-self)  %>%
    left_join(school_names,by=c("uCode","year")) %>%
    unique()
  # Iterate )
  
  return(result_table)
}


color_gradient <- function(dt, column_name, gradient_colors) {
  
  col_func <- colorRampPalette(gradient_colors)
  dt %>% 
    formatStyle(column_name, 
                backgroundColor = styleEqual(
                  sort(unique(dt$x$data[[column_name]]), decreasing = TRUE),
                  col_func(length(unique(dt$x$data[[column_name]])))
                )
    ) 
}
