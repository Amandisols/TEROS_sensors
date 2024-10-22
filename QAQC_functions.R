# QAQC functions
# epp
# Adapted from python code sent by Steven Quiring, described in 2016 paper
# introducing the National Soil Moisture Database

library(dplyr)

# set parameters for cleaning functions ----------------------------------------

## set range test parameters
vwc_min <- 0
vwc_max <- 0.5
vwc_temp_min <- 0
vwc_temp_max <- 0
matric_kPa_min <- 0
matric_kPa_max <- 0
matric_temp_min <- 0
matric_temp_max <- 0
satext_min <- 0
satext_max <- 0

## set window size and streak length
max_streak_length <- 10

## set window size for climatology and temporal interpolation
window_size <- 29

## set the flag identifiers for each QAQC step
flag_ts <- 1 # DONE!
flag_range <- 2
flag_streak <- 3
flag_mad <- 4
flag_step <- 5
flag_mad_step <- 6
flag_add_dar <- 10

## bind range parameters to dataframe for looping through later
range_parameters <- data.frame(vars = c('vwc', 'vwc_temp', 'matric_kPa', 'matric_temp', 'satext'),
                               min = c(vwc_min, vwc_temp_min, matric_kPa_min, matric_temp_min, satext_min),
                               max = c(vwc_max, vwc_temp_max, matric_kPa_max, matric_temp_max, satext_max))

# function - convert to time series --------------------------------------------

## Step 1 of QAQC - creates rows for missing datetime values, fills datetime,
## and fills in the rest of the row with NA

fill_datetimes <- function(df, datetime_col, step_flag) {
  ## Ensure the datetime_col is converted to POSIXct and summarize by hour if needed
  ## get rid of logger info here?
  df <- df %>% 
    mutate(!!sym(datetime_col) := as.POSIXct(!!sym(datetime_col), format = "%Y-%m-%d %H")) %>%
    mutate(flag = NA) %>% select(1, flag, everything(), -starts_with("logger")) %>% 
    group_by(!!sym(datetime_col)) %>%
    summarise(across(everything(), ~ if (all(is.na(.))) NA else mean(., na.rm = TRUE)))
  
  ## Get min and max for the file being processed
  min_datetime <- min(df[[datetime_col]], na.rm = TRUE)
  max_datetime <- max(df[[datetime_col]], na.rm = TRUE)
  
  ## Generate hourly datetime values between min and max, create flag column
  complete_datetime <- data.frame(
    datetime = seq(from = min_datetime, to = max_datetime, by = "hour"),
    flag = step_flag)
  
  ## Add empty measurement columns to complete_datetime
  add_cols <- setdiff(names(df), names(complete_datetime))
  complete_datetime[add_cols] <- NA
  
  ## Identify missing time values, merge with original dataframe
  new_times <- complete_datetime[!complete_datetime$datetime %in% df[[datetime_col]], ]
  merged_df <- rbind(df, new_times)
  merged_df <- merged_df[order(merged_df[[datetime_col]]),] 
  
  return(merged_df)
}

# function - create QAQC and flag columns --------------------------------------

## Step 1.1 of QAQC - no test applied here, just initiating columns in the df

create_qaqc_and_flag_columns <- function(df) {
  ## Don't make qaqc and flag cols for cols 1 and 2
  cols_to_keep <- df[, 1:2]
  
  ## Loop through the rest of the columns
  new_cols <- lapply(names(df)[-c(1, 2)], function(col) {
    qaqc_col <- paste0(col, "_qaqc")  
    flag_col <- paste0(col, "_flag")  
    ## Initialize QAQC columns (copies of originals) and a flag columns (NA)
    list(df[[col]], df[[col]], NA)
  })
  
  ## Combine original, QAQC, and Flag columns in order
  combined_cols <- do.call(cbind, c(cols_to_keep, unlist(new_cols, recursive = FALSE)))
  
  ## Assign names for the new dataframe
  new_colnames <- c(names(cols_to_keep), unlist(lapply(names(df)[-c(1, 2)], function(col) {
    c(col, paste0(col, "_qaqc"), paste0(col, "_flag"))
  })))
  colnames(combined_cols) <- new_colnames
  combined_cols <- as.data.frame(combined_cols)
  ## make sure the datetime column stays in the correct format
  combined_cols$datetime <- as.POSIXct(combined_cols$datetime, format = "%Y-%m-%d %H")
  
  return(combined_cols)
}


# function - perform range test ------------------------------------------------

## Step 2 of QAQC - remove data outside ranges set at top of script

perform_range_test <- function(df, cols_to_test, range_min, range_max, step_flag) {
  # define range constraints function and remove out of bounds values
  constrain <- function(x) {ifelse(x < range_min | x > range_max, NA, x)}
  df <- df %>% mutate(across(.cols = all_of(cols_to_test), .fns = constrain))
  return(df)
}

# function - compare and flag --------------------------------------------------

## Step 2.1 of QAQC - compare base columnd to QAQC columns and flag with step #

compare_and_flag <- function(df, flag) {
  ## get the qaqc column names from the dataframe
  qaqc_cols <- grep("_qaqc$", colnames(df), value = TRUE)
  ## loop through the qaqc columns to get base name, flag col, and add the flags
  for (qaqc_col in qaqc_cols) {
    ## get base column names
    base_col <- sub("_qaqc$", "", qaqc_col)
    ## get flag col names
    flag_col <- paste0(base_col, "_flag")
    ## compare base and _qaqc cols, add flag marker to _flag cols
    df[[flag_col]] <- ifelse(!is.na(df[[base_col]]) & is.na(df[[qaqc_col]]), 
                             flag, df[[flag_col]])
  }
  return(df)
}

# function - persistence test --------------------------------------------------

## Step 3 of QAQC - looks for values that persist beyond a set time period

# perform_persistence_test <- function(df, cols_to_test, streak_length, step_flag) {
#   df <- df %>%
#   mutate(repeat_flag = with)
#   
# }
