# APPLY QAQC 

# get file --- just for now - these functions will later be applied across 
# the masters folder??
test_data <- read.csv("C:/GitHub/TEROS_sensors/HubbardBrook/master/HBw3B.csv")

# add rows for missing datetimes and fills those values ------------------------
data_datetimes_filled <- fill_datetimes(test_data, "datetime", flag_ts)


# add qaqc and flag columns ----------------------------------------------------
data_qaqc_flags <- create_qaqc_and_flag_columns(data_datetimes_filled)


# get the column names for each observation type in a list ---------------------
  ## if you dont want to apply tests to a var, you can comment it out! 

## volumetric water content
vwc <- names(data_qaqc_flags)[grepl("^vwc", names(data_qaqc_flags)) & 
                                !grepl("^vwc_temp", names(data_qaqc_flags)) & 
                                grepl("qaqc", names(data_qaqc_flags))]
## vwc temperature
vwc_temp <- names(data_qaqc_flags)[grepl("^vwc_temp", names(data_qaqc_flags)) & 
                                     grepl("qaqc", names(data_qaqc_flags))]
## matric kPa
matric_kPa <- names(data_qaqc_flags)[grepl("^matric_kPa", names(data_qaqc_flags)) & 
                                       grepl("qaqc", names(data_qaqc_flags))]
## matric temperature
matric_temp <- names(data_qaqc_flags)[grepl("^matric_temp", names(data_qaqc_flags)) & 
                                        grepl("qaqc", names(data_qaqc_flags))]
## electrical conductivity
satext <- names(data_qaqc_flags)[grepl("^satext", names(data_qaqc_flags)) & 
                                   grepl("qaqc", names(data_qaqc_flags))]

## bind lists of colnames into list
list_of_var_cols <- list(vwc = vwc, 
                         vwc_temp = vwc_temp, 
                         matric_kPa = matric_kPa, 
                         matric_temp = matric_temp, 
                         satext = satext)


# perform the range test -------------------------------------------------------
data_range_flagged <- range_test_and_flag(data_qaqc_flags, list_of_var_cols,
                                          range_parameters, flag_range)
