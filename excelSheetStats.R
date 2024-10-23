### testing out code snippets to use for sm raw file reformatting .Rmd
### epp
### 10.4.2024

### Notes: this isn't a very efficient function so i threw in a progress bar
### so the user can have an estimate of how long it should take to process

library(dplyr)
library(readxl)
library(progress)

# set directory that contains sub directories of downloads
forest_directory <- "C:/GitHub/TEROS_sensors/Coweeta/raw"

# get file list
file_list <- list.files(path = forest_directory,
                        pattern = ".xlsx",
                        recursive = TRUE,
                        full.names = TRUE)

# initialize file stats data.frame for the loop
file_stats <- data.frame(subdir = character(),
                         fileName = character(),
                         sheetName = character(),
                         noRows = numeric(),
                         noCol = numeric(),
                         dateMin = as.POSIXct(character()),
                         dateMax = as.POSIXct(character()),
                         stringsAsFactors = FALSE)

# function to check excel file sheets
check_sheets <- function(file) {
  # get values for table
  subdirectory <- dirname(file)
  subdirectory <- gsub(paste0(forest_directory, "/"), "", subdirectory)
  file_name <- basename(file)
  
  # get all sheet names in the file
  sheet_names <- excel_sheets(file)
  
  # message output in console if 'Metadata' sheet missing
  if (!"Metadata" %in% sheet_names) {
    message(paste("Warning: File", file_name, "in", subdirectory,
                  "does not contain a sheet called 'Metadata'"))
  }
  
  # loop through all sheets in each file
  for (sheet_name in sheet_names) {
    # Read the sheet data, skipping first 2 rows
    sheet_data <- suppressMessages(read_excel(file, sheet = sheet_name, col_names = FALSE, skip = 3))
    
    # Get number of rows and columns
    no_rows <- nrow(sheet_data)
    no_col <- ncol(sheet_data)
    if(sheet_name != "Metadata") {
      date_min <- min(as.POSIXct(sheet_data$'...1'), na.rm = TRUE)
      date_max <- max(as.POSIXct(sheet_data$'...1'), na.rm = TRUE)
    } else {
      date_min <- NA
      date_max <- NA
    }
    
    # Append results to the file_stats dataframe
    file_stats <<- rbind(file_stats, data.frame(subdir = subdirectory,
                                                fileName = file_name,
                                                sheetName = sheet_name,
                                                noRows = no_rows,
                                                noCol = no_col,
                                                dateMin = date_min,
                                                dateMax = date_max))
  }
}


# loop through all files in directory, check sheets in each file
# also lets add a progress bar because i like knowing how long it will take
pb <- progress_bar$new(
  format = "Processing files [:bar] :percent :elapsed",
  total = length(file_list), 
  clear = FALSE, 
  width = 60)

for (file in file_list) {
  check_sheets(file)
  #Progress bar to display file processing
  pb$tick(tokens = list(file = file)) }

# Are the metadata sheets all formatted the same?
meta_cols <- file_stats %>% filter(sheetName == "Metadata") %>% select(-dateMin, -dateMax)

# Filter for files with more than one "processed data config" sheet
# filter out the Metadata sheet and the max processed data sheet 
# (thats the one that we've automatically been using)
# SO, this shows all the sheets that have been missed
sheets_extra <- file_stats %>% 
  distinct() %>%
  filter(!grepl("Raw", sheetName)) %>%
  filter(!grepl("Metadata", sheetName)) %>%
  mutate(processedSheet = as.numeric(substr(sheetName, nchar(sheetName)-1, nchar(sheetName)))) %>%
  select(subdir, fileName, processedSheet, sheetName, everything()) %>%
  group_by(fileName) %>%
  filter(n() > 1) %>%  
  ungroup() 

# these are all the sheets that have been missed!
unprocessed_sheets <- sheets_extra %>% 
  group_by(fileName) %>%
  filter(processedSheet != max(processedSheet))

unprocessed_sheets <- unprocessed_sheets %>%
  filter(noCol > 5)

# total date range for all the unprocessed sheets
missed_sheets_dates <- unprocessed_sheets %>% ungroup() %>%
  summarize(min = min(dateMin), max = max(dateMax))








### ----------------------------------------------------------------------------
# code playground below



# what data are in these sheets??
# make a for loop to read over all the column headers?

for (i in 1:length(unprocessed_sheets$fileName)) {
test <- suppressMessages(read_excel(paste0(forest_directory, "/", sheets_extra$subdir[i], "/", sheets_extra$fileName[i]), 
                                    sheet = sheets_extra$sheetName[[i]], col_names = FALSE))

}


i <- 5
test <- suppressMessages(read_excel(paste0(forest_directory, "/", sheets_extra$subdir[i], "/", sheets_extra$fileName[i]), 
                                    sheet = sheets_extra$sheetName[[i]], col_names = FALSE)) 







