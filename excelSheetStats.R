### testing out code snippets to use for sm raw file reformatting .Rmd
### epp
### 10.4.2024

### Notes: this isn't a very efficient function but if you're only doing one
### data download folder with ~10-15 files, it shouldn't be a problem


# if you've got the tidyverse library loaded, you may need to dump it before
# running this function due to package conflicts
detach("package:readxl", unload=TRUE)
library(openxlsx)
library(dplyr)

# set directory that contains sub directories of downloads
forest_directory <- "C:/GitHub/TEROS_sensors/HubbardBrook/raw"

# get file list
file_list <- list.files(path = forest_directory,
                        pattern = ".xlsx", 
                        recursive = TRUE, 
                        full.names = TRUE)

# initialize file stats data.frame for the loop
file_stats <- data.frame(subdir = character(),
                         fileName = character(), 
                         sheetStatus = character(),
                         sheetName = character(),
                         noRows = numeric(),
                         noCol = numeric(),
                         stringsAsFactors = FALSE)


# function to that checks excel file for visible and hidden sheets
check_sheets <- function(file) {
  # get values for table
  subdirectory <- dirname(file)
  subdirectory <- gsub(paste0(forest_directory, "/"), "", subdirectory)
  file_name <- basename(file)
  wb <- loadWorkbook(file)
  vis_stat <- sheetVisibility(wb)
  sheet_names <- names(wb)
  # message output in console if 'Metadata' sheet missing
  checkMeta <- sheet_names[vis_stat == "visible"]
  if (!"Metadata" %in% checkMeta) {
    message(paste("Warning: File", file_name, "in", subdirectory, 
                  "does not contain a sheet called 'Metadata'"))}
  # loop through all sheets in each file 
  for (i in seq_along(sheet_names)) {
    sheet_name <- sheet_names[i]
    sheet_status <- vis_stat[i]
    no_rows <- nrow(read.xlsx(file, sheet = sheet_name))
    no_col <- ncol(read.xlsx(file, sheet = sheet_name))
    # global attribution to rbind results from each file outside of function
    file_stats <<- rbind(file_stats, data.frame(subdir = subdirectory,
                                               fileName = file_name,
                                               sheetStatus = sheet_status,
                                               sheetName = sheet_name,
                                               noRows = no_rows,
                                               noCol = no_col))}
}

# loop through all files in directory, check sheets in each file
for (file in file_list) {check_sheets(file)}

# hidden vs visible sheets
hidden_sheets <- file_stats %>% filter(sheetStatus == "hidden")
visible_sheets <- file_stats %>% filter(sheetStatus == "visible")

# filter for files with more than one "processed data config" sheet
visible_sheets_extra <- file_stats %>% filter(sheetStatus == "visible") %>% 
  group_by(fileName) %>% 
  filter(n() > 2) %>%
  ungroup()

# which files have extra sheets?
extras <- visible_sheets_extra %>% group_by(fileName) %>% summarize(totalSheets=length(unique(sheetName)))

# are the metadata sheets all formatted the same?
meta_cols <- file_stats %>% filter(sheetName == "Metadata")

# extract file names for files that contain extra sheets
extra_files <- extras %>% pull(fileName)

# what are the data that are in the extra sheets?
file_list <- list.files(path = forest_directory,
                        pattern = ".xlsx", recursive = TRUE) 
file_list <- basename(file_list)

# for files in extra_files, what are the column names? headers?
test1 <- read_excel(paste0(forest_directory,"/",file_list[16]), sheet = "Processed Data Configuration 1", col_names = FALSE, skip = 3)
test2 <- read_excel(paste0(forest_directory,"/",file_list[16]), sheet = "Processed Data Configuration 2", col_names = FALSE, skip = 3)
test3 <- read_excel(paste0(forest_directory,"/",file_list[16]), sheet = "Processed Data Configuration 3", col_names = FALSE, skip = 3)

paste0(forest_directory,"/",file_list[16])
difference <- setdiff(test1[,1], test2[,1])

# range
min(test1[,1])
extra_files
