# Initialise paths ####
# Copy paste at beginning of every R script
# Edit as appropriate

cache.path <- file.path(".", "Data")
stage.path <- file.path("~/Data/ratings-migration")
source.path <- file.path("~/Data/DataLake/Fitch/Sovereign Ratings History/data")

library(googledrive)
drive_auth()

#*********************#

# Get raw files ####

## List extant data files
(dataset.files <- source.path %>% drive_ls())

## Download input data file

data.file <- "data00_sanitised rating actions.rds" # Select file for download
data.id <- file.path(source.path, data.file) %>% drive_get()
local.path <- file.path(cache.path, data.file)
drive_download(data.id, local.path, overwrite = TRUE)