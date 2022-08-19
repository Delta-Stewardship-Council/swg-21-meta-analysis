##this script integrates the chlorophyll extraction files from different reviewers into one data set and writes the integrated data set to the file "chl_extraction_all.csv"

#load packages
library(dplyr)
library(readxl)

#load data
chl_group <- read.csv("data_raw/chl_extraction_spreadsheet.csv")
chl_dc <- read.csv("data_raw/dc_meta_chladata.csv")
chl_es <- read.csv("data_raw/ES_chl_extraction_spreadsheet_060322.csv")
chl_mb3 <- read.csv("data_raw/chl_extraction_r3_MB.csv")
chl_pg3 <- read_excel("data_raw/r3_full_text_pg.xlsx", sheet = "chl")

#check columns
colnames(chl_group)
colnames(chl_dc) #extra column "in.figure", reviewer column is blank
colnames(chl_es)
colnames(chl_mb3)
colnames(chl_pg3) #columns have same names, but are in a different order. This is not an issue for rbind, so no change needed.

#populate chl_dc$review with reviewer initials
chl_dc$reviewer <- "DC"

#remove in.figure column in chl_dc to allow binding
##confirmed that the information in in.figure column is already represented within the notes column so no information is lost
chl_dc <- chl_dc %>%
  select(-in.figure)

#merge data sets
chl_all <- rbind(chl_group, chl_dc, chl_es, chl_mb3, chl_pg3)

#save integrated data set
write.csv(chl_all, "data_clean/chl_extraction_all.csv")
