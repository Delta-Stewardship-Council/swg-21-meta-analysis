#load packages
library(dplyr)

#load data
chl_group <- read.csv("data_raw/chl_extraction_spreadsheet.csv")
chl_dc <- read.csv("data_raw/dc_meta_chladata.csv")
chl_es <- read.csv("data_raw/ES_chl_extraction_spreadsheet_060322.csv")

#check columns
colnames(chl_group)
colnames(chl_dc) #extra column "in.figure", reviewer column is blank
colnames(chl_es)

#populate chl_dc$review with reviewer initials
chl_dc$reviewer <- "DC"

#remove in.figure column in chl_dc to allow binding
##confirmed that the information in in.figure column is already represented within the notes column so no information is lost
chl_dc <- chl_dc %>%
  select(-in.figure)

#merge data sets
chl_all <- rbind(chl_group, chl_dc, chl_es)

#save integrated data set
write.csv(chl_all, "data_clean/chl_extraction_all.csv")
