# this script reduces the chl_extraction_all dataset to only papers that reported mean chlorophyll values in the text

#load packages
library(tidyr)
library(dplyr)

#load data
chl_all <- read.csv("data_clean/chl_extraction_all.csv")

#remove articles with NA in the chlorophyll mean column
chl_mean <- chl_all %>%
  drop_na(mean_chl)

#remove articles for which the chlorophyll mean value was approximated from figures
chl_mean <- chl_mean %>%
  filter(ID != "chl_33" & ID != "chl_22" & ID != "chl_41" & ID != "chl_68")

#save mean chlorophyll data set
write.csv(chl_mean, "data_clean/chl_means.csv")
