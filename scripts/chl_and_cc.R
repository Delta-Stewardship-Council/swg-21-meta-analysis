#load packages
library(dplyr)
library(ggplot2)

#read in data
chl <- read.csv("data_clean/chl_means_categorical_units.csv")
cc <- read.csv("data_clean/content_coding_results.csv")

#merge datasets
cc_chl <- merge(chl, cc, by = "ID", all.x=TRUE)

boxplot(mean_chl ~ connectivity_type, data=cc_chl)
boxplot(mean_chl ~ connectivity_measure, data=cc_chl)

ggplot(data=cc_chl, aes(mean_chl, connectivity_measure)) +
  geom_boxplot() +
  facet_wrap(vars(connectivity_type))

dat_g <- subset(cc_chl, connectivity_measure == "g")
unique(dat_g[,c(1,20)])
