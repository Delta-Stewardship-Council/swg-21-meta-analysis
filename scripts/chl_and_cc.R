#load packages
library(dplyr)
library(ggplot2)

#read in data
chl <- read.csv("data_clean/chl_means_categorical_units.csv")
cc <- read.csv("data_clean/content_coding_results.csv")
cc_dec <- read.csv("data_clean/content_coding_results_decomposed.csv")

#merge datasets
cc_chl <- merge(chl, cc, by = "ID", all.x=TRUE)
cc_dec_chl <- merge(chl, cc_dec, by = "ID", all.x=TRUE)

boxplot(mean_chl ~ connectivity_type, data=cc_chl)
boxplot(mean_chl ~ connectivity_measure, data=cc_chl)

ggplot(data=cc_chl, aes(mean_chl, connectivity_measure)) +
  geom_boxplot() +
  facet_wrap(vars(connectivity_type))

dat_g <- subset(cc_chl, connectivity_measure == "g")
unique(dat_g[,c(1,20)])

dat_g <- subset(cc_chl, connectivity_measure == "g")


cc_dec_chl_simple <- subset(cc_dec_chl, connectivity_type != 0 & connectivity_type != 3)

unique(cc_dec_chl_simple$connect_binary)

ggplot(data=cc_dec_chl_simple, aes(mean_chl, connectivity_measure)) +
  geom_boxplot() +
  facet_grid(vars(connectivity_type),vars(connect_binary))

unique(cc_dec_chl_simple$connect_binary)

cc_dec_chl_simple$connect_binary <- ifelse(is.na(cc_dec_chl_simple$connect_binary), "unclear", cc_dec_chl_simple$connect_binary)

cc_dec_chl_simple_g <- subset(cc_dec_chl_simple, connectivity_measure != "g")

cc_dec_chl_simple_g$connectivity_type <- ifelse(cc_dec_chl_simple_g$connectivity_type == 1, "longitudinal",
                                                ifelse(cc_dec_chl_simple_g$connectivity_type ==2, "lateral",
                                                       cc_dec_chl_simple_g$connectivity_type))

ggplot(data=cc_dec_chl_simple_g, aes(mean_chl, connectivity_measure)) +
  geom_boxplot() +
  scale_y_discrete(labels=c("distance", "flow", "status", "site",
                            "event", "correlation", "not defined",
                            "none", "salinity")) +
  theme_bw() +
  xlab("Mean Chlorophyll") + ylab("Connectivity Measure") +
  facet_grid(vars(connectivity_type),vars(connect_binary))

# add locations
chl_ID <- unique(chl$ID)
analysis_dat <- read.csv("data_clean/analysis_dat_updated.csv")
analysis_dat_chl <- analysis_dat[analysis_dat$ID %in% chl_ID,]

# four did not have enough detail
title_ID <- c("chl_32", "r2_02", "r3_05", "r3_23")
titles <- read.csv("data_clean/complete_title_db_qc.csv")
titles_chl <- titles[titles$ID %in% title_ID,]

#chl_36 is the only one that varied across locations
chl_36 <- subset(chl, ID == "chl_36")
