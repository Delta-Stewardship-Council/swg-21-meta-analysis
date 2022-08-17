#use analysis_dat_updated (created in methods to add round 3 data, and includes edits made in connectivity_summary.R)
library(ggplot2)
library(tidyr)
library(dplyr)

analysis_dat_updated <- read.csv("data_clean/analysis_dat_updated.csv", check.names = FALSE, fileEncoding = "Latin1")

unique(analysis_dat_updated$connectivity_type)
unique(analysis_dat_updated$connectivity_measure)
unique(analysis_dat_updated$seasonal_code)
unique(analysis_dat_updated$repeats_code)

length(unique(analysis_dat_updated$ID))

analysis_dat_updated$seasonal_code <- tolower(analysis_dat_updated$seasonal_code)
analysis_dat_updated$repeats_code <- tolower(analysis_dat_updated$repeats_code)

analysis_dat_updated$seasonal_code <- ifelse( is.na(analysis_dat_updated$seasonal_code), "n",
                                              analysis_dat_updated$seasonal_code)
analysis_dat_updated$repeats_code <- ifelse( is.na(analysis_dat_updated$repeats_code), "n",
                                              analysis_dat_updated$repeats_code)

# something happened to the commas in the connectivity_type
analysis_dat_updated$connectivity_type <- ifelse(analysis_dat_updated$connectivity_type == "12", "1, 2", analysis_dat_updated$connectivity_type)
analysis_dat_updated$connectivity_type <- ifelse(analysis_dat_updated$connectivity_type == "13", "1,3", analysis_dat_updated$connectivity_type)
analysis_dat_updated$connectivity_type <- ifelse(analysis_dat_updated$connectivity_type == "123", "1, 2, 3", analysis_dat_updated$connectivity_type)
analysis_dat_updated$connectivity_type <- ifelse(analysis_dat_updated$connectivity_type == "23", "2, 3", analysis_dat_updated$connectivity_type)

write.csv(analysis_dat_updated, "data_clean/analysis_dat_updated.csv", row.names = FALSE)

write.csv(analysis_dat_updated[,c(1:5)], "data_clean/content_coding_results.csv", row.names = FALSE)

# plots and summary
cc_dat <- analysis_dat_updated[,c(1:5)]

dat4plot <- cc_dat %>%
  group_by(connectivity_type, connectivity_measure, seasonal_code, repeats_code) %>%
  summarise(count = n_distinct(ID), .groups = 'drop')

# break up unique combinations
cc_dat$connectivity_measure <- gsub("[[:space:]]", "", cc_dat$connectivity_measure)
cc_dat$connectivity_type <- gsub("[[:space:]]", "", cc_dat$connectivity_type)

unique(cc_dat$connectivity_type)

test_dat <- cc_dat %>%
  separate_rows(connectivity_type, sep = ',')

test_dat <- test_dat %>%
  separate_rows(connectivity_measure, sep = ',')

write.csv(test_dat, "data_clean/content_coding_results_decomposed.csv", row.names = FALSE)

dat4combo <- test_dat %>%
  group_by(connectivity_type, connectivity_measure) %>%
  summarise(count = n_distinct(ID), .groups = 'drop')

# now add season and repeats
dat4all <- content_coding_results_decomposed %>%
  group_by(connectivity_type, connectivity_measure, seasonal_code, repeats_code) %>%
  summarise(count = n_distinct(ID), .groups = 'drop')

dat4all$seasonal_code <- ifelse(dat4all$seasonal_code == "y", "season", "none")
dat4all$repeats_code <- ifelse(dat4all$repeats_code == "y", "year", "none")

tiff("content_coding_heatmap.tiff", units = "in", width = 8, height = 11, res = 300)

ggplot(dat4all, aes(x=connectivity_type,y=connectivity_measure, fill=count)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1)+
  scale_fill_gradient(low = "gray94", high = "black") +
  guides(fill = guide_colourbar(barwidth = 2, barheight = 14, title = "# of Articles", ticks = FALSE)) +
  scale_x_discrete(labels=c("none", "longitudinal", "lateral", "hyporheic")) +
  scale_y_discrete(labels=c("distance", "flow", "status", "site",
                            "event", "correlation", "not defined",
                            "none", "salinity", "permafrost")) +
  theme_bw() +
  xlab("Connectivity Type") + ylab("Connectivity Measure") +
  facet_grid(vars(seasonal_code), vars(repeats_code))

dev.off()
