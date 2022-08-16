# need to remove chl_47 from results data, marine

library(readr)

analysis_dat_updated <- read_csv("data_clean/analysis_dat_updated.csv")
summary_pub_year <- read_csv("data_clean/summary_pub_year.csv")
content_coding_results_decomposed <- read_csv("data_clean/content_coding_results_decomposed.csv")
content_coding_results <- read_csv("data_clean/content_coding_results.csv")
year_dat <- read_csv("data_clean/year_dat.csv")


analysis_dat_updated_47_rm <- subset(analysis_dat_updated, ID != "chl_47")
summary_pub_year_47_rm <- subset(summary_pub_year, ID != "chl_47")
content_coding_results_decomposed_47_rm <- subset(content_coding_results_decomposed, ID != "chl_47")
content_coding_results_47_rm <- subset(content_coding_results, ID != "chl_47")
year_dat_47_rm <- subset(year_dat, ID != "chl_47")

write.csv(analysis_dat_updated_47_rm, "data_clean/analysis_dat_updated.csv")
write.csv(summary_pub_year_47_rm, "data_clean/summary_pub_year.csv")
write.csv(content_coding_results_decomposed_47_rm, "data_clean/content_coding_results_decomposed.csv")
write.csv(content_coding_results_47_rm, "data_clean/content_coding_results.csv")
write.csv(year_dat_47_rm, "data_clean/year_dat.csv")

