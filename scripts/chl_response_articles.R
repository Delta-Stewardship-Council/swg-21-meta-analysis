# look for articles with responses
#library
library(dplyr)

# data
full_text_review_qc_update <- read.csv("data_clean/full_text_review_results_update.csv", check.names = FALSE, fileEncoding = "Latin1")

analysis_dat_updated <- read.csv("data_clean/analysis_dat_updated.csv", check.names = FALSE, fileEncoding = "Latin1")

head(full_text_review_qc_update)

unique(full_text_review_qc_update$chlorophyll_response)
unique(full_text_review_qc_update$ntoes)
unique(full_text_review_qc_update$cc_notes)

dat_response <- full_text_review_qc_update[,c(1,34)]

dat_response_increase <- subset(dat_response, chlorophyll_response == "increase")

# check
chl_36 <- subset(full_text_review_qc_update, ID == "chl_36")

final_ID <- unique(analysis_dat_updated$ID)

dat_response_IDs <- dat_response %>%
  filter(ID %in% final_ID)

write.csv(dat_response_IDs, "data_clean/chl_response.csv")
