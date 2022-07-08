# data updated post qc

review <- read.csv("data_clean/full_text_review_results.csv")

unique(review$include_exclude)

full_text <- subset(review, include_exclude != "exclude" & include_exclude != "N")
length(unique(full_text$ID))

# qc disagreed with these being excluded
pro_48 <- subset(review, ID == "pro_48")
chl_14 <- subset(review, ID == "chl_14")
pro_42 <- subset(review, ID == "pro_42")
chl_13 <- subset(review, ID == "chl_13")
chl_70 <- subset(review, ID == "chl_70")
pro_5 <- subset(review, ID == "pro_5")

analysis_dat <- rbind(full_text, pro_48, pro_42, chl_13, chl_70, pro_5)
length(unique(analysis_dat$ID))

write.csv(analysis_dat, "data_clean/analysis_dat.csv", row.names = FALSE)

# content coding qc

unique(analysis_dat$connectivity_type)
unique(analysis_dat$connectivity_measure)

analysis_dat %>%
  group_by(connectivity_type) %>%
  summarise(count = n_distinct(ID))

check <- subset(analysis_dat, connectivity_type == "d" | connectivity_type == "f" | connectivity_type == "g"|
                  is.na(connectivity_type))

# round two are in wrong columns, plus pro_42 is NA
# pro_42 should be 0 & g

check[6,2] <- "g"
check[6,3] <- "0"
check[6,4] <- "n"
check[6,5] <- "n"

colnames(check)[2] <- "connectivity_measure"
colnames(check)[3] <- "connectivity_type"

exclude = check$ID
analysis_dat_qc = analysis_dat[!(analysis_dat$ID %in% exclude), ]

analysis_dat_qc <- rbind(analysis_dat_qc, check)

write.csv(analysis_dat_qc, "data_clean/analysis_dat.csv", row.names = FALSE)
