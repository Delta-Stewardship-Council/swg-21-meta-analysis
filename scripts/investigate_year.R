# investigate year

# library
library(dplyr)
library(tidyr)
library(tidyverse)
# data
analysis_dat_fin <- read.csv("data_clean/analysis_dat.csv")

unique(analysis_dat_fin$year)

# error
# pro_48 - June 2016, September 2017 (qc for content coding)
analysis_dat_fin[97,15] <- "2016, 2017"
# chl_70 is NA
full_text_review_assignments <- read_csv("data_clean/full_text_review_assignments.csv")
chl_70 <- subset(full_text_review_assignments, ID == "chl_70")
analysis_dat_fin$year <- ifelse(is.na(analysis_dat_fin$year), "not reported", analysis_dat_fin$year)

# deal with commas

# deal with dashes
library(unglue)

##

year_full_text <- analysis_dat_fin %>%
  separate_rows(year, sep = ',')

year_df <- year_full_text[,c(1,15)]

year_expand <- unglue_unnest(year_df, year, "{start}-{end}")

year_expand <- na.omit(year_expand)

year_expand <- year_expand %>%
  transmute(ID, year = map2(start, end, `:`)) %>%
  unnest(cols = c(year))

# put it together
ID_expand <- unique(year_expand$ID)

year_df_all <- year_df[ ! year_df$ID %in% ID_expand, ]
year_df_all$year <- as.numeric(year_df_all$year)
hist(year_df_all$year)

year_df_all <- na.omit(year_df_all) # drops 4 without collection year info

# get publication year
abstract_results_all <- read_csv("data_clean/abstract_results_all.csv")
pub_yr <- abstract_results_all[,c(6,19)]
head(pub_yr)

year_df_pub <- merge(year_df_all, pub_yr, by = "ID", all = TRUE)
str(year_df_pub)

year_df_pub <- year_df_pub[!is.na(year_df_pub$year),]

# missing publication year
mis_pub_year <- subset(year_df_pub, is.na(pub_year))
length(unique(mis_pub_year$ID)) #16

# mostly round two
MB_abstract_review_Round2 <- read_csv("data_raw/MB_abstract_review_Round2.csv")

mis_year <- MB_abstract_review_Round2[,c(1,6)]
ID_missing <- c("chl_18", "chl_20", "chl_27", "chl_33", "chl_41", "chl_44", "chl_47", "chl_54", "chl_60", "chl_73", "chl_79")
year_missing <- full_text_review_assignments[ full_text_review_assignments$ID %in% ID_missing, ]

mis_year <- rbind(mis_year, year_missing[,4:5])

year_df_pub <- merge(year_df_pub, mis_year, by = "ID", all.x = TRUE)

year_df_pub$pub_year.x <- ifelse(is.na(year_df_pub$pub_year.x), year_df_pub$pub_year.y, year_df_pub$pub_year.x)

# plots

ggplot(year_df_pub, aes(year, fill = pub_year.x)) +
  geom_histogram(position = "stack", binwidth=2)

plot(year_df_pub$pub_year.x, year_df_pub$year)

hist(year_df_pub$pub_year.x, main = "", xlab = "Year Published", ylab = "Number of Articles")

# add connectivity type (from connectivity_summary.R)
new_dat <- merge(test_dat[,1:2], year_df_pub, by = "ID", all.y = TRUE)
unique(new_dat$connectivity_type)

pch <- c(0,1,2,6)
connectivity_type <- c("1", "2", "3", "0")

pch_ct <- data.frame(pch, connectivity_type)
new_dat <- merge(new_dat, pch_ct, by = "connectivity_type")

plot(jitter(new_dat$pub_year.x, amount = 2), jitter(new_dat$year, amount = 2), pch = new_dat$pch, cex = 1.5, xlab = "Publication Year", ylab = "Study Year")
legend("topleft", c("longitudinal", "lateral", "hyporheic", "none"),
       pch = c(0,1,2,6))
