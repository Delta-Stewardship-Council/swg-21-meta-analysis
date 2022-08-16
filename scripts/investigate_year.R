# investigate year

# library
library(dplyr)
library(tidyr)
library(tidyverse)
# data
analysis_dat_updated <- read.csv("data_clean/analysis_dat_updated.csv")

unique(analysis_dat_updated$year)
length(unique(analysis_dat_updated$ID))

# error
# pro_48 - June 2016, September 2017 (qc for content coding)
#analysis_dat_fin[97,15] <- "2016, 2017"
# chl_70 is NA
#full_text_review_assignments <- read_csv("data_clean/full_text_review_assignments.csv")
#chl_70 <- subset(full_text_review_assignments, ID == "chl_70")

# above fixed, but need to investigate r3_21 (NA)
analysis_dat_updated$year <- ifelse(is.na(analysis_dat_updated$year), "not reported", analysis_dat_updated$year)

# r3_13, year = "20,182,019"
analysis_dat_updated[106,15] <- "2018, 2019"

# deal with commas

# deal with dashes
library(unglue)

##

year_full_text <- analysis_dat_updated %>%
  separate_rows(year, sep = ',')

year_df <- year_full_text[,c(1,15)]

year_expand <- unglue_unnest(year_df, year, "{start}-{end}")

year_expand_na <- na.omit(year_expand)

year_expand_na <- year_expand_na %>%
  transmute(ID, year = map2(start, end, `:`)) %>%
  unnest(cols = c(year))

# put it together
ID_expand <- unique(year_expand_na$ID) #21

year_df_all <- year_df[ ! year_df$ID %in% ID_expand, ]
length(unique(year_df_all$ID)) #98
year_df_all$year <- as.numeric(year_df_all$year)
hist(year_df_all$year)

year_df_all <- na.omit(year_df_all) # 93, drops 5 without collection year info

year_final <- rbind(year_expand_na, year_df_all)
length(unique(year_final$ID)) #114

# get publication year
abstract_results_all <- read_csv("data_clean/abstract_results_all.csv")
pub_yr <- abstract_results_all[,c(6,19)]
head(pub_yr)

# round two
MB_abstract_review_Round2 <- read_csv("data_raw/MB_abstract_review_Round2.csv")
mis_year <- MB_abstract_review_Round2[,c(1,6)]

# round three
additions_ab_review <- read_csv("data_clean/additions_ab_review.csv")

r3_year <- additions_ab_review[,c(3,7)]
colnames(r3_year)[2] <- "pub_year"

pub_year_all <- rbind(pub_yr, mis_year, r3_year)

pub_year_unique <- unique(pub_year_all) # 7 more than there should be because some have NA pub_yr
# r3_03 has pub_year = 1 (should be 2004)

pub_year_unique <- pub_year_unique[!is.na(pub_year_unique$pub_year),]

pub_year_unique <- subset(pub_year_unique, pub_year > 1980)

pub_year_unique[170,1] <- 2004
pub_year_unique[170,2] <- "r3_03"

# pub year and study year
year_df_pub <- merge(year_final, pub_year_unique, by = "ID", all.x = TRUE)
str(year_df_pub)
length(unique(year_df_pub$ID))

# missing publication year
mis_pub_year <- subset(year_df_pub, is.na(pub_year))
length(unique(mis_pub_year$ID)) #13


ID_missing <- unique(mis_pub_year$ID)
full_text_review_assignments <- read_csv("data_clean/full_text_review_assignments.csv")
year_missing <- full_text_review_assignments[ full_text_review_assignments$ID %in% ID_missing, ]

mis_year <- rbind(mis_year, year_missing[,4:5])

year_df_pub_corrected <- merge(year_df_pub, mis_year, by = "ID", all.x = TRUE)

year_df_pub_corrected$pub_year.x <- ifelse(is.na(year_df_pub_corrected$pub_year.x), year_df_pub_corrected$pub_year.y, year_df_pub_corrected$pub_year.x)

length(unique(year_df_pub_corrected$ID))

colnames(year_df_pub_corrected)[3] <- "pub_year"

write.csv(year_df_pub_corrected[,-4], "data_clean/year_dat.csv")

# plots

# publication year
summary_pub_year <- unique(year_df_pub_corrected[,c(2,4)])
# need to add "not reported" back into data

not_reported <- c("pro_51", "chl_74", "chl_70", "chl_25", "r3_21")
year_not_reported <- full_text_review_assignments[ full_text_review_assignments$ID %in% not_reported, ]
year_not_reported <- year_not_reported[,4:5]
year_not_reported[5,1] <- 2019
year_not_reported[5,2] <- "r3_21"

summary_pub_year <- rbind(summary_pub_year, year_not_reported)

write.csv(summary_pub_year, "data_clean/summary_pub_year.csv", row.names = FALSE)

tiff("year_published.tiff", units = "in", width = 8, height = 11, res = 300)

hist(summary_pub_year$pub_year, main = "", xlab = "Year Published", ylab = "Number of Articles", breaks = 25)

dev.off()

num_by_yr <- summary_pub_year %>%
  group_by(pub_year) %>%
  summarise_each(funs(n_distinct))

# both
ggplot(year_df_pub_corrected, aes(year, fill = pub_year)) +
  geom_histogram(position = "stack", binwidth=2)

plot(year_df_pub_corrected$pub_year, year_df_pub$year)

year_df_pub_corrected$pub_year <- as.factor(year_df_pub_corrected$pub_year)

ggplot(year_df_pub_corrected, aes(x=pub_year, y=year)) +
  geom_violin() +
  geom_boxplot(width=0.1)

ggplot(year_df_pub_corrected, aes(x=pub_year, y=year)) +
  geom_violin()+
  geom_jitter(shape=1, position=position_jitter(0.1))

ggplot(year_df_pub_corrected, aes(x=pub_year, y=year)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))+
  geom_jitter(shape=1, position=position_jitter(0.1))

# add connectivity type (from connectivity_summary.R)
year_df_pub_corrected <- read.csv("data_clean/year_dat.csv")
test_dat <- read.csv("data_clean/content_coding_results_decomposed.csv")

new_dat <- merge(test_dat[,1:2], year_df_pub_corrected[,-1], by = "ID", all.y = TRUE)
unique(new_dat$connectivity_type)

pch <- c(0,1,2,6)
connectivity_type <- c("1", "2", "3", "0")

pch_ct <- data.frame(pch, connectivity_type)
new_dat_2 <- merge(new_dat, pch_ct, by = "connectivity_type")

tiff("year_published_vs_year_studied.tiff", units = "in", width = 11, height = 8, res = 300)

plot(jitter(new_dat_2$pub_year, amount = 2), jitter(new_dat_2$year, amount = 2), pch = new_dat_2$pch, cex = 1.5, xlab = "Publication Year", ylab = "Study Year")
legend("topleft", c("longitudinal", "lateral", "hyporheic", "none"),
       pch = c(0,1,2,6))

dev.off()

year_by_year <- new_dat[,c(1,3)] %>%
  group_by(ID) %>%
  summarise_each(funs(n_distinct))

# check, something isn't looking right
chl_11 <- subset(test_dat, ID == "chl_11")

year_by_year <- merge(year_by_year, test_dat, by = "ID")

sd(year_by_year$year)
