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

unique(analysis_dat_qc$connectivity_type)
unique(analysis_dat_qc$connectivity_measure)
unique(analysis_dat_qc$seasonal_code)
unique(analysis_dat_qc$repeats_code)

# more consistency work
analysis_dat_qc$connectivity_type <- ifelse(analysis_dat_qc$connectivity_type == "lateral", "2",
                                       ifelse(analysis_dat_qc$connectivity_type == "lateral, longitudinal", "1, 2",
                                              ifelse(analysis_dat_qc$connectivity_type == "longitudinal, hyporheic", "1, 3",
                                                     analysis_dat_qc$connectivity_type)))

analysis_dat_qc$connectivity_measure <- tolower(analysis_dat_qc$connectivity_measure)

# need to be alphabetical with space, "d,a" "e,d"   "d, c, a" "d, c"
analysis_dat_qc$connectivity_measure <- ifelse(analysis_dat_qc$connectivity_measure == "d,a", "a, d",
                                               ifelse(analysis_dat_qc$connectivity_measure == "e,d", "d, e",
                                                      ifelse(analysis_dat_qc$connectivity_measure == "d, c, a", "a, c, d",
                                                             ifelse(analysis_dat_qc$connectivity_measure == "d, c", "c, d",
                                                                    analysis_dat_qc$connectivity_measure))))

# chl_27, pro_48 have NA
full_text_review_assignments <- read_csv("data_clean/full_text_review_assignments.csv")
titles_measure <- subset(full_text_review_assignments, ID == "chl_27" | ID == "pro_48")
analysis_measure <- subset(analysis_dat_qc, ID == "chl_27" | ID == "pro_48")
# chl_27 - "winter of 2007 (February and March), Connectivity (categorical data) (changes of longitudinal connectivity at the site related to the presence of weirs, culverts or other)." should be 1, d, n, n
# pro_48 - June 2016, September 2017, connectivity is site based, 3, d, n, n

analysis_measure[1, 2] <- "1"
analysis_measure[1, 3] <- "d"
analysis_measure[1, 4] <- "n"
analysis_measure[1, 5] <- "n"

analysis_measure[2, 2] <- "3"
analysis_measure[2, 3] <- "d"
analysis_measure[2, 4] <- "n"
analysis_measure[2, 5] <- "n"

measure_issue = analysis_measure$ID
analysis_dat_qc = analysis_dat[!(analysis_dat$ID %in% measure_issue), ]

analysis_dat_qc <- rbind(analysis_dat_qc, analysis_measure)

#
analysis_dat_qc$seasonal_code <- tolower(analysis_dat_qc$seasonal_code)
analysis_dat_qc$repeats_code <- tolower(analysis_dat_qc$repeats_code)

analysis_dat_qc$seasonal_code <- ifelse(analysis_dat_qc$seasonal_code == "yes", "y",
                                   ifelse(analysis_dat_qc$seasonal_code == "no", "n",
                                          analysis_dat_qc$seasonal_code))

analysis_dat_qc$repeats_code <- ifelse(analysis_dat_qc$repeats_code == "yes", "y",
                                  ifelse(analysis_dat_qc$repeats_code == "no", "n",
                                         analysis_dat_qc$repeats_code))

# chl_74, chl_8, chl_44 - changing to n

analysis_dat_qc$repeats_code <- ifelse(analysis_dat_qc$repeats_code == "na" | is.na(analysis_dat_qc$repeats_code), "n",
                                              analysis_dat_qc$repeats_code)

analysis_dat_qc$seasonal_code <- ifelse(analysis_dat_qc$seasonal_code == "na" | is.na(analysis_dat_qc$seasonal_code), "n",
                                       analysis_dat_qc$seasonal_code)

# notes included in same cell as results
notes <- subset(analysis_dat_qc, repeats_code != "n" & repeats_code != "y")

notes[1, 5] <- "y"
notes[2, 5] <- "y"
notes[3, 5] <- "y"
notes[4, 5] <- "n"
notes[5, 5] <- "y"


notes_ID = notes$ID
analysis_dat_fin = analysis_dat_qc[!(analysis_dat_qc$ID %in% notes_ID), ]

analysis_dat_fin <- rbind(analysis_dat_fin, notes)

unique(analysis_dat_fin$connectivity_type)
unique(analysis_dat_fin$connectivity_measure)
unique(analysis_dat_fin$seasonal_code)
unique(analysis_dat_fin$repeats_code)

write.csv(analysis_dat_fin, "data_clean/analysis_dat.csv", row.names = FALSE)

# heat map
## seasonal_code, repeats_code will be stated inside the square

dat4plot <- analysis_dat_qc %>%
  group_by(connectivity_type, connectivity_measure) %>%
  summarise(count = n_distinct(ID))


library(ggplot2)

ggplot(df, aes(x=Price,y=Category, fill=No.Apps)) +
  geom_tile()+
  scale_fill_gradientn(colours=rev(heat.colors(10)))+
  scale_x_discrete(expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))+
  coord_fixed()
