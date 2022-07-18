# mb found issue with "journal articles" filter and reran Ag & Environmental Science search

complete_title_db_qc <- read.csv("data_clean/complete_title_db_qc.csv")

head(complete_title_db_qc)

Ag_chla_update <- read_excel("data_raw/AgEnvironmenalSci_chla_search_results_2022-07-18.xls")
Ag_pro_update <- read_excel("data_raw/AgEnvironmentalSci_productivity_search_results_2022-07-18.xls")

Ag_chla_update$Database_unique <- "Agricultural & Environmental Science Collection"

Ag_pro_update$Database_unique <- "Agricultural & Environmental Science Collection"

Ag_chla_update$search <- "chla"
Ag_pro_update$search <- "pro"

head(Ag_chla_update[,c(1,2,8,34,56,57)])
head(Ag_pro_update[,c(1,2,7,33,55,56)]) # adds 13

Ag_update <- rbind(Ag_chla_update[,c(1,2,8,34,56,57)], Ag_pro_update[,c(1,2,7,33,55,56)])


Ag_update <- Ag_update %>%
  # make title lower case
  mutate(Title = tolower(Title),
         # strip white spaces at end/beginnings all columns
         across(.cols=everything(), ~str_trim(.)))

Ag_update$Title <- str_remove(Ag_update$Title, "\\.$") # remove end periods
Ag_update$Title <- gsub(pattern = "</sub>|<//sub>", "", Ag_update$Title)

Ag_dup_removed <- Ag_update[!duplicated(Ag_update$Title),]
dim(Ag_dup_removed) # 68

colnames(Ag_update)[1] <- "title"
ID <- "Ag"
check_Ag <- data.frame(Ag_update[,1], ID)

compare_Ag <- rbind(check_Ag, complete_title_db_qc[,1:2])

write.csv(compare_Ag, "data_raw/deal_w_Ag_search_issue.csv")

# only 4 are new

Ag_titles <- read.csv("data_raw/qc_Ag_search_issue.csv")
colnames(Ag_dup_removed)[1] <- "title"
Ag_titles_fulldetails <- merge(Ag_titles, Ag_dup_removed, by = "title", all.x = TRUE)

complete_list <- rbind(Ag_titles_fulldetails, complete_title_db_qc[,-3])

write.csv(complete_list, "data_clean/complete_title_db_qc_Ag.csv", row.names = FALSE)

# assign abstract review
r3 <- subset(complete_list, grepl('r3', ID) )

name = rep(c("MB", "PG"), length = nrow(r3))
assignments <- cbind(r3, reviewer_name = name)

# get coumn names
abstract_results_all <- read_csv("data_clean/abstract_results_all.csv")
colnames(abstract_results_all)

marine_y_n <- "NA"
water_body_type <- "NA"
original_research_y_n <- "NA"
response_variable <- "NA"
connectivity_metric <- "NA"
inclusion <- "NA"
notes <- "NA"
reason <- "NA"

ab_review <- data.frame(assignments, marine_y_n, water_body_type, original_research_y_n, response_variable, connectivity_metric, inclusion, notes, reason)

write.csv(ab_review, "data_clean/additions_ab_review.csv")
