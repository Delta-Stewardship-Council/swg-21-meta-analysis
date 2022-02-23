# load data

DC <- read.csv("data_raw/dc_assignments_productivity search.csv")
MB <- read.csv("data_raw/MB_assignments_productivity_search_split.csv")
PG <- read.csv("data_raw/PG_assignments_productivity_search_split_results.csv")
LY <- read.csv("data_raw/LY_assignments_productivity_search_split.csv")
CP <- read.csv("data_raw/CP_assignments_productivity_search_split_reviewed.csv")
ES <- read.csv("data_raw/ES_assignments_productivity_search_split.csv")
RP <- read.csv("data_raw/RP_Productivity_abstracts_results.csv")

# actual reviewers were different than assigned for RP
RP$reviewer_name <- c("MB", "MB", "MB", "MB", "PG", "PG", "PG", "LY", "LY", "LY", "ES", "ES", "ES")

# add in decisions/changes made in 1/28/22 meeting
MB$inclusion <- ifelse(MB$ID == 53, "n", MB$inclusion)

CP$inclusion <- ifelse(CP$ID == 51, "y",
                                          ifelse(CP$ID == 65, "n",
                                                 ifelse(CP$ID == 13, "n",
                                                        CP$inclusion)))

LY$inclusion <- ifelse(LY$ID == 52, "y", LY$inclusion)

DC$inclusion <- ifelse(DC$ID == 59, "y", DC$inclusion)

# master file for primary productivity results
colnames(MB)[20] <- "reason"
colnames(LY)[20] <- "reason"
colnames(ES)[20] <- "reason"
DC <- DC[,-2]
DC$reason <- "NA"
pro_results <- rbind(DC, MB, PG, LY, CP, ES, RP)

write.csv(pro_results, "data_clean/pro_results.csv")
