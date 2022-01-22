# load data

DC <- read.csv("data_raw/dc_assignments_productivity search.csv")
MB <- read.csv("data_raw/MB_assignments_productivity_search_split.csv")
PG <- read.csv("data_raw/PG_assignments_productivity_search_split_results.csv")
LY <- read.csv("data_raw/LY_assignments_productivity_search_split.csv")
CP <- read.csv("data_raw/CP_assignments_productivity_search_split_reviewed.csv")
ES <- read.csv("data_raw/ES_assignments_productivity_search_split.csv")
# need to add RP later

# rename columns
colnames(DC)[19] <- "result_DC"
colnames(MB)[18] <- "result_MB" # "unsure"
colnames(PG)[18] <- "result_PG"
colnames(LY)[18] <- "result_LY" # "unsure"
colnames(CP)[18] <- "result_CP" # "n*" "u"
colnames(ES)[18] <- "result_ES"
#colnames(RP)[17] <- "result_RP"


# looked at notes and simplified, but we can back track this at our next meeting
MB$result_MB <- ifelse(MB$result_MB == "unsure", "n", MB$result_MB)
LY$result_LY <- ifelse(LY$result_LY == "unsure", "n", LY$result_LY)
CP$result_CP <- ifelse(CP$result_CP == "n*", "n",
                       ifelse(CP$result_CP == "u", "n",
                              CP$result_CP))
# merge

compare_abstracts_pro <- merge(merge(merge(merge(merge(MB[,c(10,18)], DC[,c(11,19)], by='ID', all=T), PG[,c(10,18)],  by = "ID", all=TRUE), LY[,c(10,18)],  by = "ID", all=TRUE), CP[,c(10,18)], by = "ID", all=TRUE), ES[,c(10,18)], by = "ID", all=TRUE)

write.csv(compare_abstracts_pro, "data_clean/abstract_results_productivity.csv")
