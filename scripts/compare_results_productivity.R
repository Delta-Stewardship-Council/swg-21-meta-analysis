# load data

DC <- read.csv("data_raw/dc_assignments_productivity search.csv")
MB <- read.csv("data_raw/MB_assignments_productivity_search_split.csv")
PG <- read.csv("data_raw/PG_assignments_productivity_search_split_results.csv")
LY <- read.csv("data_raw/LY_assignments_productivity_search_split.csv")
CP <- read.csv("data_raw/CP_assignments_productivity_search_split_reviewed.csv")
ES <- read.csv("data_raw/ES_assignments_productivity_search_split.csv")
RP <- read.csv("data_raw/RP_Productivity_abstracts_results.csv")


# rename columns
colnames(DC)[19] <- "result_DC"
colnames(MB)[18] <- "result_MB" # "unsure"
colnames(PG)[18] <- "result_PG"
colnames(LY)[18] <- "result_LY" # "unsure"
colnames(CP)[18] <- "result_CP" # "n*" "u"
colnames(ES)[18] <- "result_ES"
colnames(RP)[18] <- "result_RP" # "unsure"


# looked at notes and simplified, but we can back track this at our next meeting
MB$result_MB <- ifelse(MB$result_MB == "unsure", "n", MB$result_MB)
LY$result_LY <- ifelse(LY$result_LY == "unsure", "n", LY$result_LY)
CP$result_CP <- ifelse(CP$result_CP == "n*", "n",
                       ifelse(CP$result_CP == "u", "n",
                              CP$result_CP))
#RP$result_RP <- ifelse(RP$result_RP == "unsure", "n", RP$result_RP)

# merge

compare_abstracts_pro <- merge(merge(merge(merge(merge(merge(MB[,c(10,18)], DC[,c(11,19)], by='ID', all=T), PG[,c(10,18)],  by = "ID", all=TRUE), LY[,c(10,18)],  by = "ID", all=TRUE), CP[,c(10,18)], by = "ID", all=TRUE), ES[,c(10,18)], by = "ID", all=TRUE), RP[,c(10,18)], by = "ID", all=TRUE)

write.csv(compare_abstracts_pro, "data_clean/abstract_results_productivity.csv")

# add in decisions/changes made in 1/28/22 meeting
#ID # 59 – Y DC
#ID # 52 - Y LY
#ID # 51 - Y CP
#ID # 65 - N CP
#ID # 13 - N CP
#ID # 53 - N MB

compare_abstracts_pro$result_MB <- ifelse(compare_abstracts_pro$ID == 53, "n", compare_abstracts_pro$result_MB)

compare_abstracts_pro$result_CP <- ifelse(compare_abstracts_pro$ID == 51, "y",
                                          ifelse(compare_abstracts_pro$ID == 65, "n",
                                                 ifelse(compare_abstracts_pro$ID == 13, "n",
                                            compare_abstracts_pro$result_CP)))

compare_abstracts_pro$result_LY <- ifelse(compare_abstracts_pro$ID == 52, "y", compare_abstracts_pro$result_LY)

compare_abstracts_pro$result_DC <- ifelse(compare_abstracts_pro$ID == 59, "y", compare_abstracts_pro$result_DC)

write.csv(compare_abstracts_pro, "data_clean/abstract_results_productivity_update.csv")
