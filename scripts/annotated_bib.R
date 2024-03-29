# load results from abstract review of primary productivity search

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

# need to flag consistency checks
DC$reviewer_name <- ifelse(DC$reviewer_name != "DC", "check", DC$reviewer_name)
CP$reviewer_name <- ifelse(CP$reviewer_name != "CP", "check", CP$reviewer_name)
MB$reviewer_name <- ifelse(MB$reviewer_name != "MB", "check", MB$reviewer_name)
ES$reviewer_name <- ifelse(ES$reviewer_name != "ES", "check", ES$reviewer_name)
LY$reviewer_name <- ifelse(LY$reviewer_name != "LY", "check", LY$reviewer_name)
PG$reviewer_name <- ifelse(PG$reviewer_name != "PG", "check", PG$reviewer_name)

# master file for primary productivity results
colnames(MB)[20] <- "reason"
colnames(LY)[20] <- "reason"
colnames(ES)[20] <- "reason"
DC <- DC[,-2]
DC$reason <- "NA"

pro_results <- rbind(DC, MB, PG, LY, CP, ES, RP)

pro_results <- pro_results[,-1]
pro_results$ID <- sub("^", "pro_", pro_results$ID)

write.csv(pro_results, "data_clean/pro_results.csv")

# load results from abstract review of chl-a search
DC <- read.csv("data_raw/DC_assignments_results.csv")
MB <- read.csv("data_raw/MB_assignments_split_reviewed.csv")
PG <- read.csv("data_raw/PG_assignments_results.csv")
LY <- read.csv("data_raw/LY_assignments_results.csv")

CP <- read.csv("data_raw/Abstracts_cp.csv")
ES <- read.csv("data_raw/ES_assignments_results.csv")
RP <- read.csv("data_raw/RP_assignments_split.csv")

# CP and LY reformatted table
CP <- CP[,-10]
CP$abstract <- "NA"
CP$authors <- "NA"
CP$article_type <- "NA"
CP$pub_year <- "NA"
CP$volume <- "NA"
CP$database_id  <- "NA"
CP$database_type <- "NA"
CP$consistency_check <- "NA"

for( i in 1:ncol(CP)){
  colnames(CP)[i] <- tolower(colnames(CP)[i])
}

colnames(CP)[c(1,3:7)] <- c("reviewer_name", "marine_y_n",
                         "water_body_type","original_research_y_n", "response_variable"
                         ,"connectivity_metric")
LY$abstract <- "NA"
LY$authors <- "NA"
LY$article_type <- "NA"
LY$pub_year <- "NA"
LY$volume <- "NA"
LY$database_id  <- "NA"
LY$database_type <- "NA"
LY$consistency_check <- "NA"

for( i in 1:ncol(LY)){
  colnames(LY)[i] <- tolower(colnames(LY)[i])
}

colnames(LY)[c(1,3:7)] <- c("reviewer_name", "marine_y_n",
                            "water_body_type","original_research_y_n", "response_variable"
                            ,"connectivity_metric")

# Dylan has two entries for the same title, looking at notes, 'y' is the final
DC <- DC[-8,]

# change CP's unsure to NO and Liz's unsure to YES (decided at 12/17 meeting)
ES[9, 17] <- "y"
CP[13, 8] <- "n"

# need to flag consistency checks
DC$reviewer_name <- ifelse(DC$reviewer_name != "DC", "check", DC$reviewer_name)
MB$reviewer_name <- ifelse(MB$reviewer_name != "MB", "check", MB$reviewer_name)
ES$reviewer_name <- ifelse(ES$reviewer_name != "ES", "check", ES$reviewer_name)
PG$reviewer_name <- ifelse(PG$reviewer_name != "PG", "check", PG$reviewer_name)

chl_results <- rbind(DC[,-1], MB[,-1], PG[,-1], LY, CP, ES[,-1], RP[,-1])

# master list with unique IDs
all_titles <- read.csv("data_clean/abstract_results_chl.csv")
head(all_titles)
all_titles <- all_titles[,c(2,6)]

chl_results <- merge(chl_results, all_titles, by = "title", all = TRUE)

# missed a duplicated title bc of "</sub>"
chl_results[20,18] <- 16

chl_results$ID <- sub("^", "chl_", chl_results$ID)

# find CP and LY consistency checks

#ID_sub <- chl_results[duplicated(chl_results$ID),]$ID

chl_results$dup <- duplicated(chl_results$ID)
dups <- subset(chl_results, dup == TRUE)
checks <- dups$ID
chl_results_sub <- chl_results[chl_results$ID %in% checks,]
# character values
chl_results_sub[chl_results_sub=='NA'] <- NA
changes <- chl_results_sub[is.na(chl_results_sub$consistency_check),]
changes$ID

chl_results[chl_results=='NA'] <- NA
chl_results$consistency_check <- ifelse(is.na(chl_results$database_type), chl_results$reviewer_name, chl_results$consistency_check)

chl_results$reviewer_name <- ifelse(is.na(chl_results$database_type), "check", chl_results$reviewer_name)

write.csv(chl_results[,-19], "data_clean/chl_results.csv")

# pull them together and add decision column
# also need to find 'reason' for all
chl_reason <- read.csv("data_raw/abstract_reason_chl.csv")
head(chl_reason)
chl_reason <- chl_reason[,-1]
colnames(chl_reason)[3] <- "ID"
reason <- chl_reason$ID

# need to add with missing reason from DC and deal with disagreement duplication
chl_reason_sub <- chl_results[chl_results$ID %in% reason,]
new_reason <- c("marine","connectivity","type","NA","can I convert this to a yes?","connectivity","type","type","NA","connectivity","marine","connectivity","connectivity","type (but I made this decision after reviewing full text (can I convert it to a yes?)","NA","marine","marine","type","connectivity","marine; type", "connectivity","marine","marine","type","NA","type","connectivity")

chl_reason_sub <- cbind(chl_reason_sub, new_reason)

# Liz included notes about changing to yes in the reason file
chl_reason_sub[5,17] <- "y"
chl_reason_sub[14,17] <- "y"

chl_reason_main <- chl_results[!chl_results$ID %in% reason,]
chl_reason_main$new_reason <- "NA"
chl_results_w_reason <- rbind(chl_reason_main, chl_reason_sub)
colnames(chl_results_w_reason)[20] <- "reason"
full_results <- rbind(chl_results_w_reason[,-1], pro_results)

write.csv(full_results, "data_clean/abstract_results_all.csv")

# subset yes inclusion
