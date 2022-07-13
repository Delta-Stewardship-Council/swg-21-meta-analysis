# integrate final search (2022-06-30)

# library
library(readxl)
library(dplyr)

# data
AFSA_chla <- read_excel("data_raw/AFSA_chla_search_results_2022-06-30.xls")
AFSA_pro <- read_excel("data_raw/AFSA_productivity_search_results_2022-06-30.xls")
Ag_chla <- read_excel("data_raw/AgEnvironmenalSci_chla_search_results_2022-06-30.xls")
Ag_pro <- read_excel("data_raw/AgEnvironmentalSci_productivity_search_results_2022-06-30.xls")
Bio_chla <- read_excel("data_raw/BiosisPreview_chla_search_results_2022-06-30.xls")
Bio_pro <- read_excel("data_raw/BiosisPreview_productivity_search_results_2022-06-30.xls")
Geo_chla <- read_excel("data_raw/GeoRef_chla_search_results_2022-06-30.xls")
Geo_pro <- read_excel("data_raw/GeoRef_productivity_search_results_2022-06-30.xls")
Zoo_chla <- read_excel("data_raw/ZooRec_chla_search_results_2022-06-30.xls")
Zoo_pro <- read_excel("data_raw/ZooRec_productivity_search_results_2022-06-30.xls")

# add db and search

unique(AFSA_chla$Database)
unique(AFSA_pro$Database)
unique(Ag_chla$Database)# 7 different variations
Ag_chla$Database_unique <- "Agricultural & Environmental Science Collection"
unique(Ag_pro$Database)# 9
Ag_pro$Database_unique <- "Agricultural & Environmental Science Collection"
Bio_chla$Database_unique <- "BiosisPreview"
Bio_pro$Database_unique <- "BiosisPreview"
unique(Geo_chla$Database)
unique(Geo_pro$Database)
Zoo_chla$Database_unique <- "ZooRec"
Zoo_pro$Database_unique <- "ZooRec"

AFSA_chla$Database_unique <- AFSA_chla$Database
AFSA_pro$Database_unique <- AFSA_pro$Database
Geo_chla$Database_unique <- Geo_chla$Database
Geo_pro$Database_unique <- Geo_pro$Database

AFSA_chla$search <- "chla"
AFSA_pro$search <- "pro"

Ag_chla$search <- "chla"
Ag_pro$search <- "pro"

Bio_chla$search <- "chla"
Bio_pro$search <- "pro"

Geo_chla$search <- "chla"
Geo_pro$search <- "pro"

Zoo_chla$search <- "chla"
Zoo_pro$search <- "pro"

all_titles <- rbind(AFSA_chla[,c(1,2,8,28,47,48)], AFSA_pro[,c(1,2,7,26,43,44)], Ag_chla[,c(1,2,8,33,55,56)], Ag_pro[,c(1,2,7,31,53,54)], Geo_chla[,c(1,2,7,23,37,38)], Geo_pro[,c(1,2,7,23,37,38)])

# column headers vary
more <- rbind(Bio_chla[,c(2,10,35,34,59,60)], Bio_pro[,c(2,10,35,34,59,60)], Zoo_chla[,c(2,10,35,34,59,60)], Zoo_pro[,c(2,10,35,34,59,60)])

colnames(all_titles)
colnames(more)<- c("Authors", "Title", "Abstract", "year", "Database_unique", "search")

all_titles <- rbind(all_titles, more) #465

all_titles %>%
  group_by(Database_unique) %>%
  summarise(count = n())

write.csv(all_titles, "data_clean/all_titles.csv", row.names = FALSE)

# now work on removing duplicates and pulling in IDs
all_titles <- all_titles %>%
  # make title lower case
  mutate(Title = tolower(Title),
         # strip white spaces at end/beginnings all columns
         across(.cols=everything(), ~str_trim(.)))

all_titles$Title <- str_remove(all_titles$Title, "\\.$") # remove end periods
all_titles$Title <- gsub(pattern = "</sub>|<//sub>", "", all_titles$Title) # remove special html for subscript

# remove duplicates
titles_dup_removed <- all_titles[!duplicated(all_titles$Title),]
dim(titles_dup_removed) #197
table(titles_dup_removed$Database_unique)
#Agricultural & Environmental Science Collection - 41
#ASFA: Aquatic Sciences and Fisheries Abstracts - 77
#BiosisPreview - 66
#GeoRef - 10
#ZooRec - 3

# requires some manual qc
write.csv(titles_dup_removed, "titles_dup_removed.csv")

# an additional 13 found in qc
titles_dup_removed <- read.csv("titles_dup_removed.csv")
titles_dup_removed_qc <- subset(titles_dup_removed, qc != "dup") # 184

# get IDs
abstract_results_all <- read_csv("data_clean/abstract_results_all.csv")
length(unique(fin_abs$ID)) # 156

R2 <- read.csv("data_raw/MB_abstract_review_Round2.csv") #7

reviewed_titles <- rbind(abstract_results_all[,c(2,19)], R2[,c(1,2)])
reviewed_titles <- unique(reviewed_titles)
colnames(titles_dup_removed_qc)[2] <- "title"
complete_title_db <- merge(reviewed_titles, titles_dup_removed_qc, by = "title", all = TRUE)

# more manual qc
write.csv(complete_title_db, "data_clean/complete_title_db.csv")

complete_title_db <- read.csv("data_clean/complete_title_db.csv")
complete_title_db_qc <- subset(complete_title_db, ID != "dup") # 187

write.csv(complete_title_db_qc, "data_clean/complete_title_db_qc.csv", row.names = FALSE)
