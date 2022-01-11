# load packages
library(dplyr)
library(stringr)
library(janitor)
library(readr)

# Import data -------------------------------------------------------------

# load data
pro <- read.csv("data_raw/ProQuest_primaryproduct-2021-12-17.csv") # 94
wos <- read.csv("data_raw/WOS_primaryproduct-2021-12-17.csv") # 63

# Clean Data --------------------------------------------------------------

# Clean titles
pro_titles <- pro %>%
  # make title lower case
  mutate(Title = tolower(Title),
         # strip white spaces at end/beginnings all columns
         across(.cols=everything(), ~str_trim(.)))

wos_titles <- wos %>%
  mutate(Article.Title = tolower(Article.Title),
         # strip white spaces at end/beginnings all columns
         across(.cols=everything(), ~str_trim(.)))

# pull just title, abstract, authors, article type, pubyear, volume, ID
# rename to be consistent
pro_titles_sel <- select(pro_titles,
                         title=Title, abstract=Abstract, authors=Authors,
                         article_type=ArticleType, pub_year=year, volume,
                         database_id = StoreId) %>%
  mutate(database_type = "proquest") # add where data came from

wos_titles_sel <- select(wos_titles,
                         title=Article.Title, abstract=Abstract, authors=Authors,
                         article_type=Publication.Type,
                         pub_year=Publication.Year, volume=Volume,
                         database_id = `UT..Unique.ID.`) %>%
  mutate(database_type="wos") # add where data came from

# bind together
titles_all <- bind_rows(pro_titles_sel, wos_titles_sel)

# one database has periods at the end of titles and one does not, remove
titles_all$title <- str_remove(titles_all$title, "\\.$") # remove end periods
titles_all$title <- gsub(pattern = "<sub>|<//sub>", "", titles_all$title) # remove special html for subscript
#titles_all$title <- gsub("[[:punct:][:blank:]]+", " ", titles_all$title)

# fix the article type column:
titles_all <- titles_all %>%
  mutate(article_type = case_when(
    article_type == "J" ~ "Scholarly Journals",
    article_type == "B" ~ "Books",
    TRUE ~ article_type))

table(titles_all$article_type)

# Remove Duplicates -------------------------------------------------------

# remove duplicates
titles_dup_removed <- titles_all[!duplicated(titles_all$title),]
dim(titles_dup_removed) #94
table(titles_dup_removed$database_type)
# proquest=65, wos=29

#write csv with combined and cleaned search results
write.csv(titles_dup_removed, "productivity_search_all.csv")



# Compare to chlorophyll search
# -----------------------------------------------------

library(dplyr)
library(stringr)

#read in chlorophyll search results
chla <- read.csv("data_clean/assignments_all.csv")
head(chla)

#rename "titles_dup_removed"
productivity <- titles_dup_removed

#library(data.table)
#df1 <- as.vector(chla$title)
#df2 <- as.vector(productivity$title)
#setDT(df1, df2)

in_common <- intersect(chla$title, pro$title)
write.csv(in_common, "titles_in_common_chla_productivity.csv")

# found eight duplicates during manual QC
# sort
productivity_search_all_sort <- with(productivity_search_all, productivity_search_all[order(title) ,])

productivity_search_all_qc <- productivity_search_all_sort[-c(6,15,22,40,41,64,65,68),]

# remove those titles incommon with chla search
productivity_search_all_qc <- productivity_search_all_qc[!productivity_search_all_qc$title %in% in_common, ]

# add ID column
productivity_search_all_qc$ID <- seq.int(nrow(productivity_search_all_qc))

write.csv(productivity_search_all_qc[,-1], "data_clean/productivity_search_qc.csv")

############ group chose to include, adding abstract review assignments
# load packages
library(dplyr)
library(readr)

# random assignments
name = rep(c("MB", "CP", "LY", "DC", "PG", "ES", "RP"), length = nrow(productivity_search_all_qc))
assignments_pro <- cbind(productivity_search_all_qc[,-1], reviewer_name = name)

# add 25% duplication
# reviewers for consistency
Shuffled <- function(inVec) {
  Res <- vector()
  while ( TRUE ) {
    Res <- sample(inVec)
    if ( !any(Res == inVec) ) { break }
  }
  Res
}

assignments_pro$consistency_check <- ave(assignments_pro$reviewer_name, FUN = Shuffled)

# add review columns:
review_cols <- read_csv("Abstract_Reading_Analysis_Template.csv") %>%
  clean_names() %>% select(-c(reviewer, title))
(review_cols_names <- colnames(review_cols))
review_cols$reason <- "marine, type or connectivity"
# add columns but fill with NA
assignments_pro[,review_cols_names]=NA
head(assignments_pro)

# save out all:
write.csv(assignments_pro, file = "data_clean/assignments_productivity_search_all.csv", row.names = FALSE)


# split and write per person:
library(purrr)
library(glue)

assignments_pro %>%
  group_split(reviewer_name) %>% # split by reviewer initials
  walk(., ~write_csv(.x, file=glue("data_clean/{.x$reviewer_name[1]}_assignments_productivity_search_split.csv")))

# pull just three for each person (to double check 25%)
assignments_PG <- subset(assignments_pro, consistency_check == "PG")
subsample_PG <- assignments_PG[sample(nrow(assignments_PG), 3), ]

pg <- read.csv("data_clean/PG_assignments_productivity_search_split.csv")
pg_final <- rbind(pg, subsample_PG)
write.csv(pg_final, "data_clean/PG_assignments_productivity_search_split.csv")

# not the most efficient, but I dont want to delay abstract reviews...
assignments_CP <- subset(assignments_pro, consistency_check == "CP")
subsample_CP <- assignments_CP[sample(nrow(assignments_CP), 3), ]

cp <- read.csv("data_clean/CP_assignments_productivity_search_split.csv")
cp_final <- rbind(cp, subsample_CP)
write.csv(cp_final, "data_clean/CP_assignments_productivity_search_split.csv")

assignments_DC <- subset(assignments_pro, consistency_check == "DC")
subsample_DC <- assignments_DC[sample(nrow(assignments_DC), 3), ]

dc <- read.csv("data_clean/DC_assignments_productivity_search_split.csv")
dc_final <- rbind(dc, subsample_DC)
write.csv(dc_final, "data_clean/DC_assignments_productivity_search_split.csv")

assignments_ES <- subset(assignments_pro, consistency_check == "ES")
subsample_ES <- assignments_ES[sample(nrow(assignments_ES), 3), ]

es <- read.csv("data_clean/ES_assignments_productivity_search_split.csv")
es_final <- rbind(es, subsample_ES)
write.csv(es_final, "data_clean/ES_assignments_productivity_search_split.csv")

assignments_LY <- subset(assignments_pro, consistency_check == "LY")
subsample_LY <- assignments_LY[sample(nrow(assignments_LY), 2), ]

ly <- read.csv("data_clean/LY_assignments_productivity_search_split.csv")
ly_final <- rbind(ly, subsample_LY)
write.csv(ly_final, "data_clean/LY_assignments_productivity_search_split.csv")

assignments_MB <- subset(assignments_pro, consistency_check == "MB")
subsample_MB <- assignments_MB[sample(nrow(assignments_MB), 3), ]

mb <- read.csv("data_clean/MB_assignments_productivity_search_split.csv")
mb_final <- rbind(mb, subsample_MB)
write.csv(mb_final, "data_clean/MB_assignments_productivity_search_split.csv")

assignments_RP <- subset(assignments_pro, consistency_check == "RP")
subsample_RP <- assignments_RP[sample(nrow(assignments_RP), 2), ]

rp <- read.csv("data_clean/RP_assignments_productivity_search_split.csv")
rp_final <- rbind(rp, subsample_RP)
write.csv(rp_final, "data_clean/RP_assignments_productivity_search_split.csv")
