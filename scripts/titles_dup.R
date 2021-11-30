# code for sorting titles from Mattea's search

# Load libraries ----------------------------------------------------------

# load packages
library(dplyr)
library(stringr)
library(janitor)
library(readr)

# Import data -------------------------------------------------------------

# load data
pro <- read.csv("data_raw/ProQuest-2021-11-11.csv") # 72
wos <- read.csv("data_raw/WOS_2021-11-11.csv") # 67

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
dim(titles_dup_removed) #80 (12 each)
table(titles_dup_removed$database_type)
# proquest=45, wos=35

# Random Assignments for Review -------------------------------------------

# random assignments
name = rep(c("MB", "CP", "LY", "DC", "PG", "ES", "RP"), length = nrow(titles_dup_removed))
assignments <- cbind(titles_dup_removed, reviewer_name = name)

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

assignments$consistency_check <- ave(assignments$reviewer_name, FUN = Shuffled)

# add review columns:
review_cols <- read_csv("Abstract_Reading_Analysis_Template.csv") %>%
  clean_names() %>% select(-c(reviewer, title))
(review_cols_names <- colnames(review_cols))

# add columns but fill with NA
assignments[,review_cols_names]=NA
head(assignments)

# save out all:
write.csv(assignments, file = "data_clean/assignments_all.csv", row.names = FALSE)

# split and write per person:
library(purrr)
library(glue)

assignments %>%
  group_split(reviewer_name) %>% # split by reviewer initials
  walk(., ~write_csv(.x, file=glue("data_clean/{.x$reviewer_name[1]}_assignments_split.csv")))

# pull just three for each person (to double check 25%)
assignments_PG <- subset(assignments, consistency_check == "PG")
subsample_PG <- assignments_PG[sample(nrow(assignments_PG), 3), ]

pg <- read.csv("data_clean/PG_assignments_split.csv")
pg_final <- rbind(pg, subsample_PG)
write.csv(pg_final, "data_clean/PG_assignments_split.csv")

# not the most efficient, but I dont want to delay abstract reviews...
assignments_CP <- subset(assignments, consistency_check == "CP")
subsample_CP <- assignments_CP[sample(nrow(assignments_CP), 3), ]

cp <- read.csv("data_clean/CP_assignments_split.csv")
cp_final <- rbind(cp, subsample_CP)
write.csv(cp_final, "data_clean/CP_assignments_split.csv")

assignments_DC <- subset(assignments, consistency_check == "DC")
subsample_DC <- assignments_DC[sample(nrow(assignments_DC), 3), ]

dc <- read.csv("data_clean/DC_assignments_split.csv")
dc_final <- rbind(dc, subsample_DC)
write.csv(dc_final, "data_clean/DC_assignments_split.csv")

assignments_ES <- subset(assignments, consistency_check == "ES")
subsample_ES <- assignments_ES[sample(nrow(assignments_ES), 3), ]

es <- read.csv("data_clean/ES_assignments_split.csv")
es_final <- rbind(es, subsample_ES)
write.csv(es_final, "data_clean/ES_assignments_split.csv")

assignments_LY <- subset(assignments, consistency_check == "LY")
subsample_LY <- assignments_LY[sample(nrow(assignments_LY), 3), ]

ly <- read.csv("data_clean/LY_assignments_split.csv")
ly_final <- rbind(ly, subsample_LY)
write.csv(ly_final, "data_clean/LY_assignments_split.csv")

assignments_MB <- subset(assignments, consistency_check == "MB")
subsample_MB <- assignments_MB[sample(nrow(assignments_MB), 3), ]

mb <- read.csv("data_clean/MB_assignments_split.csv")
mb_final <- rbind(mb, subsample_MB)
write.csv(mb_final, "data_clean/MB_assignments_split.csv")

assignments_RP <- subset(assignments, consistency_check == "RP")
subsample_RP <- assignments_RP[sample(nrow(assignments_RP), 3), ]

rp <- read.csv("data_clean/RP_assignments_split.csv")
rp_final <- rbind(rp, subsample_RP)
write.csv(rp_final, "data_clean/RP_assignments_split.csv")
