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

# add review columns:
review_cols <- read_csv("Abstract_Reading_Analysis_Template.csv") %>%
  clean_names() %>% select(-c(reviewer, title))
(review_cols_names <- colnames(review_cols))

# add columns but fill with NA
assignments[,review_cols_names]=NA
head(assignments)

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

assignments$review <- ave(assignments$name, FUN = Shuffled)

# pull just three for each person (example for my initials).
assignments_PG <- subset(assignments, review == "PG")
assignments_PG[sample(nrow(assignments_PG), 3), ]

# save out all:
write.csv(assignments, file = "data_clean/assignments_all.csv", row.names = FALSE)

# split and write per person:
library(purrr)
library(glue)

assignments %>%
  group_split(reviewer_name) %>% # split by reviewer initials
  walk(., ~write_csv(.x, file=glue("data_clean/{.x$reviewer_name[1]}_assignments_split.csv")))
