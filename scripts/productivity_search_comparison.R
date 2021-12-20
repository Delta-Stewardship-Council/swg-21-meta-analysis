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

in_common <- intersect(chla$title, productivity$title)
write.csv(in_common, "titles_in_common_chla_productivity.csv")



