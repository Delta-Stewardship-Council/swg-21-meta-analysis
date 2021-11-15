# code for sorting titles from Mattea's search
# load data
pro <- read.csv("ProQuest-2021-11-11.csv") # 72
wos <- read.csv("WOS_2021-11-11.csv") # 67

# load packages
library(tidyverse)

# make titles lower case
pro_titles <- pro %>% mutate(Title = tolower(Title))
wos_titles <- wos %>% mutate(Article.Title = tolower(Article.Title))

# make a single list
titles <- c(pro_titles$Title, wos_titles$Article.Title)
length(titles) #139

# remove duplicates
titles_dup_removed <- titles[!duplicated(titles)]
length(titles_dup_removed) #86 (12 to 13 each)

# random assignments
name = rep(c("MB", "CP", "LY", "DC", "PG", "ES", "RP"), length = 86)
assignments <- data.frame(title = titles_dup_removed, name = name)
write.csv(assignments, "assignments.csv")
