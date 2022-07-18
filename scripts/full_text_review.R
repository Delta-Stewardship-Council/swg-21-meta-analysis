#library
library(dplyr)

# data
full_results <- read.csv("data_clean/abstract_results_all.csv")
all_titles <- read.csv("data_clean/abstract_results_chl.csv")

# subset yes/unsure inclusion
unique(full_results$inclusion)
full_results_include <- subset(full_results, inclusion == "Y" | inclusion == "y" |  inclusion == "unsure")
# remove duplicates
final_titles <- full_results_include[!duplicated(full_results_include[,c(1,18)]),] #105
# random assignments
name = rep(c("MB", "CP", "LY", "DC", "PG", "ES", "RP"), length = nrow(final_titles))
# remove abstract review data
final_titles <- final_titles[,c(1,3:8,18)]
assignments <- cbind(final_titles, reviewer_name = name)

# missing some data from chl-a titles
head(all_titles)
all_titles$ID <- sub("^", "chl_", all_titles$ID)
assignments_na <- subset(assignments, is.na(authors))

assignments_fix <- assignments_na  %>% inner_join(all_titles, by = "ID") %>%
  mutate (authors = coalesce(authors.x, authors.y), pub_year = coalesce(pub_year.x, pub_year.y))

assignments_fix <- assignments_fix[,c(1,8,9,23,24)]
colnames(assignments_fix)[1] <- "title"

assignments_not_na <- subset(assignments, !is.na(authors))

assign_final <- rbind(assignments_not_na[,c(1,2,4,8,9)], assignments_fix)

write.csv(assign_final, "data_clean/full_text_review_assignments.csv")
