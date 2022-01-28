# load data
compare_abstracts <- read.csv("data_clean/abstract_results_chl.csv")

# ID drops
compare_abstracts$NO <- apply(compare_abstracts, 1, function(r) any(r == "n"))
drops <- subset(compare_abstracts, NO == "TRUE")

compare_abstracts_pro$NO <- apply(compare_abstracts_pro, 1, function(r) any(r == "n"))
drops <- subset(compare_abstracts_pro, NO == "TRUE")

write.csv(drops, "data_clean/abstract_eliminate_chl.csv")

# fill in why they were dropped
# marine
# not original research
# not landscape-scale hydrologicl connectivity

