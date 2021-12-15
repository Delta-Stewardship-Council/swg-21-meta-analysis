library(dplyr)
library(stringr)

SL_21 <- read.csv("data_raw/Shen_Liu_2021 .csv")
head(SL_21)

SL_titles <- SL_21 %>%
  # make title lower case
  mutate(Title = tolower(Title),
         # strip white spaces at end/beginnings all columns
         across(.cols=everything(), ~str_trim(.)))

head(SL_titles)

ours <- read.csv("data_clean/assignments_all.csv")
head(ours)

library(data.table)
df1 <- as.vector(SL_titles$Title)
df2 <- as.vector(ours$title)
setDT(df1, df2)

in_common <- intersect(SL_titles$Title, ours$title)
write.csv(in_common, "titles_in_common_w_LS2021.csv")
