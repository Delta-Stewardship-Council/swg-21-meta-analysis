# load data
DC <- read.csv("data_raw/DC_assignments_results.csv")
MB <- read.csv("data_raw/MB_assignments_split_reviewed.csv")
PG <- read.csv("data_raw/PG_assignments_results.csv")
LY <- read.csv("data_raw/LY_assignments_results.csv")
DC <- data.frame(DC)
MB <- data.frame(MB)

# rename columns
colnames(DC)[17] <- "result_DC"
colnames(MB)[17] <- "result_MB"
colnames(PG)[17] <- "result_PG"
colnames(LY)[8] <- "result_LY"
colnames(LY)[2] <- "title"

# change to unsure (also called 'maybe' and '?')
DC$result_DC <- ifelse(DC$result_DC == "?", "unsure", DC$result_DC)
PG$result_PG <- ifelse(PG$result_PG == "maybe", "unsure", PG$result_PG)

# merge by title
compare <- merge(DC[,c(2,17)], MB[,c(2,17)], by = "title", all = TRUE)
compare <- merge(compare, PG[,c(2,17)],  by = "title", all = TRUE)
compare <- merge(compare, LY[,c(2,8)],  by = "title", all = TRUE)

# make lower case
compare$result_LY <- tolower(compare$result_LY)

# true/false (agreement/disagreement)

compare$agree_disagree <- ifelse(!is.na(compare) & compare$result_DC == compare$result_MB == compare$result_PG == compare$result_LY, 1, 0)

compare$agree_disagree <- identical(compare$result_DC, compare$result_MB, compare$result_PG)

compare$agree_disagree <-!mapply(`%in%`, compare$result_DC, compare$result_MB, compare$result_PG)

`%!=na%` <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))

compare$agree_disagree <- duplicated(compare[,c(2:5)], incomparables = "NA")
compare$agree_disagree <- duplicated(compare[c("result_DC", "result_MB", "result_PG", "result_LY")], na.rm = TRUE)
compare$agree_disagree <- unique(compare[,c(2:5)], incomparables = "NA")
compare$agree_disagree <- compare[!duplicated(compare$result_DC, compare$result_MB, compare$result_PG), ]
