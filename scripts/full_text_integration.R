# libraries
library(readxl)

# load data
cc_pg <- read_excel("data_raw/Content_coding_PG.xlsx")
fta_pg <- read_csv("data_raw/Fulltext_Analysis_Results_PG.csv")

fta_ly <- read_csv("data_raw/Fulltext_Analysis_Template_LY.csv")
fta_es <- read_csv("data_raw/Fulltext_Analysis_Template_ebs_v2.csv")

fta_mb <- read_csv("data_raw/MB_Fulltext_Analysis.csv")
cc_mb <- read_excel("data_raw/MB_coding.xlsx")

fta_cp <- read_excel("data_raw/CP_Fulltext_Coding.xlsx")
cc_cp <- read_excel("data_raw/CP_Fulltext_Coding.xlsx", sheet = "Coding")

fta_dc <- read_csv("data_raw/dc_metafulltextreview_4.27.csv")

fta_rp <- read.csv("data_raw/RP_full_text_review_results.csv")
cc_rp <- read.csv("data_raw/RP_full_text_review_conn_coding.csv")

# combine full text analysis and content coding for PG, MB, CP
fta_pg <- merge(fta_pg, cc_pg, by = "ID")
fta_mb <- merge(fta_mb, cc_mb, by = "ID")
colnames(cc_cp)[1] <- "ID" #space behind ID...
fta_cp <- merge(fta_cp, cc_cp, by = "ID", all.x = TRUE)

# get the same column names
a <- colnames(fta_ly) # should all have 50 columns
b <- colnames(fta_dc)
setdiff(a, b)
setdiff(b, a)

colnames(fta_ly)[31] <- "cc_notes" #connectivity code notes
colnames(fta_ly)[24] <- "regulated"
fta_ly$p_value <- "NA"
fta_ly$water_body_size <- "NA"

fta_es$regulated <- "NA"
fta_es$cc_notes <- "NA"
fta_es$basin_size <- "NA"
colnames(fta_es)[36] <- "p_value"

colnames(fta_cp)[47] <- "seasonal_code"
colnames(fta_cp)[48] <- "repeats_code"
colnames(fta_cp)[49] <- "regulated"
colnames(fta_cp)[45] <- "connectivity_type"
colnames(fta_cp)[46] <- "connectivity_measure"
colnames(fta_cp)[51] <- "cc_notes"
fta_cp <- fta_cp[,-c(3,50)]
fta_cp$p_value <- "NA"

fta_mb$basin_size <- "NA"
fta_mb$regulated <- "NA"
colnames(fta_mb)[48] <- "cc_notes"
colnames(fta_mb)[40] <- "notes"

colnames(fta_pg)[46] <- "seasonal_code"
colnames(fta_pg)[47] <- "repeats_code"
colnames(fta_pg)[49] <- "regulated"
colnames(fta_pg)[44] <- "connectivity_type"
colnames(fta_pg)[45] <- "connectivity_measure"
colnames(fta_pg)[48] <- "cc_notes"
colnames(fta_pg)[32] <- "p_value"
fta_pg$basin_size <- "NA"

# should have done this up front...
for (i in 1:length(colnames(fta_dc))) {
  colnames(fta_dc)[i] = tolower(colnames(fta_dc)[i])
  # return()     ... you don't need this
}
colnames(fta_dc)[1] <- "ID"
colnames(fta_dc)[12] <- "basin_size"
colnames(fta_dc)[13] <- "site_size"
colnames(fta_dc)[18] <- "seasons"
colnames(fta_dc)[33] <- "primary_productivity_metric"
colnames(fta_dc)[34] <- "food_web"
colnames(fta_dc)[40] <- "data_analysis_method"
fta_dc <- fta_dc[,-7]

# lots missing from DC
fta_dc$seasonality <- "NA"
fta_dc$hyporheic <- "NA"
fta_dc$temporal_def <- "NA"
fta_dc$regulated <- "NA"
fta_dc$connectivity_def <- "NA"
fta_dc$cc_notes <- "NA"
fta_dc$meta_analysis <- "NA"
fta_dc$include_exclude <- "NA"
fta_dc$include_exclude_reason <- "NA"
fta_dc$p_value <- "NA"

full_text_review <- rbind(fta_dc, fta_pg, fta_mb, fta_ly, fta_es, fta_cp)

write.csv(full_text_review, "data_clean/full_text_review_results.csv")
