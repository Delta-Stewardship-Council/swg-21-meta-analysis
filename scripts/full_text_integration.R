# libraries
library(readxl)
library(readr)

# load data
cc_pg <- read_excel("data_raw/Content_coding_PG.xlsx")
fta_pg <- read_csv("data_raw/Fulltext_Analysis_Results_PG.csv")

fta_ly <- read_csv("data_raw/Fulltext_Analysis_TemplateUpdated_LY.csv")
fta_es <- read_csv("data_raw/Fulltext_Analysis_Template_ebs_060322.csv")

fta_mb <- read_csv("data_raw/MB_Fulltext_Analysis_Jun10.csv")
cc_mb <- read_excel("data_raw/MB_coding.xlsx")

fta_cp <- read_excel("data_raw/CP_Fulltext_Coding_v3.xlsx", sheet = "Full_text")
cc_cp <- read_excel("data_raw/CP_Fulltext_Coding_v3.xlsx", sheet = "Connectivity_Coding")

fta_dc <- read_csv("data_raw/dc_metafulltextreview_5.20.csv")

fta_rp <- read.csv("data_raw/RP_full_text_review_results.csv")
cc_rp <- read.csv("data_raw/RP_full_text_review_conn_coding.csv")

# add round two results
fta_r2 <- read_excel("data_raw/MB_Full_Text_Review_Round2.xlsx")
cc_r2 <- read_excel("data_raw/MB_Content_Coding_Round2.xlsx")

# combine full text analysis and content coding for PG, MB, CP
fta_pg <- merge(fta_pg, cc_pg, by = "ID")
fta_mb <- merge(fta_mb, cc_mb, by = "ID")
fta_rp <- merge(fta_rp, cc_rp, by = "ID")
fta_r2 <- merge(fta_r2, cc_r2, by = "ID")

colnames(cc_cp)[1] <- "ID" #space behind ID...
fta_cp <- merge(fta_cp, cc_cp, by = "ID", all.x = TRUE)

# get the same column names
a <- colnames(fta_ly) # should all have 50 columns
b <- colnames(fta_r2)
setdiff(a, b)
setdiff(b, a)

# update has two extra list columns
fta_ly <- fta_ly[,-c(2:3)]
colnames(fta_ly)[29] <- "cc_notes" #connectivity code notes
colnames(fta_ly)[22] <- "regulated"
fta_ly$p_value <- "NA"
fta_ly$water_body_size <- "NA"

colnames(fta_es)[30] <- "regulated"
#fta_es$regulated <- "NA"
fta_es$cc_notes <- "NA"
fta_es$basin_size <- "NA"
colnames(fta_es)[37] <- "p_value"
fta_es <- fta_es[,-c(2:3)]

colnames(fta_cp)[47] <- "seasonal_code"
colnames(fta_cp)[48] <- "repeats_code"
colnames(fta_cp)[50] <- "regulated"
colnames(fta_cp)[45] <- "connectivity_type"
colnames(fta_cp)[46] <- "connectivity_measure"
colnames(fta_cp)[49] <- "cc_notes"
fta_cp <- fta_cp[,-c(2:4)]
fta_cp$p_value <- "NA"

fta_mb$basin_size <- "NA"
#fta_mb$regulated <- "NA"
colnames(fta_mb)[49] <- "cc_notes"
colnames(fta_mb)[40] <- "notes"
fta_mb <- fta_mb[,-c(2:3)]

colnames(fta_pg)[46] <- "seasonal_code"
colnames(fta_pg)[47] <- "repeats_code"
#colnames(fta_pg)[49] <- "regulated"
colnames(fta_pg)[44] <- "connectivity_type"
colnames(fta_pg)[45] <- "connectivity_measure"
colnames(fta_pg)[48] <- "cc_notes"
colnames(fta_pg)[32] <- "p_value"
fta_pg$basin_size <- "NA"
fta_pg <- fta_pg[,-c(2:3)]

# should have done this up front...
for (i in 1:length(colnames(fta_dc))) {
  colnames(fta_dc)[i] = tolower(colnames(fta_dc)[i])
  # return()     ... you don't need this
}
colnames(fta_dc)[1] <- "ID"
colnames(fta_dc)[13] <- "basin_size"
colnames(fta_dc)[49] <- "data_analysis_method"
colnames(fta_dc)[7] <- "chlorophyll"
fta_dc <- fta_dc[,-36] # seems to be repeat of 7
colnames(fta_dc)[39] <- "p_value"

# a few extra columns
fta_dc <- fta_dc[,-c(6,8,9,19,41,42)] #"month", "phytoplankton", "fish"

# missing from DC
fta_dc$regulated <- "NA"
fta_dc$cc_notes <- "NA"
fta_dc$meta_analysis <- "NA"
fta_dc$include_exclude_reason <- "NA"
#fta_dc$p_value <- "NA"

colnames(fta_rp)[29] <- "p_value"
colnames(fta_rp)[7] <- "latitude"
colnames(fta_rp)[45] <- "regulated"
colnames(fta_rp)[41] <- "connectivity_type"
colnames(fta_rp)[46] <- "cc_notes"
colnames(fta_rp)[23] <- "predictors"
colnames(fta_rp)[37] <- "notes"
fta_rp$basin_size <- "NA"
fta_rp$longitude <- "NA"

fta_r2 <- fta_r2[,-c(2,3,42)]
colnames(fta_r2)[42] <- "seasonal_code"
colnames(fta_r2)[43] <- "repeats_code"
colnames(fta_r2)[41] <- "connectivity_type"
colnames(fta_r2)[40] <- "connectivity_measure"
colnames(fta_r2)[44] <- "cc_notes"
colnames(fta_r2)[37] <- "meta_analysis" #visually this is fine, but R doesn't like it
fta_r2$basin_size <- "NA"
fta_r2$temporal_def <- "NA"
fta_r2$primary_productivity_metric <- "NA"

full_text_review <- rbind(fta_dc, fta_pg, fta_mb, fta_ly, fta_es, fta_cp, fta_rp, fta_r2)

write.csv(full_text_review, "data_clean/full_text_review_results.csv")
