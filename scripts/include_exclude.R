# seems like exclude/include is not consistent in full text results

# libraries
library(readxl)
library(readr)

# check each results spreadsheet

fta_rp <- read.csv("data_raw/RP_full_text_review_results.csv")
cc_rp <- read.csv("data_raw/RP_full_text_review_conn_coding.csv")

fta_rp <- merge(fta_rp, cc_rp, by = "ID")

rp_exclude <- subset(fta_rp, include_exclude == "N")

# chl_13 and pro_5 are incorrectly assigned, chl_70 is not original research

fta_ly <- read_csv("data_raw/Fulltext_Analysis_TemplateUpdated_LY.csv")

ly_exclude <- subset(fta_ly, include_exclude == "exclude")

# pro_42 incorrectly assigned

cc_pg <- read_excel("data_raw/Content_coding_PG.xlsx")
fta_pg <- read_csv("data_raw/Fulltext_Analysis_Results_PG.csv")

fta_pg <- merge(fta_pg, cc_pg, by = "ID")
pg_exclude <- subset(fta_pg, include_exclude == "exclude")

# fine

fta_mb <- read_csv("data_raw/MB_Fulltext_Analysis.csv")
cc_mb <- read_excel("data_raw/MB_coding.xlsx")

fta_mb <- merge(fta_mb, cc_mb, by = "ID")

mb_exclude <- subset(fta_mb, include_exclude == "exclude")

# fine

fta_es <- read_csv("data_raw/Fulltext_Analysis_Template_ebs_060122.csv")

# all "include"

fta_cp <- read_excel("data_raw/CP_Fulltext_Coding_v3.xlsx", sheet = "Full_text")
cc_cp <- read_excel("data_raw/CP_Fulltext_Coding_v3.xlsx", sheet = "Connectivity_Coding")

colnames(cc_cp)[1] <- "ID" #space behind ID...
fta_cp <- merge(fta_cp, cc_cp, by = "ID", all.x = TRUE)

cp_exclude <- subset(fta_cp, include_exclude == "exclude")

# fine

fta_dc <- read_csv("data_raw/dc_metafulltextreview_5.20.csv")

dc_exclude <- subset(fta_dc, include_exclude == "exclude")
# need to check pro_48


# also need to check abstract review responses
