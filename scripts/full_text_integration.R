# libraries
library(readr)

# load data
cc_pg <- read_excel("data_raw/Content_coding_PG.xlsx")
fta_pg <- read_csv("data_raw/Fulltext_Analysis_Results_PG.csv")

fta_ly <- read_csv("data_raw/Fulltext_Analysis_Template_LY.csv")
fta_es <- read_csv("data_raw/Fulltext_Analysis_Template_ebs.csv")
fta_mb <- read_csv("data_raw/MB_Fulltext_Analysis.csv")

cc_mb <- read_excel("data_raw/MB_coding.xlsx")
fta_cp <- read_excel("data_raw/CP_Fulltext_Coding.xlsx")
fta_dc <- read_csv("data_raw/dc_metafulltextreview_4.27.csv")

