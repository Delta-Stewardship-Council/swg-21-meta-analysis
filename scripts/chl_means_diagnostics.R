
#load packages
library(dplyr)
library(lattice)
library(car)

#load data
chl <- read.csv("data_clean/chl_means_categorical_units.csv")

#filter for standard units
#units_df <- chl %>%
 # select(ID, units)

#units_by_article <- units_df %>%
#  group_by(units) %>%
#  summarize_each((n_distinct))

chl_subset <- subset(chl, stdunits == "ug/L")

#boxplots
boxplot(mean_chl ~ connectivity_categorical, data = chl_subset)

boxplot(mean_chl ~ connect_binary, data = chl_subset)

boxplot(mean_chl ~ connect_tri, data = chl_subset)


#outliers
boxplot(chl_subset$mean_chl)
dotchart(chl_subset$mean_chl)
bwplot(mean_chl ~ connectivity_categorical | summarized_by, data = chl_subset)

#homogeneity of variance
bwplot(mean_chl ~ connectivity_categorical | connect_binary, data = chl_subset)

bwplot(mean_chl ~ season_month, data = chl_subset)

bwplot(mean_chl ~ year | connect_binary, data = chl_subset)

#distribution
hist(chl_subset$mean_chl, breaks=100)
hist(log(chl_subset$mean_chl), breaks=100)

#zeros
plot(table(round(chl_subset$mean_chl * chl_subset$mean_chl)),     type = "h")

#relationships
scatterplotMatrix(~mean_chl + error_chl + min_chl + max_chl, data=chl_subset)

#interactions
acf(chl_subset$mean_chl)
acf(chl_rmnas$chlorophyll)
