
#load packages

#load data
chl <- read.csv("data_clean/chl_means_categorical.csv")

#boxplots
boxplot(mean_chl ~ connectivity_categorical, data = chl)

boxplot(mean_chl ~ connectivity, data = chl)
