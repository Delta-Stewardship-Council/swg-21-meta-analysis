#load packages
library(sf)
library(ggplot2)
library(leaflet)
library(scales)
library(ggmap)
library(plyr)
library(dplyr)
library(tidyr)
library(rworldmap)
library(stringr)

#load data
data_orig <- read.csv("data_clean/analysis_dat_updated.csv")
data <- data_orig

#clean country column
unique(data$country)

##standardize capitalization
data <- data %>%
  mutate(country = tolower(country))

##replace and with comma
data$country <- str_replace_all(data$country, " and ", ",")

##split lists into multiple rows by commas
data <- data %>%
  separate_rows(country, sep = ',')

##remove percentages
data$country <- gsub("[%()0123456789]", "", data$country)

##remove spaces at beginning and end of responses
data$country <- str_trim(data$country)

##fix spellings

data$country <- revalue(data$country,
                        c("coatia" = "croatia",
                          "austrailia" = "australia",
                          "democratic republic of congo" = "democratic republic of the congo",
                          "us" = "usa"))

##make uppercase to match with map data
data$country <- str_to_title(data$country)

##fix capitalization
data$country <- revalue(data$country,
                        c("Uk" = "UK",
                          "Usa" = "USA",
                          "Democratic Republic Of The Congo" = "Democratic Republic of the Congo"))


#make country into factor to allow tabulation
data$country <- as.factor(data$country)

#make table with count for each country
country_count <- as.data.frame(table(data$country))
country_count <- setNames(country_count, c("country", "n"))

#create map
world <- map_data("world")

world %>%
  merge(country_count, by.x="region", by.y = "country", all.x = T) %>%
  arrange(group,order) %>%
  ggplot(aes(x = long, y = lat, group=group, fill=n)) +
  geom_polygon()


##4 articles missing from map because does not match country data: North America, South America, Africa, and England
