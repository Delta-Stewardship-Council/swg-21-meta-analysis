

#load packages
library(tidyr)
library(plyr)
library(dplyr)


#load data
chl <- read.csv("data_clean/chl_means.csv")

#view categorical connectivity designations
unique(chl$connectivity_categorical)

#attempt to assign all chlorophyll means as either connected or disconnected
##create new column
chl$connectivity <- chl$connectivity_categorical

##rename values
chl$connectivity <- revalue(chl$connectivity,
                            c("isolated" = "disconnected",
                              "isolated?" = "disconnected",
                              "connected?" = "connected",
                              "semi isolated?" = "connected",
                              "connected lake" = "connected",
                              "isolated lake" = "disconnected",
                              "connected river" = "connected",
                              "connected stream" = "connected",
                              "connected lake to main channel" = "connected",
                              "connected lake to Mini Stream (but generally more isolated)" = "connected",
                              "Isolated river" = "disconnected",
                              "Isolated wetland"= "disconnected",
                              "isolated or dry" = "disconnected",
                              "high water" = "connected",
                              "partially connected" = "connected",
                              "less connected" = "connected",
                              "more connected" = "connected",
                              "semi-connected" = "connected",
                              "highly connected" = "connected",
                              "intermediate connectivity" = "connected",
                              "na" = "unclear", #connectivity not defined in paper, maybe should be removed
                              "falling water" = "unclear",
                              "upstream" = "unclear", #connected not clear defined in paper, maybe should be removed
                              "downstream" = "unclear", #connectivity not clearly defined in paper, maybe should be removed
                              "connected, disconnected" = "unclear",
                              "gross communicty production" = "unclear",
                              "net community metabolism" = "unclear",
                              "community respiration" = "unclear"))
chl <- chl %>%
  mutate(connectivity = replace(connectivity, connectivity == "", "connected")) #all connected, but differ in size and duration of flood. Used different function because revalue() does not work on zero-length variables.

unique(chl$connectivity)

