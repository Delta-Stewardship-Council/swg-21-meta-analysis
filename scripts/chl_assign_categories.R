

#load packages
library(tidyr)
library(plyr)
library(dplyr)


#load data
chl <- read.csv("data_clean/chl_means.csv")

#view categorical connectivity designations
unique(chl$connectivity_categorical)

#attempt to assign all chlorophyll means as either connected or disconnected
##create new columns
chl$connect_binary <- chl$connectivity_categorical
chl$connect_tri <- chl$connectivity_categorical

##rename values either connected or disconnected
chl$connect_binary <- revalue(chl$connect_binary,
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
  mutate(connect_binary = replace(connect_binary, connect_binary == "", "connected")) #all connected, but differ in size and duration of flood. Used different function because revalue() does not work on zero-length variables.

unique(chl$connect_binary)


##rename values either connected, semi-connected, or disconnected
chl$connect_tri <- revalue(chl$connect_tri,
                              c("isolated" = "disconnected",
                                "isolated?" = "disconnected",
                                "connected?" = "connected",
                                "semi isolated?" = "semi-connected",
                                "connected lake" = "connected",
                                "isolated lake" = "disconnected",
                                "connected river" = "connected",
                                "connected stream" = "connected",
                                "connected lake to main channel" = "connected",
                                "connected lake to Mini Stream (but generally more isolated)" = "semi-connected",
                                "Isolated river" = "disconnected",
                                "Isolated wetland"= "disconnected",
                                "isolated or dry" = "disconnected",
                                "high water" = "connected",
                                "partially connected" = "semi-connected",
                                "less connected" = "semi-connected",
                                "more connected" = "connected",
                                "semi-connected" = "semi-connected",
                                "highly connected" = "connected",
                                "intermediate connectivity" = "semi-connected",
                                "na" = "unclear", #connectivity not defined in paper, maybe should be removed
                                "falling water" = "unclear",
                                "upstream" = "unclear", #connected not clear defined in paper, maybe should be removed
                                "downstream" = "unclear", #connectivity not clearly defined in paper, maybe should be removed
                                "connected, disconnected" = "unclear",
                                "gross communicty production" = "unclear",
                                "net community metabolism" = "unclear",
                                "community respiration" = "unclear"))
chl <- chl %>%
  mutate(connect_tri = replace(connect_tri, connect_tri == "", "connected")) #all connected, but differ in size and duration of flood. Used different function because revalue() does not work on zero-length variables.

unique(chl$connect_tri)

#save data set with category assignment
write.csv(chl, "data_clean/chl_means_categorical.csv", row.names = FALSE)

