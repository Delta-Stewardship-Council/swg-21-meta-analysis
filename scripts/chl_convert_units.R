
#load packages
library(tidyr)
library(plyr)
library(dplyr)

#read in data
chl <- read.csv("data_clean/chl_means_categorical.csv")

#inspect units
unique(chl$units)

##assign equivalent units to one label
chl$stdunits <- chl$units
chl$stdunits <- revalue(chl$stdunits,
                            c("ug/l" = "ug/L",
                              "mg/m3" = "ug/L",
                              "μg L–1" = "ug/L",
                              "ppb" = "ug/L",
                              "ug dm^-3" = "ug/L"))
chl <- chl %>%
  mutate(stdunits = replace(stdunits, stdunits == "", "ug/L")) #chl_19 Neeves et al 2018 - checked article and units were ug/L

unique(chl$stdunits) #remaining two variations are rates per day associated with papers with unclear connectivity classifications
#chl_27 -- Feio et al 2010
#chl_50 -- Steinman et al 2011

#save data set with standardized units
write.csv(chl, "data_clean/chl_means_categorical_units.csv", row.names = FALSE)

