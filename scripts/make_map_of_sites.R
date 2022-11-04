# map locations

#load packages
library(tidyverse)
library(sf)
library(scales)
library(tmap)
library(tmaptools)
library(rworldmap)
library(stringr)

#load data
dat <- read_csv("data_clean/analysis_dat_updated.csv")

