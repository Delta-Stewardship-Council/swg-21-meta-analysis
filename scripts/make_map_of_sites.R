# map locations

#load packages
library(dplyr)
library(readr)
library(sf)
library(scales)
library(tmap)
library(tmaptools)
library(rnaturalearth)

# load data
dat <- read_csv("data_raw/all_localities_by_paper.csv")

# Add a col for original label (drop anything _a or _b)
dat <- dat |>
  mutate(ID_orig = gsub("_[a-z]$", "", ID), .after=ID)

# load kml locations
sites <- st_read("data_raw/NCEAS_metanalysis_sites.kml") %>%
  mutate(lon_x = st_coordinates(.)[,1],
         lat_y = st_coordinates(.)[,2]) %>%
  select(ID=Name, lon_x, lat_y, geometry)

# Join Data ---------------------------------------------------------------

dat_sf <- left_join(sites, dat)

# check for duplicates?
dat_sf %>% st_drop_geometry() %>% group_by(ID) %>%
  tally() %>% # should all be 1
  filter(n>1) # should get zero back



# Make a Quick World Map --------------------------------------------------

data(World, land)

(m1 <- tm_shape(World, projection = "+proj=eck4") +
  tm_fill("continent", palette="Greys",legend.show = FALSE, alpha = 0.8) +
  tm_shape(World, projection = "+proj=eck4") +
  tm_polygons(border.col = "gray50", alpha=0, legend.show = FALSE) +
  tm_shape(dat_sf) + tm_symbols(shape = 21, col="aquamarine", alpha=0.9, border.col="gray20", size = 0.6) +
  tm_style("white", frame.lwd=0) + tm_format("World")+
  tm_layout(frame=FALSE,
            main.title = "chl-a Sites",
            main.title.position = "center"))

tmap_save(m1, filename = "figs/map_of_literature_study_locations.png", width = 11, height = 8.5, dpi = 300)
