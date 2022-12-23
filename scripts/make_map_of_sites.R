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

dat_sf <- left_join(sites, dat) |>
  # drop Z value that comes with Google Earth Files
  st_zm()

# check for duplicates?
# this is for the revised ID
dat_sf %>% st_drop_geometry() %>% group_by(ID) %>%
  tally() %>% # should all be 1
  filter(n>1) # should get zero back

# this is for the original ID
dat_sf %>% st_drop_geometry() %>% group_by(ID_orig) %>%
  tally() %>%
  filter(n>1) # this is for multi-point Sites

# find the missing ones
dat$ID[!dat$ID %in% sites$ID]

# Make a World Map --------------------------------------------------

# add custom fonts
#library(showtext)
#font_paths() # where are fonts installed
# check if font is installed?
# font_files() %>% filter(grepl("Roboto", ps_name))
# add it remotely
#font_add_google("Roboto")
# use this to make font available
#showtext_auto()

data(World, land)

(m1 <- tm_shape(World, projection = "+proj=eck4") +
  tm_fill("continent", palette="Greys",legend.show = FALSE, alpha = 0.8) +
  tm_shape(World, projection = "+proj=eck4") +
  tm_polygons(border.col = "gray50", alpha=0, legend.show = FALSE) +
  tm_shape(dat_sf) + tm_symbols(shape = 21, col="aquamarine", alpha=0.9, border.col="gray20", size = 0.6) +
  tm_style("white", frame.lwd=0) + tm_format("World")+
  tm_layout(frame=FALSE, fontfamily = "Roboto",
            main.title = "Meta-analysis Sites",
            main.title.position = "center"))

tmap_save(m1, filename = "figs/map_of_literature_study_locations.png", width = 11, height = 8.5, dpi = 300)

tmap_save(m1, filename = "figs/map_of_literature_study_locations.pdf", width = 11, height = 8.5, dpi = 300, device = cairo_pdf)


# Export data with lat long ------------------------------------------------

# export to csv
dat_sf |> st_drop_geometry() |>
  write_csv(file = "data_clean/all_localities_by_paper_w_lat_lon.csv")

# export to shp
st_write(dat_sf, "data_clean/all_localities_by_paper_w_lat_lon.shp")
