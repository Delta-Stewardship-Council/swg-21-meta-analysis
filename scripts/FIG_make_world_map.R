
#load packages
library(sf)
library(ggplot2)
library(leaflet)
library(scales)
library(ggmap)
library(plyr)
library(dplyr)
library(tidyr)
#library(rworldmap)
library(stringr)

# some good mapping packages/options:
library(tmap) # similar to ggplot but just for mapping stuff
library(tmaptools) # mapping tools including basemap layers
library(units) # good for messing with units
library(rnaturalearth) # another base layer of data for world stuff
library(ggspatial) # for scales and compasses in ggplot2

# Load data -------------
data <- read.csv("data_clean/analysis_dat_updated.csv")

# Clean data --------------

#clean country column
unique(data$country)

##standardize capitalization
data <- data %>%
  mutate(country = tolower(country))

##replace "and" with comma
data$country <- str_replace_all(data$country, " and ", ",")

##split lists into multiple rows by commas
data <- data %>%
  separate_rows(country, sep = ',')

##remove percentages
data$country <- gsub("[%()0123456789]", "", data$country)

##remove spaces at beginning and end of responses
data$country <- str_trim(data$country)

##fix spellings

data$country <- plyr::revalue(data$country,
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

###4 articles missing from map because does not match country data: North America, South America, Africa, and England
sum(country_count$n)
# can recode with dplyr::case_when for nested ifelse type statements
# here we can change these (or basically add 1 to each representative country instead of continent)

country_count <- country_count |>
  mutate(country = as.character(country)) |>  # need to do this for reformat below
  filter(country!="England") |> # drop the England row
  mutate(
    n = case_when(
      grepl("^UK", country) ~ n + 1L,
      TRUE ~ n))

# for the continents, could refine or add 1 for each country within continent using some different datasets (just join to this).


# Get World Polygons ------------------------------------------------------

# data from ggplot2 package (in ggplot format)
world <- map_data("world")

# data from rnatural earth package (in sf format)
world_r <- rnaturalearth::countries110 |> st_as_sf()


# Map with ggplot2 --------------------------------------------------------

# base maps
(gg_b1 <- ggplot(data=world, aes(x = long, y = lat, group=group)) +
  geom_polygon(fill="gray", color="black"))

(gg_b2 <- ggplot() + geom_sf(data=world_r, fill="gray"))


## Add Country Count Data: ggplot --------------------------------------------------

(gg_world <-
  world %>%
  merge(country_count, by.x="region", by.y = "country", all.x = T) %>%
  arrange(group,order)  %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group=group, fill=n),
               color=alpha("gray20", 0.3), alpha=0.8) + # add outlines
  scale_fill_viridis_c("No. of Articles")) +
  labs(x="Longitude", y="Latitude")

# add some fancy stuff
gg_world +
  #ggthemes::theme_clean()
  ggthemes::theme_map() +
  ggspatial::annotation_north_arrow(width = unit(0.8, "cm"),
                                    location = "br")
#dir.create("figs")
ggsave(filename = "figs/ggplot_world_map_w_count.png", dpi=300, width = 11, height = 8.5, bg = "white")

## Add Country Count Data: tmap --------------------------------------------------

library(tmap)
tmap_options(check.and.fix = TRUE)
data("World") # load the world dataset from tmap

country_world <-
   country_count |>
   # fix names
   mutate(country =
            case_when(
              country == "UK" ~ "United Kingdom",
              country == "USA" ~ "United States",
              country == "Democratic Republic of the Congo" ~ "Dem. Rep. Congo",
              TRUE ~ country))
# now join
tmap_world <- left_join(World, country_world, by=c("name"="country")) |>
  left_join(country_world, by=c("continent"="country")) # now join by continent
# that gives a "n.x" and "n.y" column...we can join them for one final sum
names(tmap_world)
tmap_world <- tmap_world |> rowwise() |>
  mutate(n = rowSums(cbind(n.x, n.y), na.rm = T), .after="n.y",
         n = na_if(n, 0))

tm_plot1 <- tm_shape(tmap_world) +
  tm_polygons(col = "n", colorNA="gray", border.col = "gray20") +
  tm_compass() +
  tm_scale_bar() +
  tm_layout(bg.color = alpha("cyan4", 0.4), inner.margins = c(0, .02, .02, .02))
tm_plot1

# can adjust the legend scale breaks, color palettes, etc

tmap_save(tm_plot1, filename = "figs/tmap_world_map_w_count.png",
          width = 11, dpi = 300)

