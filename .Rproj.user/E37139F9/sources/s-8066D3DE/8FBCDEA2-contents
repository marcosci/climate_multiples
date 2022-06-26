library(tidyverse)
library(glue)
library(terra)
library(sf)
library(scico)
library(hrbrthemes)
library(classInt)
# create folder to store data in project
dir.create("data")

# we have data for the following years:
years <- 1951L:2021L

# ... and start download it:
walk(years, function(year){
  download.file(
    glue("https://opendata.dwd.de/climate_environment/CDC/grids_germany/annual/hot_days/grids_germany_annual_hot_days_{year}_17.asc.gz"),
    destfile = glue("data/grids_germany_annual_hot_days_{year}.asc.gz")
  )
  R.utils::gunzip(glue("data/grids_germany_annual_hot_days_{year}.asc.gz"), remove=TRUE)
})

# list all raster 
hot_raster <- list.files(path = "data/", full.names = TRUE)

# and read them via terra:
hot_raster <- rast(hot_raster)
names(hot_raster) <- years

# read in counties
# source of shapefile: https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/verwaltungsgebiete/verwaltungsgebiete-1-2-500-000-stand-01-01-vg2500.html
counties <- st_read("vg2500/vg2500_krs.shp")
counties_vect <- vect(counties)

# reproject to bring the data into the same space
dwd_crs <- "epsg:31467"
crs(hot_raster) <- dwd_crs
hot_raster <- terra::project(hot_raster, dwd_crs)

crs(counties_vect) <- dwd_crs
counties_vect <- terra::project(counties_vect, dwd_crs)

# extract raster values for each into the county polygon
hot_counties <- terra::extract(hot_raster, 
                               counties_vect, 
                               mean, 
                               na.rm=TRUE)

# make our data tidy and pivot to long
names(hot_counties) <- c("ARS", years)
hot_counties$ARS <- counties_vect$ARS

hot_counties_long <- hot_counties %>%
  pivot_longer(!ARS, names_to = "Year", values_to = "n_days") %>% 
  mutate(n_days = replace_na(n_days, 0))

# classify the data - if you change the data to another country
# this step definitely needs some fine-tuning
hot_classes <- classIntervals(hot_counties_long$n_days, 
                              n=8, 
                              style="fixed",
                              fixedBreaks=c(-1, 1, 5, 10, 15, 20, 25, 30, 100))
hot_counties_long$classes <- cut(hot_counties_long$n_days, hot_classes$brks)

# and finally,
counties_long <- left_join(hot_counties_long, counties, by = c("ARS"))

legend_labels <- c("0 - 1", "2-5", "6-10", "11-15", "16-20", "21-25", "26-30", ">31")

counties_long %>%  
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = classes),
          size = 0.01) +
  scale_fill_viridis_d(labels= legend_labels, 
                       option = "F",
                       direction = -1, 
                       begin = .17, 
                       end = .97,
                       na.value = "#F9DFCCFF") +
  facet_wrap(Year ~ .,
             ncol = 11) +
  labs(title = "Germany's heating-up",
       #subtitle = "The hottest counties in Germany since 1951. We calculated from a raster source all",
       fill = "Number of days above 30°C",
       caption = "Data: BKK & DWD | Visualisation: Marco Sciaini") + 
  guides(fill = guide_legend(keyheight = unit(1, units = "mm"),  
                             keywidth = unit(3, units = "mm"),
                             direction = "horizontal",
                             nrow = 1,
                             ticks.colour = "white",
                             label.position = "bottom",
                             title.position = "bottom",
                             title.hjust = 0.5)) +
  theme_void() +
  theme(legend.direction="horizontal",
        legend.position="bottom", 
        legend.box = "vertical",
        text=element_text(family="West"),
        panel.spacing.y = unit(1, "lines"),
        plot.title = element_text(size = 26, 
                                  face = "bold", 
                                  colour = '#181c20',
                                  margin = margin(.3,0,.5,0, "cm")),
        strip.background =element_rect(fill= alpha('#cd5c5c', 0.05), colour = NA),
        strip.text = element_text(size = 7, 
                                  face = "bold", 
                                  colour = '#181c20',
                                  margin = margin(.1,0,.1,0, "cm")),
        plot.caption = element_text(size = 7,
                                    colour = '#181c20',
                                    margin = margin(.3,0,.3,0, "cm")))


ggsave("germany_heating_up.png", dpi = 300, width = 2000, height = 2400, units = "px")
