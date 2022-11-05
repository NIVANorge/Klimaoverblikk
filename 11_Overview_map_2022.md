---
title: "Overview map"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_depth: 3
    toc_float: true
    code_folding: hide
    df_print: paged   
---

Makes overview map, using the map data in NIVA's "K:/Kart"  
The "second try" map is the one used   
  
This code uses the new _sf_ package (which supersedes _sp_). It works nicely together with ggplot2 (see below), but for the time being (Dec. 2018) you need the _development version_ of ggplot2. See code below for details.      
  
## Packages

```r
# Install
# install.packages("sf")

# Also needs the *development version* of ggplot2
# First, remove folders digest and ggplot2 in your Library folder (I needed to do that, at least)
# (Eh, I don't know where I store my packages? Run: .libPaths() )
# Then:
#   install.packages("digest")
#   devtools::install_github("tidyverse/ggplot2")


library(sf)
library(ggplot2)
library(dplyr)
library(ggrepel)

library("rnaturalearth")
library("rnaturalearthdata")
#install.packages("rgeos")
library ("rgeos")
library("ggspatial")

save_plot <- FALSE
```

## Data 

### Load basemap

```r
world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)
```

### Stations   


```r
df_stations <- read.csv(textConnection("Station_type, Name, Lat, Long 
Hard bottom,HR104, 58.2732, 08.5372
Hard bottom,HT113, 58.5132, 08.9445
Soft bottom,BR1, 58.3253, 08.6295
Soft bottom,BT44, 58.4038, 09.0312
Hydrography/plankton, Arendal 2, 58.3870, 8.8330
River,Glomma, 59.278, 11.134
River,Drammenselva, 59.75399, 10.00903
River,Numedalslågen, 59.08627, 10.06962
River,Skienselva, 59.199, 9.611
"), stringsAsFactors = FALSE)

df_stations$Station_type <- factor(
  df_stations$Station_type,
  levels = c("River", "Hydrography/plankton", "Hard bottom", "Soft bottom"))

sf_stations <- st_as_sf(df_stations, coords = c("Long", "Lat"), crs = "+proj=longlat")
sf_stations[1,]
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Station_type"],"name":[1],"type":["fct"],"align":["left"]},{"label":["Name"],"name":[2],"type":["chr"],"align":["left"]},{"label":["geometry"],"name":[3],"type":["sf_POINT"],"align":["right"]}],"data":[{"1":"Hard bottom","2":"HR104","3":"<sf_POINT>","_rn_":"1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

### Rivers    


#### Read data  

- takes a minute or something to read
- downloaded from NVEs download site  


```r
file_rivers <- "Map_data/11_sf_rivers.rds"

if (!file.exists(file_rivers)){
  
  # Using st_read in package sf
  fn <- "C:/Data/Mapdata/NVE_ELVIS_elvenett/NVEData/Elv_Hovedelv.geojson"
  sf_rivers_all <- sf::st_read(fn)
  
  # str(sf_rivers_all, 1)  
  
  #### Select rivers  
  
  sel <- grepl("Glomma", sf_rivers_all$elvenavn); sum(sel)
  
  rivernames <- c("Glommavassdraget", "Drammensvassdraget", "Skiensvassdraget", "Numedalslågen")
  
  sf_rivers <- sf_rivers_all %>%
    filter(elvenavn %in% rivernames)
  
  saveRDS(sf_rivers, file_rivers)
  
} else {
  
  sf_rivers <- readRDS(file_rivers)
  
}

nrow(sf_rivers)
```

```
## [1] 4
```

#### Test plot  

```r
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = sf_rivers, color = "blue", show.legend = "point") + 
    coord_sf(xlim = c(5, 13), ylim = c(57, 60))
```

![](11_Overview_map_2022_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Final plot  

### Make and save plot  

```r
# Select different stations for positioning labels  
leftside <- sf_stations$Name %in% c("HR104")
br1 <- sf_stations$Name %in% c("BR1")
arendal <- grepl("Arendal", sf_stations$Name)
rightside <- !leftside & !arendal & !br1

# For label with line (Arendal)
xrel <- -0.35
yrel <- 0.13
away_from_point <- 0.2   # a number 0-1, not on long or latitude scale 
away_from_text <- 0.05    # longitude scale
arendal_line <- st_coordinates(sf_stations[arendal,])
arendal_line <- rbind(arendal_line + c(X = xrel*away_from_point, Y = yrel*away_from_point), 
                      arendal_line + c(X = xrel, Y = yrel)) 
# arendal_line 

# sf_stations

p <- ggplot(data = world) +
  geom_sf(fill = "khaki") +
  geom_sf(data = sf_rivers, color = "blue") + 
  geom_sf(data = sf_stations, aes(shape = Station_type), size = 3) + 
  geom_sf_text(data = sf_stations[rightside,], aes(label = Name), 
               hjust = 0, nudge_x = 0.1) +
  geom_sf_text(data = sf_stations[leftside,], aes(label = Name), 
               hjust = 1, nudge_x = -0.1) +
  geom_sf_text(data = sf_stations[br1,], aes(label = Name), 
               hjust = 1, nudge_x = -0.1, nudge_y = 0.05) +
  geom_sf_text(data = sf_stations[arendal,], aes(label = Name), hjust = 1, 
               nudge_x = xrel - away_from_text, nudge_y = yrel) +
  geom_segment(aes(x = arendal_line[1,1], y = arendal_line[1,2],
                   xend = arendal_line[2,1], yend = arendal_line[2,2])) +
  annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true", 
        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
        style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(5, 13), ylim = c(57, 60)) +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "paleturquoise"),
        axis.title = element_blank())

p2 <- p + 
  scale_shape_manual("Station type", values = c(17, 16, 1, 2)) +
  guides(
    shape = guide_legend(
    override.aes = list(size = 3) ) 
    ) 
#  scale_color_discrete("Station Type")

# p2

ggsave("Figures_publ/11_overview_map.png", p2, width = 9, height = 11)
```

### Show plot  

```r
p2
```

![](11_Overview_map_2022_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

