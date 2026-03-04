library(sf)
library(dplyr)
library(lwgeom)
#library(tmap)

generate_unalaska_service_area <- function() {
  sf_use_s2(FALSE)
  #tmap_mode("view")
  
  islands <- st_read("./unalaska-islands.geojson") %>%
    st_transform(4326)
  
  unalaska_island  <- islands %>% filter(name == "Unalaska Island") %>% st_make_valid()
  amaknak_island  <- islands %>% filter(name == "Amaknak Island") %>% st_make_valid()
  
  # That part of Amaknak Island South of 53 ° 55' North Latitude.
  # 55/60 = 0.9167
  
  bb <- st_bbox(amaknak_island)
  amaknak_south_part <- st_crop(
    amaknak_island,
    xmin = bb[["xmin"]], 
    xmax = bb[["xmax"]],
    ymin = bb[["ymin"]],
    ymax = 53 + 55/60
  )
  
  # That portion of Unalaska Island North of 53°50' North Latitude, West
  # of 166 °29' West Longitude, and East of 166 °34' West Longitude
  
  bb <- st_bbox(unalaska_island)
  unalaska_part <- st_crop(
    unalaska_island,
    xmin = -(166 + 34/60), 
    ymin = 53 + 50/60,
    xmax = -(166 + 29/60), 
    ymax = bb[["ymax"]]
  )
  
  
  #tm_shape(unalaska_part) +
  #  tm_polygons()
  
  
  # That portion of Unalaska Island bordering Captains Bay from 53 °52'
  # North Latitude to 53 ° 51' North Latitude for a distance of 500 yards
  # inland from the mean low water line
  
  unalaska_coastal_part <- st_crop(
    unalaska_island,
    xmin = -(166 + 34/60), 
    ymin = 53 + 51/60,
    xmax = bb[["xmax"]], 
    ymax = 53 + 52/60
  ) %>%
    st_cast("POLYGON")
  
  unalaska_coastal_part <- st_crop(
    unalaska_island,
    xmin = bb[["xmin"]], 
    ymin = 53 + 51/60,
    xmax = bb[["xmax"]], 
    ymax = 53 + 52/60
  ) %>%
    st_cast("POLYGON")
  
  bb <- st_bbox(unalaska_part)
  
  captains_bay_coastline_buffered <-  st_read("./pacific-ocean.geojson") %>%  
    st_transform(4326) %>%
    st_crop(xmin =  -(166 + 24/60), 
            ymin = 53 + 51 / 60,
            xmax =  -(166 + 36/60), 
            ymax = 53 + 52 / 60) %>%
    st_transform(26940) %>%
    st_buffer(dist = 50) %>%
    st_buffer(dist = 457.2) # 500 yards = 457.2 meters
  
  unalaska_coastal_part_east <- st_transform(unalaska_coastal_part[2,], 26940)
  
  coastal_part <- st_intersection(captains_bay_coastline_buffered, unalaska_coastal_part_east)
  
  
  unalaska_service_area <- st_union(amaknak_south_part, unalaska_part) %>%
    st_union(st_transform(coastal_part, 4326))
  
  return (unalaska_service_area)
}
