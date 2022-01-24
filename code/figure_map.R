
# setup -------------------------------------------------------------------

pacman::p_load(tidyverse,
               patchwork,
               cowplot,
               magick,
               ggspatial,
               sf)


# read data ---------------------------------------------------------------

source("code/gis_crs_fmt.R")
source("code/data_fmt_fishdata.R")

## hokkaido shape
albers_sf_hkd <- st_read("data_gis/albers_hkd_shape.gpkg") %>% 
  st_set_crs(wkt_jgd_albers)

## stream network
albers_sf_channel <- st_read("data_gis/albers_channel_hkd.gpkg") %>% 
  st_set_crs(wkt_jgd_albers)

albers_sf_channel_example <- filter(albers_sf_channel, wsd_id == 488) #example

## watershed
albers_sf_wsd <- st_read("data_gis/albers_watershed_hkd_final.gpkg") %>% 
  st_set_crs(wkt_jgd_albers)

albers_sf_wsd_example <- filter(albers_sf_wsd, wsd_id == 488) #example

## site data
site_selected <- distinct(df_fish, site_id) %>% 
  pull()

albers_sf_point <- st_read("data_gis/epsg4326_point_snap_prtwsd_edit.gpkg") %>% 
  st_transform(wkt_jgd_albers) %>% 
  mutate(site_id = paste0(river, site)) %>% 
  filter(site_id %in% site_selected)


# figure: map -------------------------------------------------------------
  
g_example <- ggplot() +
  geom_sf(data = albers_sf_wsd_example,
          fill = grey(0.97)) +
  geom_sf(data = albers_sf_channel_example,
          color = grey(0.4)) +
  geom_sf(data = filter(albers_sf_point,
                        river == "okushibetsu"),
          size = 1) +
  annotation_scale() +
  theme_void()# +
  #theme(plot.background = element_rect(fill = grey(1, 0.5)))

g_hkd <- ggplot() +
  geom_sf(data = albers_sf_hkd,
          color = NA,
          fill = grey(0.75)) +
  geom_sf(data = albers_sf_point,
          size = 0.5) +
  theme_bw()# +
  #inset_element(g_example,
  #              ignore_tag = TRUE,
  #              0.6, 0.05, 0.99, 0.3)

g_masu <- ggdraw() +
  draw_image("image/masu_salmon.jpg") +
  theme_void()

