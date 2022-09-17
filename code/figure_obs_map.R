
# setup -------------------------------------------------------------------

source(here::here("code/library.R"))

# read data ---------------------------------------------------------------

source(here::here("code/set_crs.R"))
source(here::here("code/data_fmt_fishdata.R"))

## hokkaido shape
albers_sf_hkd <- st_read(here::here("data_raw/gis/albers_hkd_shape.gpkg"),
                         quiet = TRUE) %>% 
  st_set_crs(wkt_jgd_albers)

## stream network
albers_sf_channel <- readRDS(here::here("data_raw/gis/epsg4326_channel.rds")) %>% 
  st_transform(wkt_jgd_albers)

## watershed
albers_sf_wsd <- readRDS(here::here("data_raw/gis/albers_wsd_outlet.rds")) %>% 
  st_set_crs(wkt_jgd_albers)


# example watershed -------------------------------------------------------

albers_sf_channel_join <- st_join(albers_sf_channel,
                                  albers_sf_wsd) %>% 
  drop_na(river)

## example watershed (okushibetsu)
albers_sf_channel_oku <- filter(albers_sf_channel_join,
                                river == "okushibetsu") #example

albers_sf_wsd_oku <- filter(albers_sf_wsd,
                            river == "okushibetsu") #example

## site data
site_selected <- distinct(df_fish, site_id) %>% 
  pull()

albers_sf_point <- readRDS(here::here("data_raw/gis/epsg4326_point_snap.rds")) %>% 
  st_transform(wkt_jgd_albers) %>% 
  mutate(site_id = paste0(river, site)) %>% 
  filter(site_id %in% site_selected)


# figure: map -------------------------------------------------------------

g_example <- ggplot() +
  geom_sf(data = albers_sf_wsd_oku,
          fill = grey(0.97)) +
  geom_sf(data = albers_sf_channel_oku,
          color = grey(0.4)) +
  geom_sf(data = filter(albers_sf_point,
                        river == "okushibetsu"),
          size = 1) +
  annotation_scale() +
  theme_void()

g_hkd <- ggplot() +
  geom_sf(data = albers_sf_hkd,
          color = NA,
          fill = grey(0.75)) +
  geom_sf(data = albers_sf_point,
          size = 0.5) +
  theme_bw()

g_masu <- ggdraw() +
  draw_image(here::here("image/masu_salmon.jpg")) +
  theme_void()

