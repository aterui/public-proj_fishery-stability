
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse,
               sf,
               maptools)
setwd(here::here("code_empirical"))


# define function ---------------------------------------------------------

st_snap_points = function(x, y, max_dist = 1000) {
  
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  return(out)
}


# gis ---------------------------------------------------------------------

df_channel <- st_read("data_gis/albers_channel_hkd.gpkg")

df_site <- read_csv("data_gis/site-coordinate_hogosuimen_terui-org-2019.csv") %>% 
  drop_na(longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = st_crs(df_channel))

df_site_snap <- st_snap_points(df_site, df_channel)

st_write(df_site_snap, "data_gis/albers_point_snap_prwsd.gpkg")

# mapping -----------------------------------------------------------------

mapview::mapView(df_site) +
  mapview::mapView(df_site_snap,
                   color = "red") +
  mapview::mapView(df_channel)
