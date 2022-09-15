
# setup -------------------------------------------------------------------

rm(list = ls())

pacman::p_load(tidyverse,
               foreach,
               sf,
               stars,
               raster,
               whitebox,
               doParallel, 
               doSNOW)

wbt_init()

source("code/gis_crs_fmt.R")
source(here::here("code/set_functions.R"))


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


# snap to the nearest streamline -------------------------------------------

#albers_sf_channel <- st_read(dsn = "data_raw/gis/epsg4326_channel_1sqkm.gpkg") %>% 
#  st_transform(crs = wkt_jgd_albers)
#
#albers_sf_site <- read_csv("data_raw/gis/site-coordinate_hogosuimen_terui-org-2019.csv") %>% 
#  drop_na(longitude) %>% 
#  st_as_sf(coords = c("longitude", "latitude")) %>% 
#  st_set_crs(4326) %>% 
#  st_transform(crs = st_crs(albers_sf_channel))
#
#albers_sf_site_snap <- st_snap_points(albers_sf_site, albers_sf_channel)
#df_site_snap_coord <- st_coordinates(albers_sf_site_snap) %>% 
#  as_tibble() %>% 
#  bind_cols(albers_sf_site) %>% 
#  dplyr::select(-geometry)
#
#wgs84_sf_site_snap_coord <- st_as_sf(df_site_snap_coord,
#                                     coords = c("X", "Y")) %>% 
#  st_set_crs(st_crs(albers_sf_channel)) %>% 
#  st_transform(crs = 4326)
#
#st_write(wgs84_sf_site_snap_coord,
#         dsn = "data_raw/gis/epsg4326_point_snap_prtwsd.gpkg",
#         append = FALSE)


# watershed delineation with whitebox -------------------------------------

# whitebox functions cannot take path including whitespace
# gis files for whitebox is saved in temporary directory
# NOTE: "epsg4326_point_snap_prtwsd_edit.gpkg" was manually edited to adjust around confluences
# source file "epsg4326_point_snap_prtwsd.gpkg"

## flow direction
wgs84_dir_arc <- raster::raster("data_raw/gis/epsg4326_dir.tif")
wgs84_dir_d8 <- arc2d8(wgs84_dir_arc)

temp_d8 <- paste(tempdir(), "epsg4326_dir_d8.tif", sep = "\\")

raster::writeRaster(wgs84_dir_d8,
                    temp_d8,
                    overwrite = TRUE)

## pour point
wgs84_outlet <- sf::st_read("data_raw/gis/epsg4326_point_snap_prtwsd_edit.gpkg")

## parallel setup
cl <- makeCluster(detectCores())
registerDoSNOW(cl)

pb <- txtProgressBar(max = nrow(wgs84_outlet), style = 3)
fun_progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = fun_progress)

## run wbt_watershed 
wgs84_watershed <- NULL
wgs84_watershed <- foreach(i = seq_len(nrow(wgs84_outlet)),
                           .combine = bind_rows,
                           .packages = c("sf",
                                         "raster",
                                         "dplyr",
                                         "whitebox",
                                         "stars")
                           ) %dopar% {
                             
  st_write(wgs84_outlet[i,],
           dsn = tempdir(),
           "outlet",
           driver = "ESRI Shapefile",
           append = FALSE,
           quiet = TRUE)
  
  ### watershed raster
  wsd <- paste(tempdir(), "epsg4326_watershed.tif", sep = "\\")
  
  wbt_watershed(d8_pntr = temp_d8,
                pour_pts = paste(tempdir(),
                                 "outlet.shp",
                                 sep = "\\"), 
                output = wsd)
  
  ### raster to polygon
  wgs84_wsd_shape <- NULL
  wgs84_wsd_shape <- raster::raster(wsd) %>% 
    st_as_stars() %>% 
    st_as_sf(merge = TRUE,
             as_points = FALSE) %>% 
    mutate(area = st_area(.)) %>% 
    slice(which.max(area))
  
  return(wgs84_wsd_shape)
}

stopCluster(cl)

mapview::mapview(wgs84_watershed) + mapview::mapview(wgs84_outlet)


# export ------------------------------------------------------------------

## attach site information
wgs84_watershed <- wgs84_watershed %>% 
  mutate(river = wgs84_outlet$river,
         site = wgs84_outlet$site)

st_write(wgs84_watershed,
         dsn = "data_raw/gis/epsg4326_upstr_watershed.gpkg",
         append = FALSE)
