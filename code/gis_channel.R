
# setup -------------------------------------------------------------------

rm(list = ls())

source(here::here("code/library.R"))
source(here::here("code/set_crs.R"))
source(here::here("code/set_functions.R"))

wbt_init()


# channel delineation -----------------------------------------------------

## specify temporary file location to avoid white space in file path
upa_name <- paste(tempdir(), "upa.tif", sep = "\\")
stream_name <- paste(tempdir(), "stream.tif", sep = "\\")
point_name <- paste(tempdir(), "point.shp", sep = "\\")
point_snap_name <- paste(tempdir(), "point_snap.shp", sep = "\\")

## write flow accumulation layer to tempdir()
sr_upa <- terra::rast(here::here("data_raw/gis/epsg4326_upa.tif"))
terra::writeRaster(sr_upa,
                   filename = upa_name,
                   overwrite = TRUE)

## write point data to tempdir()
read_csv("data_raw/gis/site-coordinate_hogosuimen_terui-org-2019.csv") %>% 
  drop_na(longitude) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_write(dsn = point_name,
           append = FALSE)

## extract stream cells
wbt_extract_streams(flow_accum = upa_name,
                    output = stream_name,
                    threshold = 1)

wbt_jenson_snap_pour_points(pour_pts = point_name,
                            streams = stream_name,
                            output = point_snap_name,
                            snap_dist = 2)


# watershed delineation ---------------------------------------------------

dir_name <- paste(tempdir(), "dir.tif", sep = "\\")
wsd_name <- paste(tempdir(), "wsd.tif", sep = "\\")

terra::rast(here::here("data_raw/gis/epsg4326_dir_d8.tif")) %>% 
  terra::writeRaster(filename = dir_name,
                     overwrite = TRUE)

wbt_unnest_basins(d8_pntr = dir_name,
                  pour_pts = point_snap_name,
                  output = wsd_name)

# check -------------------------------------------------------------------

# sf_point <- st_read(point_name) %>% 
#   st_set_crs(4326)
# 
# sf_point_snap <- st_read(point_snap_name) %>% 
#   st_set_crs(4326)
# 
# r_str <- terra::rast(stream_name)
# 
# terra::writeRaster(r_str,
#                    filename = here::here("data_raw/gis/epsg4326_stream.tif"),
#                    overwrite = TRUE)
# 
# library(tmap)
# tmap_mode("view")
# 
# tm_shape(sf_point) +
#   tm_dots() +
#   tm_shape(sf_point_snap) +
#   tm_dots(col = "red")
